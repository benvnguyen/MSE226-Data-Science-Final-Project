# =============================================================================
# rf.R: random forest (ranger).
#
# How one forest is built (repeated `trees` times, then averaged):
#   1. BOOTSTRAP: draw n rows from the training data WITH replacement. About 63%
#      of rows appear at least once; the other ~37% are that tree's
#      "out-of-bag" (OOB) rows, which it never sees.
#   2. At EVERY node, pick `mtry` of the p predictors at random (a new draw each node).
#   3. Among only those `mtry`, choose the variable and cut point that most reduce
#      the sum of squared errors around the two children's means.
#   4. Keep splitting until a node has fewer than `min_n` rows (no depth limit, no pruning).
#   5. Each leaf predicts the mean earnings of its training rows.
# Prediction = the average of all trees' leaf values.
#
# Categories: respect.unordered.factors = "order". Before growing, each factor's
# levels are sorted by mean earnings in the training fold, so a single cut can
# group, e.g., high-earning states together. That's better than the
# arbitrary code order (e.g., STATE FIPS).
# =============================================================================

suppressPackageStartupMessages(library(ranger))   # needed so predict() finds ranger's method

rf_mtry_grid <- function(p) unique(round(seq(2, p, length.out = 4)))

# Median random forest (quantile regression forest, Meinshausen 2006): same trees,
# but each leaf keeps all its training earnings, and the prediction is the MEDIAN of
# the earnings in the leaves a person lands in (across all trees) rather than the
# average of leaf means. MAE is minimized by the median, so this targets our metric.
rf_fit <- function(train, vars, params, seed = 32, threads = 1,
                   importance = "none", keep.inbag = FALSE, quantreg = FALSE) {
  rec <- prep(make_recipe(vars, "tree", train))
  x <- bake(rec, NULL)
  fit <- ranger::ranger(
    y ~ ., data = x,
    num.trees     = params$trees,
    mtry          = min(params$mtry, length(vars)),
    min.node.size = params$min_n,
    respect.unordered.factors = "order",
    importance    = importance, keep.inbag = keep.inbag, quantreg = quantreg,
    num.threads   = threads, seed = seed)
  list(rec = rec, fit = fit, median = quantreg)
}

rf_predict <- function(m, newdata, threads = 1) {
  x <- bake(m$rec, newdata)
  if (isTRUE(m$median))
    predict(m$fit, x, type = "quantiles", quantiles = 0.5, num.threads = threads)$predictions[, 1]
  else predict(m$fit, x, num.threads = threads)$predictions
}

# OOB error as trees are added: shows where adding more trees stops helping.
rf_oob_curve <- function(m, train, threads = 1, step = 10) {
  x <- bake(m$rec, train)
  P <- predict(m$fit, x, predict.all = TRUE, num.threads = threads)$predictions   # n x trees
  M <- do.call(cbind, lapply(m$fit$inbag.counts, function(v) v == 0))            # TRUE = OOB
  P[!M] <- 0
  cs_p <- t(apply(P, 1, cumsum)); cs_n <- t(apply(M, 1, cumsum))
  ks <- unique(c(seq(step, ncol(P), by = step), ncol(P)))
  do.call(rbind, lapply(ks, function(k) {
    ok <- cs_n[, k] > 0
    e <- (cs_p[ok, k] / cs_n[ok, k] - x$y[ok]) * Y_SCALE
    data.frame(trees = k, oob_mae = mean(abs(e)), oob_rmse = sqrt(mean(e^2)))
  }))
}
