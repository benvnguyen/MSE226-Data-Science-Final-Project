# =============================================================================
# Section 6(b)-(c): fixed CV folds + baseline
# DEVELOPMENT DATA ONLY. This script never reads holdout_raw.rds or the raw CSV.
#
# Run from the project root:  source("Models/make_folds.R")
# Base R only. Only needed once: dev_folds.csv is already in the repo, and the
# pipeline reads it. Re-running reproduces the same file (seed 32).
#
# Output: Models/dev_folds.csv (SERIALNO, SPORDER, fold). This is the ONE fold
# assignment that every model (baseline, OLS, lasso, RF, every feature set)
# must use, so all comparisons happen on identical splits.
# =============================================================================

d <- readRDS("data/dev_raw.rds")
stopifnot(nrow(d) == 23233)            # guard: make sure this is the dev file
y <- d$PERNP_adj

SEED <- 32
K <- 10

# ---- 6(b): 10-fold CV, grouped by household ----------------------------------
# Whole households go to one fold, so two veterans from the same household are
# never split between training and validation (755 dev households have 2+ vets).
set.seed(SEED)
hh <- sort(unique(d$SERIALNO))                         # sort: order doesn't depend on row order
hh_fold <- setNames(sample(rep(seq_len(K), length.out = length(hh))), hh)
d$fold <- unname(hh_fold[d$SERIALNO])

folds_out <- data.frame(SERIALNO = d$SERIALNO, SPORDER = d$SPORDER, fold = d$fold)
write.csv(folds_out, file.path("Models", "dev_folds.csv"), row.names = FALSE)

cat("Fold balance\n")
bal <- do.call(rbind, lapply(seq_len(K), function(k) {
  v <- y[d$fold == k]
  data.frame(fold = k, n = length(v), households = length(unique(d$SERIALNO[d$fold == k])),
             mean = round(mean(v)), median = round(median(v)), pct_zero = round(mean(v == 0), 3),
             max = round(max(v)))
}))
print(bal, row.names = FALSE)
stopifnot(all(tapply(d$fold, d$SERIALNO, function(f) length(unique(f))) == 1))   # no household split

# ---- 6(c): baselines, scored on the same folds --------------------------------
base_rmse <- function(a, p) sqrt(mean((a - p)^2))
base_mae  <- function(a, p) mean(abs(a - p))
base_rsq <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)   # vs. the validation fold's own mean

per_fold <- do.call(rbind, lapply(seq_len(K), function(k) {
  tr <- y[d$fold != k]; va <- y[d$fold == k]
  p_med  <- median(tr)                 # PRIMARY baseline: training-fold median (best constant under MAE)
  p_mean <- mean(tr)                   # secondary: training-fold mean (best constant under RMSE)
  data.frame(fold = k, train_mean = p_mean, train_median = p_med,
             rmse_mean = base_rmse(va, p_mean), mae_mean = base_mae(va, p_mean), r2_mean = base_rsq(va, p_mean),
             rmse_median = base_rmse(va, p_med), mae_median = base_mae(va, p_med))
}))
cat("\nPer-fold baseline results\n")
print(format(per_fold, digits = 6, big.mark = ","), row.names = FALSE)

se <- function(v) sd(v) / sqrt(length(v))
summ <- data.frame(
  baseline = c("Training-fold mean (best constant for RMSE)", "Training-fold median (PRIMARY: best constant for MAE)"),
  RMSE = c(mean(per_fold$rmse_mean), mean(per_fold$rmse_median)),
  RMSE_se = c(se(per_fold$rmse_mean), se(per_fold$rmse_median)),
  MAE = c(mean(per_fold$mae_mean), mean(per_fold$mae_median)),
  MAE_se = c(se(per_fold$mae_mean), se(per_fold$mae_median)))
cat("\nBaseline summary (mean across 10 folds, SE = sd / sqrt(10))\n")
print(format(summ, digits = 6, big.mark = ","), row.names = FALSE)
cat("Mean R^2 of the mean baseline:", round(mean(per_fold$r2_mean), 4), "\n")

# How much of RMSE comes from the top 1%?
cat("\nTail check: share of the baseline's squared error from the top 1% of earners:",
    round({ e2 <- (y - mean(y))^2; sum(e2[y >= quantile(y, .99)]) / sum(e2) }, 3), "\n")
cat("Outcome SD on all dev:", round(sd(y)), "\n")
