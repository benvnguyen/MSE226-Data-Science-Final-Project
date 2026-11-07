# =============================================================================
# ols.R: ordinary least squares.
# Earnings = intercept + sum of (coefficient x predictor). Each category level
# becomes a 0/1 dummy column (the first level is the reference). Coefficients
# minimize the sum of squared errors. There are no hyperparameters.
# =============================================================================

ols_fit <- function(train, vars, params = list(), seed = 32) {
  rec <- prep(make_recipe(vars, "linear", train))
  x <- bake(rec, NULL)
  # A few dummies can be exact linear combinations of others (e.g. FOD1P
  # "No bachelor's" is determined by SCHL). lm drops them (NA coefficient).
  fit <- lm(y ~ ., data = x)
  list(rec = rec, fit = fit)
}

ols_predict <- function(m, newdata) {
  x <- bake(m$rec, newdata)
  suppressWarnings(unname(predict(m$fit, newdata = x)))   # silence "rank-deficient" (expected)
}

ols_describe <- function(m) {
  co <- coef(m$fit)
  data.frame(term = names(co), coef_dollars = unname(co) * Y_SCALE, row.names = NULL) |>
    mutate(dropped = is.na(coef_dollars))
}
