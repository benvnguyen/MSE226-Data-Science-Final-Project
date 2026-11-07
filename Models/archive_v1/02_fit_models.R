# =============================================================================
# 02_fit_models.R: Section 6(d)-(e)
# Trains and tunes every algorithm x feature set on the SAME 10 household-grouped
# folds, then builds the comparison table (with the baseline) and tuning table.
# DEVELOPMENT DATA ONLY.
#
# Run from the project root:  source("Models/02_fit_models.R")
# Re-runs reuse cached fits in Models/results/ (set REFIT <- TRUE in 00_setup.R to refit).
# Outputs (Models/results/):
#   comparison_table.csv   one row per combination + baseline (the 6(e) table)
#   tuning_all_configs.csv every tuning setting tried, with CV metrics
#   cv_comparison.png      MAE +/- SE by algorithm and feature set
#   tune_<algo>_<set>.rds  cached tuning results
# =============================================================================

source(file.path("Models", "00_setup.R"))

dev   <- load_dev()
folds <- load_folds(dev)

# ---- fit all 6 combinations ------------------------------------------------------
results <- list()
for (fs in names(FEATURE_SETS)) {
  vars <- FEATURE_SETS[[fs]]
  for (algo in names(ALGORITHMS)) {
    key <- paste(algo, fs, sep = "_")
    message("Fitting ", ALGORITHMS[[algo]], " / ", fs, " feature set")
    results[[key]] <- cached(key, function() {
      wf <- workflow(make_recipe(vars, RECIPE_TYPE[[algo]], dev), make_spec(algo))
      grid <- make_grid(algo, vars)
      set.seed(SEED)
      res <- if (is.null(grid)) {
        fit_resamples(wf, folds, metrics = METRICS, control = control_resamples())
      } else {
        tune_grid(wf, folds, grid = grid, metrics = METRICS, control = control_grid())
      }
      list(res = res, algo = algo, fs = fs, n_configs = if (is.null(grid)) 1L else nrow(grid))
    })
  }
}

# ---- tables ----------------------------------------------------------------------
to_dollars <- function(m) ifelse(m %in% c("mae", "rmse"), Y_SCALE, 1)
param_cols <- c("mtry", "min_n", "hidden_units", "penalty")

all_configs <- bind_rows(lapply(results, function(r) {
  collect_metrics(r$res) |>
    mutate(algorithm = ALGORITHMS[[r$algo]], feature_set = r$fs,
           mean = mean * to_dollars(.metric), std_err = std_err * to_dollars(.metric))
}))
write.csv(all_configs |> select(algorithm, feature_set, any_of(param_cols), .metric, mean, std_err, n),
          file.path(RES_DIR, "tuning_all_configs.csv"), row.names = FALSE)

best_row <- function(r) {
  m <- collect_metrics(r$res)
  best_cfg <- m |> filter(.metric == "mae") |> slice_min(mean, n = 1, with_ties = FALSE)
  m <- m |> filter(.config == best_cfg$.config)
  get <- function(metric, col) m[[col]][m$.metric == metric] * to_dollars(metric)
  sel <- best_cfg |> select(any_of(param_cols))
  data.frame(
    Algorithm = ALGORITHMS[[r$algo]], `Feature set` = r$fs, check.names = FALSE,
    `Settings tried` = paste0(r$n_configs, " (", GRID_TEXT[[r$algo]], ")"),
    `Selected` = if (ncol(sel)) paste(names(sel), signif(unlist(sel), 3), sep = "=", collapse = ", ") else "-",
    MAE = get("mae", "mean"), MAE_SE = get("mae", "std_err"),
    RMSE = get("rmse", "mean"), RMSE_SE = get("rmse", "std_err"),
    R2 = get("rsq_trad", "mean"), Minutes = round(r$minutes, 1))
}
base <- baseline_cv(folds)
bget <- function(metric, col) base[[col]][base$.metric == metric] * to_dollars(metric)
baseline_row <- data.frame(
  Algorithm = "Baseline (training-fold median)", `Feature set` = "-", check.names = FALSE,
  `Settings tried` = "-", Selected = "-",
  MAE = bget("mae", "mean"), MAE_SE = bget("mae", "std_err"),
  RMSE = bget("rmse", "mean"), RMSE_SE = bget("rmse", "std_err"),
  R2 = bget("rsq_trad", "mean"), Minutes = 0)

comparison <- bind_rows(baseline_row, bind_rows(lapply(results, best_row))) |>
  mutate(`MAE gain vs baseline` = baseline_row$MAE - MAE)
write.csv(comparison, file.path(RES_DIR, "comparison_table.csv"), row.names = FALSE)

cat("\n==== 6(e) Comparison table: 10-fold CV, mean (SE); lower MAE is better ====\n")
show <- comparison |>
  transmute(Algorithm, `Feature set`, Selected,
            MAE = sprintf("$%s (%s)", format(round(MAE), big.mark = ","), format(round(MAE_SE), big.mark = ",")),
            RMSE = sprintf("$%s (%s)", format(round(RMSE), big.mark = ","), format(round(RMSE_SE), big.mark = ",")),
            R2 = sprintf("%.3f", R2),
            `Gain vs baseline` = sprintf("$%s", format(round(`MAE gain vs baseline`), big.mark = ",")))
print(show, row.names = FALSE, right = FALSE)

# ---- figure: MAE by combination --------------------------------------------------
library(ggplot2)
pd <- comparison[-1, ]
pd$Algorithm <- factor(pd$Algorithm, levels = unname(ALGORITHMS))
p <- ggplot(pd, aes(Algorithm, MAE, colour = `Feature set`)) +
  scale_x_discrete() +      # declare first: the numeric annotate() x below would otherwise force a continuous axis
  geom_hline(yintercept = baseline_row$MAE, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 0.6, y = baseline_row$MAE, label = "Baseline (median)", vjust = -0.5,
           hjust = 0, size = 3.5, colour = "grey30") +
  geom_pointrange(aes(ymin = MAE - MAE_SE, ymax = MAE + MAE_SE),
                  position = position_dodge(width = 0.4), size = 0.6) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_colour_manual(values = c(small = "#0072B2", large = "#D55E00")) +
  labs(title = "Cross-validated MAE by algorithm and feature set",
       subtitle = "10 household-grouped folds on dev data; points = mean, bars = +/- 1 SE",
       x = NULL, y = "MAE (2024 $)") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(file.path(RES_DIR, "cv_comparison.png"), p, width = 8, height = 5, dpi = 150, bg = "white")
if (interactive()) print(p)
message("Saved tables and figure to ", normalizePath(RES_DIR))
