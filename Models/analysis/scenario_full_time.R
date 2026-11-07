# =============================================================================
# scenario_full_time.R: Section 7 scenario table (docs/NARRATIVE.md). DEV DATA ONLY.
# "Records only" = small-set median RF (best CV setting) refit on all dev.
# "If full-time" = the saved final model (large-set median RF) with WKHP = 40,
# WKWN = 52, commute = workers' median. Prints medians of predictions by group.
# Descriptive and in-sample: NOT a causal effect of working full-time.
# Run from the project root: source("Models/analysis/scenario_full_time.R")
# =============================================================================
suppressMessages({source(file.path("Models", "config.R")); source(file.path("Models", "R", "pipeline.R"))})
raw <- readRDS(DEV_PATH); dev <- prep_data(raw)
tune <- read.csv(file.path("Models", "results", "tuning_all_settings.csv"))
bs <- tune[tune$model == "qrf" & tune$features == "small", ]; bs <- bs[which.min(bs$MAE), ]
cat("small-set median RF setting:", bs$settings_text, "\n")
p <- DEFAULT_CONFIG$rf
p$mtry  <- as.numeric(sub(".*mtry=([0-9]+).*", "\\1", bs$settings_text))
p$min_n <- as.numeric(sub(".*min_n=([0-9]+).*", "\\1", bs$settings_text))
th <- max(1, parallel::detectCores() - 1)
m_small <- rf_fit(dev, FEATURE_SETS$small, p, seed = 32, threads = th, quantreg = TRUE)
final <- load_final_model(); stopifnot(final$meta$model == "qrf", final$meta$features == "large")
records_only <- rf_predict(m_small, dev, th) * Y_SCALE
ft <- dev; ft$WKHP <- 40; ft$WKWN <- 52; ft$JWMNP <- median(dev$JWMNP[dev$WKHP > 0])
scenario <- rf_predict(final$model, ft, th) * Y_SCALE
is_ft <- dev$WKHP >= 35 & dev$WKWN >= 50
edu <- cut(as.integer(as.character(dev$SCHL)), c(0, 17, 20, 21, 24),
           labels = c("HS or less", "Some college / assoc.", "Bachelor's", "Graduate"))
summ <- function(g, label) {
  d <- data.frame(g = g, ro = records_only, sc = scenario, y = dev$y * Y_SCALE, ft = is_ft)
  out <- do.call(rbind, lapply(split(d, d$g), function(z) data.frame(
    group = z$g[1], n = nrow(z), share_ft = round(mean(z$ft), 3),
    ro = round(median(z$ro)), sc = round(median(z$sc)), gap = round(median(z$sc) - median(z$ro)),
    actual_ft = round(median(z$y[z$ft])), n_ft = sum(z$ft))))
  cat("\n==", label, "==\n"); print(out, row.names = FALSE)
}
summ(dev$DRAT, "VA disability rating"); summ(edu, "Education")
summ(cut(dev$AGEP, c(16, 29, 39, 49, 59, 99), labels = c("17-29", "30-39", "40-49", "50-59", "60+")), "Age")
cat(sprintf("\nIn-sample MAE records-only $%.0f; scenario: share of FT workers whose actual > scenario %.2f\n",
            mean(abs(records_only - dev$y * Y_SCALE)), mean(dev$y[is_ft] * Y_SCALE > scenario[is_ft])))
