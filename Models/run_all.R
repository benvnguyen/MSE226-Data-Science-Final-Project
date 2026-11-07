# =============================================================================
# run_all.R: ONE-CLICK Section 6 pipeline without the GUI.
# In RStudio: set the working directory to the project folder, open this file,
# and click "Source". Settings come from Models/config.R.
#
# Runs 10-fold CV for the baseline, OLS, random forest and neural network on both
# feature sets, prints the results table, refits the best model, and saves
# everything to Models/results/ and Models/final_model/.
# =============================================================================

if (!file.exists(file.path("Models", "config.R")))
  stop("Set the working directory to the project folder first (Session > Set Working Directory).")
source(file.path("Models", "config.R"))
source(file.path("Models", "R", "pipeline.R"))

cfg <- DEFAULT_CONFIG
# Quick overrides, e.g.:
# cfg$rf$trees <- 1000
# cfg$nn$mode <- "manual"; cfg$nn$lr <- 0.003
results <- run_pipeline(cfg)
