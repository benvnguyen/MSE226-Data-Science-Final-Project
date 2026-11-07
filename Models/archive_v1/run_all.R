# =============================================================================
# run_all.R: runs Section 6 end to end. From the project root:
#   source("Models/run_all.R")
#
# 01  folds + baseline         -> Models/dev_folds.csv          (seconds)
# 02  fit all 6 models         -> Models/results/               (first run: tens of minutes;
#                                                                 cached after that)
# 03  select + save final model -> Models/final_pipeline/       (about a minute)
#
# Settings (seed, caching, parallel) live at the top of Models/00_setup.R.
# =============================================================================

source(file.path("Models", "01_folds_baseline.R"))
source(file.path("Models", "02_fit_models.R"))
source(file.path("Models", "03_final_model.R"))
