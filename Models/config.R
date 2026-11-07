# =============================================================================
# config.R: every setting for the Section 6 pipeline, in one place.
# Edit values here, or override them in the GUI (app.R). DEV DATA ONLY.
#
# Each model has a `mode`:
#   "manual" = run 10-fold CV with exactly the values given below
#   "tune"   = search for the best values (grid for RF, random search for NN)
# `time_budget_min` (optional; NA = no limit) stops a search once that many
# minutes have been spent. The best setting found so far is kept.
# =============================================================================

DEFAULT_CONFIG <- list(
  models       = c("ols", "rf", "qrf", "nn"), # which algorithms to run ("qrf" = median random forest)
  feature_sets = c("small", "large"),         # which feature sets to run
  seed         = 32,                          # same seed as the holdout split
  n_cores      = max(1, parallel::detectCores() - 1),
  refit_final  = TRUE,                        # refit the best model on all dev data

  # ---- Ordinary least squares: nothing to tune ------------------------------
  ols = list(),

  # ---- Random forest (ranger); also used by the median RF ("qrf") -------------
  rf = list(
    trees  = 500,                 # number of trees in each forest
    mode   = "tune",              # "tune" or "manual"
    mtry   = 5,                   # manual: variables tried at each split
    min_n  = 50,                  # manual: a node with fewer rows is not split further
    grid_mtry  = NA,              # tune: NA = 4 values spread from 2 to p (all variables)
    grid_min_n = c(5, 20, 50, 100),
    time_budget_min = NA
  ),

  # ---- Neural network (torch): inputs -> 4 ReLU -> 3 ReLU -> 1 output ---------
  nn = list(
    hidden      = c(4, 3),        # units in each hidden layer
    mode        = "tune",         # "tune" or "manual"
    lr          = 3e-3,           # manual: Adam learning rate
    l2          = 1e-4,           # manual: L2 penalty (Adam weight_decay)
    batch_size  = 256,            # manual: mini-batch size
    loss        = "l1",           # "l1" (absolute error; matches MAE) or "mse"
    max_epochs  = 200,            # upper limit; early stopping usually ends sooner
    patience    = 15,             # stop after this many epochs without validation improvement
    val_frac    = 0.10,           # share of each training fold held out for early stopping
    # tune: random search ranges (learning rate and L2 sampled on a log scale)
    search_n          = 30,       # settings to try per feature set (NA = until time runs out)
    search_lr         = c(3e-4, 3e-2),
    search_l2         = c(1e-6, 1e-2),
    search_batch_size = c(128, 256, 512),
    time_budget_min   = 45        # total for the NN search, split across feature sets
  )
)
