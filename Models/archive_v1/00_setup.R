# =============================================================================
# 00_setup.R: shared setup for every modeling script (sourced by 01-03)
# DEVELOPMENT DATA ONLY. Nothing here reads holdout_raw.rds or the raw CSV.
#
# Defines: packages, settings, prep_data(), feature sets, recipes, model specs,
# tuning grids, the fixed CV folds, metrics, the baseline, and caching helpers.
# =============================================================================

suppressPackageStartupMessages({
  library(rsample); library(recipes); library(parsnip); library(workflows)
  library(tune); library(yardstick); library(dials); library(dplyr)
})
tidymodels_prefer <- function() invisible(NULL)   # (not needed; listed for familiarity)

# ---- settings (edit here) ----------------------------------------------------
SEED      <- 32                        # same seed as the holdout split
Y_SCALE   <- 1e4                       # models are trained on earnings / $10,000; metrics reported in $
# REFIT and PARALLEL can also be set before sourcing, e.g. REFIT <- TRUE; source("Models/run_all.R")
if (!exists("REFIT"))    REFIT    <- FALSE   # TRUE = refit everything; FALSE = reuse cached results in Models/results/
if (!exists("PARALLEL")) PARALLEL <- TRUE    # run CV folds in parallel across CPU cores
DEV_PATH  <- file.path("data", "dev_raw.rds")
FOLD_PATH <- file.path("Models", "dev_folds.csv")
RES_DIR   <- file.path("Models", "results")
FINAL_DIR <- file.path("Models", "final_pipeline")
dir.create(RES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FINAL_DIR, showWarnings = FALSE, recursive = TRUE)

N_CORES <- max(1, parallel::detectCores() - 1)
if (PARALLEL && N_CORES > 1) {
  future::plan(future::multisession, workers = N_CORES)
  RF_THREADS <- 1                      # folds already run in parallel; don't oversubscribe
} else {
  future::plan(future::sequential)
  RF_THREADS <- N_CORES
}

# ---- prep_data(): fixed, rule-based recoding (no statistics learned) -------------
# Works on dev now and on the holdout in Part 2, with identical output columns.
SCHL_CODES  <- sprintf("%02d", 1:24)
STATE_CODES <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19",
                 "20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35",
                 "36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53",
                 "54","55","56")
prep_data <- function(raw) {
  stopifnot(all(raw$MIL == "2"), all(raw$VPSP == "01"))
  out <- data.frame(
    SERIALNO = raw$SERIALNO,
    SPORDER  = as.character(raw$SPORDER),
    AGEP     = as.numeric(raw$AGEP),
    SEX      = factor(raw$SEX, levels = c("1", "2"), labels = c("Male", "Female")),
    RAC1P    = factor(raw$RAC1P, levels = as.character(1:9)),
    MAR      = factor(raw$MAR, levels = as.character(1:5),
                      labels = c("Married", "Widowed", "Divorced", "Separated", "Never married")),
    DRAT     = factor(ifelse(is.na(raw$DRAT), "none", raw$DRAT),
                      levels = c("none", "1", "2", "3", "4", "5", "6"),
                      labels = c("No rating", "0%", "10-20%", "30-40%", "50-60%", "70-100%", "Not reported")),
    SCHL     = factor(raw$SCHL, levels = SCHL_CODES),
    STATE    = factor(raw$STATE, levels = STATE_CODES),
    FOD1P    = factor(ifelse(is.na(raw$FOD1P), "NoBA", raw$FOD1P)),   # N/A = no bachelor's degree
    WKHP     = as.numeric(raw$WKHP),       # already 0-filled: 0 = did not work
    WKWN     = as.numeric(raw$WKWN),
    JWMNP    = as.numeric(raw$JWMNP),      # 0 = no job OR works from home
    stringsAsFactors = FALSE)
  if ("PERNP_adj" %in% names(raw)) out$y <- raw$PERNP_adj / Y_SCALE
  out
}

load_dev <- function() {
  raw <- readRDS(DEV_PATH)
  stopifnot(nrow(raw) == 23233)        # guard: this must be the dev file
  prep_data(raw)
}

# ---- feature sets ------------------------------------------------------------
FEATURE_SETS <- list(
  small = c("AGEP", "SEX", "RAC1P", "MAR", "DRAT", "SCHL", "STATE", "FOD1P"),  # records-based
  large = c("AGEP", "SEX", "RAC1P", "MAR", "DRAT", "SCHL", "STATE", "FOD1P",
            "WKHP", "WKWN", "JWMNP"))                                          # + past-year work

# ---- recipes (everything data-dependent here is refit inside each CV fold) -----------
make_recipe <- function(vars, type = c("tree", "linear", "nn"), data) {
  type <- match.arg(type)
  rec <- recipe(reformulate(vars, response = "y"), data = data) |>
    step_novel(all_nominal_predictors()) |>                           # unseen levels -> "new"
    step_other(all_nominal_predictors(), threshold = 0.01, other = "other")  # rare (<1% of training fold) -> "other"
  if (type != "tree") {
    rec <- rec |>
      step_dummy(all_nominal_predictors()) |>
      step_zv(all_predictors()) |>
      step_lincomb(all_numeric_predictors())                           # drop exact linear duplicates among dummies
    # Note: FOD1P "NoBA" = 1 exactly when SCHL < 21, which is collinear with the intercept,
    # so lm drops one dummy and warns "rank-deficient fit". Predictions are unaffected.
  }
  if (type == "nn") rec <- rec |> step_normalize(all_numeric_predictors())
  rec
}

# ---- algorithms, specs, grids ------------------------------------------------
ALGORITHMS <- c(lm = "Linear regression", rf = "Random forest", nn = "Neural network (1 hidden layer)")
RECIPE_TYPE <- c(lm = "linear", rf = "tree", nn = "nn")

make_spec <- function(algo) switch(algo,
  lm = linear_reg() |> set_engine("lm"),
  rf = rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
         set_engine("ranger", num.threads = !!RF_THREADS, seed = !!SEED) |>   # !! = insert the value now
         set_mode("regression"),                                              # (parallel workers can't see SEED)
  nn = mlp(hidden_units = tune(), penalty = tune(), epochs = 500) |>
         set_engine("nnet", MaxNWts = 20000, trace = FALSE) |> set_mode("regression"))

make_grid <- function(algo, vars) switch(algo,
  lm = NULL,
  rf = grid_regular(mtry(range = c(2L, length(vars))), min_n(range = c(5L, 50L)), levels = 4),
  # Small, well-regularized nets: on one test fold, 16-32 hidden units overfit badly
  # (MAE $41-46k vs $30-40k for 8 units), and decay = 1 beat 0.01.
  nn = expand.grid(hidden_units = c(4L, 8L, 12L), penalty = c(0.1, 1, 5)))

GRID_TEXT <- c(lm = "none (no tuning parameters)",
               rf = "4 x 4 grid: mtry in {2..p}, min_n in {5, 20, 35, 50}; 500 trees",
               nn = "3 x 3 grid: hidden units {4, 8, 12} x weight decay {0.1, 1, 5}; 500 iterations")

# ---- fixed CV folds (from 01_folds_baseline.R) -------------------------------
load_folds <- function(dev) {
  fid <- read.csv(FOLD_PATH, colClasses = c("character", "character", "integer"))
  fold <- fid$fold[match(paste(dev$SERIALNO, dev$SPORDER), paste(fid$SERIALNO, fid$SPORDER))]
  stopifnot(!anyNA(fold))
  splits <- lapply(sort(unique(fold)), function(k)
    make_splits(list(analysis = which(fold != k), assessment = which(fold == k)), data = dev))
  manual_rset(splits, ids = sprintf("Fold%02d", sort(unique(fold))))
}

# ---- metrics -------------------------------------------------------------------
# Computed on the scaled outcome; MAE and RMSE are multiplied by Y_SCALE when reported.
METRICS <- metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq_trad)

baseline_cv <- function(folds) {          # training-fold median, scored like every model
  per <- lapply(folds$splits, function(s) {
    tr <- analysis(s)$y; va <- assessment(s)$y; p <- median(tr)
    c(mae = mean(abs(va - p)), rmse = sqrt(mean((va - p)^2)),
      rsq_trad = 1 - sum((va - p)^2) / sum((va - mean(va))^2))
  })
  m <- do.call(rbind, per)
  data.frame(.metric = colnames(m), mean = colMeans(m), std_err = apply(m, 2, sd) / sqrt(nrow(m)))
}

# ---- caching helper ------------------------------------------------------------
cached <- function(name, fit_fun) {
  path <- file.path(RES_DIR, paste0("tune_", name, ".rds"))
  if (!REFIT && file.exists(path)) { message("  loaded cached ", name); return(readRDS(path)) }
  t0 <- Sys.time(); out <- fit_fun()
  out$minutes <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(out, path); message(sprintf("  fit %s in %.1f min", name, out$minutes)); out
}

# ---- prediction helper (used in 03 and in Part 2) --------------------------------
predict_earnings <- function(fit, raw) predict(fit, prep_data(raw))$.pred * Y_SCALE
