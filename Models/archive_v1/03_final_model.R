# =============================================================================
# 03_final_model.R: Section 6(f)
# Picks the combination with the lowest CV MAE, refits it on ALL dev data, and
# saves everything needed to predict on the holdout in Part 2.
# DEVELOPMENT DATA ONLY.
#
# Run from the project root (after 02):  source("Models/03_final_model.R")
# Outputs (Models/final_pipeline/):
#   final_fit.rds     fitted workflow (recipe + model, fit on all 23,233 dev rows)
#   pipeline_meta.rds algorithm, feature set, tuning values, seed, Y_SCALE, prep_data()
#   sessionInfo.txt   R and package versions
#
# Part 2 usage (do NOT run until Part 2):
#   source("Models/00_setup.R")
#   fit  <- readRDS("Models/final_pipeline/final_fit.rds")
#   pred <- predict_earnings(fit, holdout_raw)     # dollars
# =============================================================================

source(file.path("Models", "00_setup.R"))

dev <- load_dev()
comparison <- read.csv(file.path(RES_DIR, "comparison_table.csv"), check.names = FALSE)
models <- comparison[comparison$Algorithm != "Baseline (training-fold median)", ]
models <- models[order(models$MAE), ]
best <- models[1, ]

algo <- names(ALGORITHMS)[ALGORITHMS == best$Algorithm]
fs   <- best$`Feature set`
vars <- FEATURE_SETS[[fs]]
res  <- readRDS(file.path(RES_DIR, paste0("tune_", algo, "_", fs, ".rds")))$res

cat(sprintf("Selected: %s / %s feature set; CV MAE $%s (SE $%s)\n", best$Algorithm, fs,
            format(round(best$MAE), big.mark = ","), format(round(best$MAE_SE), big.mark = ",")))
runner_up <- models[2, ]
cat(sprintf("Runner-up: %s / %s; CV MAE $%s, gap $%s = %.1f SE of the winner\n",
            runner_up$Algorithm, runner_up$`Feature set`, format(round(runner_up$MAE), big.mark = ","),
            format(round(runner_up$MAE - best$MAE), big.mark = ","),
            (runner_up$MAE - best$MAE) / best$MAE_SE))

wf <- workflow(make_recipe(vars, RECIPE_TYPE[[algo]], dev), make_spec(algo))
params <- if (algo == "lm") NULL else select_best(res, metric = "mae")
if (!is.null(params)) wf <- finalize_workflow(wf, params)

future::plan(future::sequential)
set.seed(SEED)
final_fit <- fit(wf, data = dev)                 # recipe and model refit on all dev data

saveRDS(final_fit, file.path(FINAL_DIR, "final_fit.rds"))
saveRDS(list(algorithm = best$Algorithm, feature_set = fs, variables = vars,
             tuning = params, cv_mae = best$MAE, cv_mae_se = best$MAE_SE,
             seed = SEED, y_scale = Y_SCALE, prep_data = prep_data,
             fold_file = FOLD_PATH, created = Sys.time()),
        file.path(FINAL_DIR, "pipeline_meta.rds"))
writeLines(capture.output(sessionInfo()), file.path(FINAL_DIR, "sessionInfo.txt"))

# Sanity check: in-sample fit on dev (will look better than CV; not a performance estimate)
p_dev <- predict(final_fit, dev)$.pred * Y_SCALE
cat(sprintf("In-sample dev MAE (sanity check only): $%s\n",
            format(round(mean(abs(dev$y * Y_SCALE - p_dev))), big.mark = ",")))
message("Saved final pipeline to ", normalizePath(FINAL_DIR))
