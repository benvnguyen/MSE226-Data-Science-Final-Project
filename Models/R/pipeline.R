# =============================================================================
# pipeline.R: run_pipeline(cfg) does the whole of Section 6:
#   baseline -> 10-fold CV (+ tuning) for each model x feature set -> results
#   table -> refit the best model on all dev data -> save everything.
# Used by both Models/run_all.R (no GUI) and app.R (GUI). DEV DATA ONLY.
# =============================================================================

for (f in c("data.R", "ols.R", "rf.R", "nn.R")) source(file.path("Models", "R", f))
suppressPackageStartupMessages(library(ggplot2))

MODEL_NAMES <- c(baseline = "Baseline (training-fold median)", ols = "Linear regression (OLS)",
                 rf = "Random forest", qrf = "Random forest (median)", nn = "Neural network")
ALGO_ORDER <- c("ols", "rf", "qrf", "nn")
cfg_key <- function(algo) if (algo == "qrf") "rf" else algo   # the median RF shares the RF settings
RESULTS_DIR <- file.path("Models", "results")
FINAL_DIR   <- file.path("Models", "final_model")

# ---- logging + progress (the GUI reads these files) -------------------------------
.log_env <- new.env()
say <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", paste0(...))
  cat(msg, "\n", sep = "")
  if (!is.null(.log_env$file)) cat(msg, "\n", sep = "", file = .log_env$file, append = TRUE)
}
set_progress <- function(stage, done = 0, total = 1) {
  if (!is.null(.log_env$progress))
    saveRDS(list(stage = stage, done = done, total = total, time = Sys.time()), .log_env$progress)
}

merge_config <- function(cfg, defaults = DEFAULT_CONFIG) {   # fill in anything the user left out
  for (k in names(defaults)) {
    if (is.null(cfg[[k]])) cfg[[k]] <- defaults[[k]]
    else if (is.list(defaults[[k]])) cfg[[k]] <- merge_config(cfg[[k]], defaults[[k]])
  }
  cfg
}

params_text <- function(algo, p) switch(algo,
  baseline = "-", ols = "-",
  rf = , qrf = sprintf("trees=%d, mtry=%d, min_n=%d", p$trees, p$mtry, p$min_n),
  nn = sprintf("%s units, lr=%s, L2=%s, batch=%d", paste(p$hidden, collapse = "-"),
               format(p$lr, digits = 3), format(p$l2, digits = 3), p$batch_size))

# ---- CV of one setting (OLS / RF / baseline run here; NN runs on the cluster) -------
cv_serial <- function(algo, vars, params, dev, fold, seed, threads) {
  t(sapply(sort(unique(fold)), function(k) {
    tr <- dev[fold != k, ]; te <- dev[fold == k, ]
    pred <- switch(algo,
      baseline = rep(median(tr$y), nrow(te)),
      ols = ols_predict(ols_fit(tr, vars), te),
      rf  = rf_predict(rf_fit(tr, vars, params, seed, threads), te, threads),
      qrf = rf_predict(rf_fit(tr, vars, params, seed, threads, quantreg = TRUE), te, threads))
    fold_metrics(te$y, pred)
  }))
}

nn_task <- function(t) {            # runs on a worker: one (setting, fold)
  tr <- DEV[FOLD != t$k, ]; te <- DEV[FOLD == t$k, ]
  m <- nn_fit(tr, FEATURE_SETS[[t$fs]], t$params, t$seed)
  list(cfg = t$cfg, k = t$k, metrics = fold_metrics(te$y, nn_predict(m, te)),
       curve = cbind(m$curve, fold = t$k), best_epoch = m$best_epoch)
}

start_cluster <- function(n, dev, fold) {
  cl <- parallel::makePSOCKcluster(n)
  root <- getwd()
  parallel::clusterCall(cl, function(root) {
    setwd(root)
    for (f in c("data.R", "nn.R")) source(file.path("Models", "R", f))
    torch::torch_set_num_threads(1)     # one thread per worker; workers run in parallel
    NULL
  }, root)
  parallel::clusterExport(cl, c("dev", "fold"), envir = environment())
  parallel::clusterEvalQ(cl, { DEV <- dev; FOLD <- fold; NULL })
  cl
}

# ---- one algorithm on one feature set: list of settings -> CV results ---------------
run_algo <- function(algo, fs, cfg, dev, fold, cl, budget_min) {
  vars <- FEATURE_SETS[[fs]]; seed <- cfg$seed; t0 <- Sys.time()
  elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "mins"))
  # 1. settings to evaluate
  settings <- switch(algo,
    baseline = list(list()), ols = list(list()),
    rf = , qrf = {
      p <- cfg$rf
      if (p$mode == "manual") list(p) else {
        mt <- if (all(is.na(p$grid_mtry))) rf_mtry_grid(length(vars)) else p$grid_mtry
        g <- expand.grid(mtry = pmin(mt, length(vars)), min_n = p$grid_min_n)
        lapply(seq_len(nrow(g)), function(i) modifyList(p, as.list(g[i, ])))
      }
    },
    nn = {
      p <- cfg$nn
      if (p$mode == "manual") list(p) else {
        n <- if (is.na(p$search_n)) 500 else p$search_n
        g <- nn_sample_configs(p, n, seed)
        lapply(seq_len(nrow(g)), function(i) modifyList(p, as.list(g[i, ])))
      }
    })
  out <- list(); curves <- list(); best_epochs <- list()
  # 2. evaluate, stopping early if the time budget runs out
  if (algo != "nn") {
    for (i in seq_along(settings)) {
      if (!is.na(budget_min) && i > 1 && elapsed() * i / (i - 1) > budget_min) {
        say(sprintf("    time budget (%.0f min) reached after %d of %d settings", budget_min, i - 1, length(settings))); break
      }
      set_progress(sprintf("%s / %s", MODEL_NAMES[[algo]], fs), i - 1, length(settings))
      out[[i]] <- list(params = settings[[i]],
                       folds = cv_serial(algo, vars, settings[[i]], dev, fold, seed, cfg$n_cores))
      if (algo %in% c("rf", "qrf")) say(sprintf("    %-32s MAE $%s", params_text("rf", settings[[i]]),
                                    format(round(mean(out[[i]]$folds[, "MAE"])), big.mark = ",")))
    }
  } else {
    K <- sort(unique(fold)); per_batch <- max(1, floor(2 * length(cl) / length(K)) + 1)
    for (b in seq(1, length(settings), by = per_batch)) {
      ids <- b:min(b + per_batch - 1, length(settings))
      if (!is.na(budget_min) && b > 1 && elapsed() * (max(ids)) / (b - 1) > budget_min) {
        say(sprintf("    time budget (%.0f min) reached after %d settings", budget_min, b - 1)); break
      }
      set_progress(sprintf("%s / %s", MODEL_NAMES[["nn"]], fs), b - 1, length(settings))
      tasks <- unlist(lapply(ids, function(i) lapply(K, function(k)
        list(cfg = i, k = k, fs = fs, seed = seed, params = settings[[i]]))), recursive = FALSE)
      res <- parallel::parLapplyLB(cl, tasks, nn_task)
      for (i in ids) {
        r <- Filter(function(x) x$cfg == i, res)
        out[[i]] <- list(params = settings[[i]], folds = do.call(rbind, lapply(r, `[[`, "metrics")))
        curves[[i]] <- do.call(rbind, lapply(r, `[[`, "curve"))
        best_epochs[[i]] <- sapply(r, `[[`, "best_epoch")
        say(sprintf("    %-45s MAE $%s", params_text("nn", settings[[i]]),
                    format(round(mean(out[[i]]$folds[, "MAE"])), big.mark = ",")))
      }
    }
  }
  # 3. summarise every setting, pick the lowest mean MAE
  tune <- do.call(rbind, lapply(seq_along(out), function(i) {
    s <- summarise_folds(out[[i]]$folds)
    data.frame(model = algo, features = fs, setting = i, settings_text = params_text(algo, out[[i]]$params),
               t(setNames(s$mean, s$metric)), MAE_SE = s$se[s$metric == "MAE"], check.names = FALSE)
  }))
  best <- which.min(tune$MAE)
  list(tune = tune, best = best, params = out[[best]]$params, folds = out[[best]]$folds,
       curve = if (algo == "nn") curves[[best]], best_epochs = if (algo == "nn") best_epochs[[best]],
       n_tried = length(out), minutes = elapsed())
}

# ---- main ----------------------------------------------------------------------
run_pipeline <- function(cfg = DEFAULT_CONFIG, out_dir = RESULTS_DIR) {
  cfg <- merge_config(cfg)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(FINAL_DIR, showWarnings = FALSE, recursive = TRUE)
  .log_env$file <- file.path(out_dir, "run_log.txt"); .log_env$progress <- file.path(out_dir, "run_progress.rds")
  cat("", file = .log_env$file)
  t_start <- Sys.time()
  say("Section 6 pipeline started. Models: ", paste(cfg$models, collapse = ", "),
      " | feature sets: ", paste(cfg$feature_sets, collapse = ", "), " | cores: ", cfg$n_cores)

  dev <- load_dev(); fold <- load_folds(dev)
  say(sprintf("Dev data: %d rows, %d households, %d folds (household-grouped)",
              nrow(dev), length(unique(dev$SERIALNO)), length(unique(fold))))

  cl <- NULL
  if ("nn" %in% cfg$models) {
    say("Starting ", cfg$n_cores, " parallel workers for the neural network...")
    cl <- start_cluster(cfg$n_cores, dev, fold); on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  runs <- list()
  say("Baseline: training-fold median")
  runs[["baseline|-"]] <- run_algo("baseline", "small", cfg, dev, fold, cl, NA)
  for (algo in intersect(ALGO_ORDER, cfg$models)) for (fs in cfg$feature_sets) {
    budget <- cfg[[cfg_key(algo)]]$time_budget_min
    if (!is.null(budget) && !is.na(budget)) budget <- budget / length(cfg$feature_sets)
    say(MODEL_NAMES[[algo]], " / ", fs, " feature set (", length(FEATURE_SETS[[fs]]), " predictors)")
    runs[[paste(algo, fs, sep = "|")]] <- run_algo(algo, fs, cfg, dev, fold, cl, if (is.null(budget)) NA else budget)
    say(sprintf("  done in %.1f min", runs[[paste(algo, fs, sep = "|")]]$minutes))
  }
  set_progress("Building results table", 0, 1)

  # ---- results table ----
  keys <- names(runs)
  best_key <- keys[-1][which.min(sapply(runs[-1], function(r) mean(r$folds[, "MAE"])))]
  base_mae <- mean(runs[[1]]$folds[, "MAE"])
  summary <- do.call(rbind, lapply(keys, function(key) {
    r <- runs[[key]]; algo <- sub("\\|.*", "", key); fs <- sub(".*\\|", "", key)
    s <- summarise_folds(r$folds); m <- setNames(s$mean, s$metric); se <- setNames(s$se, s$metric)
    d <- r$folds[, "MAE"] - runs[[best_key]]$folds[, "MAE"]        # paired, same folds
    data.frame(Model = MODEL_NAMES[[algo]], Features = fs, Settings = params_text(algo, r$params),
               MAE = m[["MAE"]], MAE_SE = se[["MAE"]], RMSE = m[["RMSE"]], RMSE_SE = se[["RMSE"]],
               R2 = m[["R2"]], MedAE = m[["MedAE"]], Bias = m[["Bias"]], Within10k = m[["Within10k"]],
               Gain_vs_baseline = 1 - m[["MAE"]] / base_mae,
               dMAE_vs_best = mean(d), dMAE_vs_best_SE = sd(d) / sqrt(length(d)),
               Settings_tried = r$n_tried, Minutes = r$minutes, check.names = FALSE)
  }))
  summary$Features[1] <- "-"
  tuning <- do.call(rbind, lapply(runs[-1], `[[`, "tune")); rownames(tuning) <- NULL
  folds_long <- do.call(rbind, lapply(keys, function(key)
    data.frame(key = key, fold = seq_len(nrow(runs[[key]]$folds)), runs[[key]]$folds)))
  nn_curves <- do.call(rbind, lapply(keys[grepl("^nn", keys)], function(key)
    data.frame(features = sub(".*\\|", "", key), runs[[key]]$curve)))

  write.csv(summary, file.path(out_dir, "summary_table.csv"), row.names = FALSE)
  write.csv(tuning, file.path(out_dir, "tuning_all_settings.csv"), row.names = FALSE)
  write.csv(folds_long, file.path(out_dir, "fold_metrics.csv"), row.names = FALSE)
  if (!is.null(nn_curves)) write.csv(nn_curves, file.path(out_dir, "nn_training_curves.csv"), row.names = FALSE)
  saveRDS(cfg, file.path(out_dir, "config_used.rds"))
  say("Results table:"); print_summary(summary)
  plot_cv(summary, file.path(out_dir, "cv_mae.png"))
  if (!is.null(nn_curves)) plot_nn_curves(nn_curves, file.path(out_dir, "nn_training_curves.png"))

  # ---- model structure details for the GUI / report ----
  for (fs in cfg$feature_sets) if ("ols" %in% cfg$models)
    write.csv(ols_describe(ols_fit(dev, FEATURE_SETS[[fs]])), file.path(out_dir, paste0("ols_coefficients_", fs, ".csv")),
              row.names = FALSE)

  # ---- refit the best model on all dev ----
  if (isTRUE(cfg$refit_final)) {
    algo <- sub("\\|.*", "", best_key); fs <- sub(".*\\|", "", best_key); r <- runs[[best_key]]
    set_progress("Refitting best model on all dev data", 0, 1)
    say("Final model: ", MODEL_NAMES[[algo]], " / ", fs, " (", params_text(algo, r$params), ")")
    vars <- FEATURE_SETS[[fs]]
    final <- switch(algo,
      ols = ols_fit(dev, vars),
      rf  = rf_fit(dev, vars, r$params, cfg$seed, cfg$n_cores, importance = "impurity", keep.inbag = TRUE),
      qrf = rf_fit(dev, vars, r$params, cfg$seed, cfg$n_cores, quantreg = TRUE),
      nn  = nn_fit(dev, vars, r$params, cfg$seed))
    meta <- list(model = algo, features = fs, variables = vars, params = r$params,
                 cv_mae = mean(r$folds[, "MAE"]), cv_mae_se = sd(r$folds[, "MAE"]) / sqrt(nrow(r$folds)),
                 seed = cfg$seed, y_scale = Y_SCALE, fold_file = FOLD_PATH, created = Sys.time())
    if (algo == "nn") {
      torch::torch_save(final$net, file.path(FINAL_DIR, "final_nn_weights.pt"))
      final$net <- NULL                                   # torch objects can't go in .rds
    } else {
      unlink(file.path(FINAL_DIR, "final_nn_weights.pt")) # drop weights left by an earlier NN final model
    }
    saveRDS(final, file.path(FINAL_DIR, "final_model.rds")); saveRDS(meta, file.path(FINAL_DIR, "final_meta.rds"))
    writeLines(capture.output(sessionInfo()), file.path(FINAL_DIR, "sessionInfo.txt"))
  }
  # RF structure details (importance + OOB-vs-trees) from the best RF setting
  rf_keys <- keys[grepl("^rf", keys)]
  if (length(rf_keys)) {
    k <- rf_keys[which.min(sapply(runs[rf_keys], function(r) mean(r$folds[, "MAE"])))]
    set_progress("Random forest: importance and OOB curve", 0, 1)
    say("Random forest details (", sub(".*\\|", "", k), " set): variable importance + OOB error vs number of trees")
    m <- if (isTRUE(cfg$refit_final) && k == best_key) final else
      rf_fit(dev, FEATURE_SETS[[sub(".*\\|", "", k)]], runs[[k]]$params, cfg$seed, cfg$n_cores,
             importance = "impurity", keep.inbag = TRUE)
    imp <- sort(m$fit$variable.importance, decreasing = TRUE)
    write.csv(data.frame(variable = names(imp), importance = imp / sum(imp)),
              file.path(out_dir, "rf_importance.csv"), row.names = FALSE)
    oob <- rf_oob_curve(m, dev, cfg$n_cores)
    write.csv(oob, file.path(out_dir, "rf_oob_curve.csv"), row.names = FALSE)
    plot_oob(oob, file.path(out_dir, "rf_oob_curve.png"))
  }
  total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  say(sprintf("Finished in %.1f min. Results in %s", total, normalizePath(out_dir)))
  set_progress("Done", 1, 1)
  invisible(list(summary = summary, tuning = tuning, runs = runs, best = best_key))
}

# ---- reuse the saved final model (Part 2) --------------------------------------------
load_final_model <- function() {
  meta <- readRDS(file.path(FINAL_DIR, "final_meta.rds"))
  m <- readRDS(file.path(FINAL_DIR, "final_model.rds"))
  if (meta$model == "nn") m$net <- torch::torch_load(file.path(FINAL_DIR, "final_nn_weights.pt"))
  list(model = m, meta = meta)
}
# raw = rows in the same format as data/dev_raw.rds; returns predicted earnings in $
predict_final <- function(raw, final = load_final_model()) {
  d <- prep_data(raw)
  p <- switch(final$meta$model, ols = ols_predict(final$model, d),
              rf = , qrf = rf_predict(final$model, d), nn = nn_predict(final$model, d))
  p * Y_SCALE
}

# ---- printing and plots ------------------------------------------------------------
dollars <- function(x) paste0("$", format(round(x), big.mark = ",", trim = TRUE))
format_summary <- function(s) data.frame(
  Model = s$Model, Features = s$Features, `Selected settings` = s$Settings,
  MAE = sprintf("%s (%s)", dollars(s$MAE), format(round(s$MAE_SE), big.mark = ",")),
  RMSE = sprintf("%s (%s)", dollars(s$RMSE), format(round(s$RMSE_SE), big.mark = ",")),
  R2 = sprintf("%.3f", s$R2), `Median AE` = dollars(s$MedAE), Bias = dollars(s$Bias),
  `Within $10k` = sprintf("%.0f%%", 100 * s$Within10k),
  `MAE gain vs baseline` = sprintf("%.0f%%", 100 * s$Gain_vs_baseline),
  `vs best (paired SE)` = ifelse(abs(s$dMAE_vs_best) < 1e-9, "best",
                                 sprintf("+%s (%s)", dollars(s$dMAE_vs_best), round(s$dMAE_vs_best_SE))),
  Tried = s$Settings_tried, Minutes = sprintf("%.1f", s$Minutes), check.names = FALSE)
print_summary <- function(s) print(format_summary(s), row.names = FALSE, right = FALSE)

plot_cv <- function(s, path) {
  pd <- s[-1, ]; pd$Model <- factor(pd$Model, levels = unique(pd$Model))
  p <- ggplot(pd, aes(Model, MAE, colour = Features)) + scale_x_discrete() +
    geom_hline(yintercept = s$MAE[1], linetype = "dashed", colour = "grey40") +
    annotate("text", x = 0.6, y = s$MAE[1], label = "Baseline (median)", vjust = -0.5, hjust = 0, size = 3.5) +
    geom_pointrange(aes(ymin = MAE - MAE_SE, ymax = MAE + MAE_SE), position = position_dodge(width = 0.4)) +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_colour_manual(values = c(small = "#0072B2", large = "#D55E00")) +
    labs(title = "Cross-validated MAE by model and feature set",
         subtitle = "10 household-grouped folds on dev data; points = mean, bars = +/- 1 SE",
         x = NULL, y = "MAE (2024 $)", colour = "Feature set") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(path, p, width = 8, height = 5, dpi = 150, bg = "white"); invisible(p)
}
plot_oob <- function(oob, path) {
  p <- ggplot(oob, aes(trees, oob_mae)) + geom_line(colour = "#009E73", linewidth = 1) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(title = "Random forest: out-of-bag MAE vs number of trees", x = "Trees", y = "OOB MAE") +
    theme_minimal(base_size = 12)
  ggsave(path, p, width = 7, height = 4, dpi = 150, bg = "white"); invisible(p)
}
plot_nn_curves <- function(cv, path) {
  d <- rbind(data.frame(cv[c("features", "fold", "epoch")], split = "train", loss = cv$train_loss),
             data.frame(cv[c("features", "fold", "epoch")], split = "validation", loss = cv$val_loss))
  p <- ggplot(d, aes(epoch, loss * Y_SCALE, colour = split, group = interaction(fold, split))) +
    geom_line(alpha = 0.5) + facet_wrap(~features, scales = "free_y") +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_colour_manual(values = c(train = "#0072B2", validation = "#D55E00")) +
    labs(title = "Neural network training curves (selected setting, one line per fold)",
         x = "Epoch", y = "Loss (mean absolute error, $)", colour = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  ggsave(path, p, width = 9, height = 4.5, dpi = 150, bg = "white"); invisible(p)
}
