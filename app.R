# =============================================================================
# app.R: point-and-click GUI for the Section 6 pipeline.
# In RStudio: open this file and click "Run App" (top right of the editor).
# Or from the R console in the project folder: shiny::runApp()
#
# The pipeline runs in a separate background R process, so the app stays
# responsive. Progress is read from Models/results/run_log.txt.
# DEV DATA ONLY: nothing here reads holdout_raw.rds or the raw CSV.
# =============================================================================

library(shiny); library(bslib); library(DT)
source(file.path("Models", "config.R"))
source(file.path("Models", "R", "pipeline.R"))

ROOT <- normalizePath(".")
D <- DEFAULT_CONFIG

# number of NN inputs for each feature set (for the architecture diagram)
NN_INPUTS <- local({
  dev <- load_dev()
  sapply(FEATURE_SETS, function(v) ncol(bake(prep(make_recipe(v, "nn", dev)), NULL)) - 1)
})

parse_nums <- function(txt) {                  # "4, 3" -> c(4, 3); blank -> NULL
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(txt, "[,; ]+")[[1]]))
  v[!is.na(v)]
}
opt_num <- function(x) if (is.null(x) || is.na(x)) NULL else x

# ---- architecture drawing -------------------------------------------------------
draw_nn <- function(p_in, hidden) {
  layers <- c(p_in, hidden, 1); L <- length(layers)
  shown <- pmin(layers, 8)
  par(mar = c(1, 1, 3, 1))
  plot(NA, xlim = c(0.5, L + 0.5), ylim = c(0, 9.5), axes = FALSE, xlab = "", ylab = "",
       main = sprintf("Neural network: %s  (%s weights)", paste(layers, collapse = " -> "),
                      format(nn_n_weights(p_in, hidden), big.mark = ",")))
  y_of <- function(n) if (n == 1) 4.5 else seq(8.5, 0.5, length.out = n)
  for (l in seq_len(L - 1)) for (a in y_of(shown[l])) for (b in y_of(shown[l + 1]))
    segments(l, a, l + 1, b, col = adjustcolor("grey50", 0.25))
  cols <- c("#0072B2", rep("#E69F00", length(hidden)), "#009E73")
  for (l in seq_len(L)) {
    points(rep(l, shown[l]), y_of(shown[l]), pch = 21, cex = 3, bg = cols[l], col = "white", lwd = 2)
    lab <- if (l == 1) sprintf("Inputs\n%d standardized\ncolumns", p_in) else if (l == L)
      "Output\nearnings / $10k\n(linear)" else sprintf("Hidden %d\n%d ReLU units", l - 1, layers[l])
    text(l, 9.4, lab, cex = 0.85, font = 2, xpd = TRUE)
  }
  if (p_in > 8) text(1, 0, sprintf("(8 of %d shown)", p_in), cex = 0.8, col = "grey30")
}

# ---- UI -----------------------------------------------------------------------
num <- function(id, label, value, ...) numericInput(id, label, value, ...)
ui <- page_navbar(
  id = "navbar",
  title = "Veteran earnings: Section 6 models",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(width = 360,
    h5("What to run"),
    checkboxGroupInput("models", NULL, inline = TRUE,
      choices = c("OLS" = "ols", "Random forest" = "rf", "Median RF" = "qrf", "Neural net" = "nn"),
      selected = D$models),
    checkboxGroupInput("fsets", "Feature sets", inline = TRUE,
      choices = c("Small (8)" = "small", "Large (11)" = "large"), selected = D$feature_sets),
    accordion(open = FALSE,
      accordion_panel("Random forest settings (also used by Median RF)",
        num("rf_trees", "Number of trees", D$rf$trees, min = 10, step = 50),
        radioButtons("rf_mode", NULL, c("Find the best settings (grid search)" = "tune",
                                        "Use my values" = "manual"), selected = D$rf$mode),
        conditionalPanel("input.rf_mode == 'manual'",
          layout_columns(num("rf_mtry", "mtry", D$rf$mtry, min = 1), num("rf_min_n", "min_n", D$rf$min_n, min = 1))),
        conditionalPanel("input.rf_mode == 'tune'",
          textInput("rf_grid_mtry", "mtry values (blank = 4 values from 2 to p)", ""),
          textInput("rf_grid_min_n", "min_n values", paste(D$rf$grid_min_n, collapse = ", "))),
        num("rf_budget", "Time budget, minutes (optional)", NA, min = 0.5)),
      accordion_panel("Neural network settings",
        textInput("nn_hidden", "Hidden layer sizes", paste(D$nn$hidden, collapse = ", ")),
        selectInput("nn_loss", "Loss", c("Absolute error (L1)" = "l1", "Squared error (MSE)" = "mse"), D$nn$loss),
        radioButtons("nn_mode", NULL, c("Find the best settings (random search)" = "tune",
                                        "Use my values" = "manual"), selected = D$nn$mode),
        conditionalPanel("input.nn_mode == 'manual'",
          layout_columns(num("nn_lr", "Learning rate", D$nn$lr, step = 1e-3),
                         num("nn_l2", "L2 penalty", D$nn$l2, step = 1e-4)),
          num("nn_bs", "Batch size", D$nn$batch_size, min = 16, step = 64)),
        conditionalPanel("input.nn_mode == 'tune'",
          num("nn_search_n", "Settings to try per feature set (blank = until time runs out)", D$nn$search_n, min = 1),
          layout_columns(num("nn_lr_lo", "LR min", D$nn$search_lr[1]), num("nn_lr_hi", "LR max", D$nn$search_lr[2])),
          layout_columns(num("nn_l2_lo", "L2 min", D$nn$search_l2[1]), num("nn_l2_hi", "L2 max", D$nn$search_l2[2])),
          textInput("nn_bs_set", "Batch sizes to try", paste(D$nn$search_batch_size, collapse = ", "))),
        layout_columns(num("nn_epochs", "Max epochs", D$nn$max_epochs, min = 1),
                       num("nn_patience", "Early-stop patience", D$nn$patience, min = 1)),
        num("nn_budget", "Time budget, minutes (optional)", D$nn$time_budget_min, min = 0.5)),
      accordion_panel("General",
        num("cores", "CPU cores", D$n_cores, min = 1, max = parallel::detectCores()),
        checkboxInput("refit", "Refit the best model on all dev data", D$refit_final))),
    layout_columns(
      actionButton("run", "Run 10-fold CV", class = "btn-primary", icon = icon("play")),
      actionButton("stop", "Stop", class = "btn-outline-danger", icon = icon("stop")))
  ),
  nav_panel("Run",
    card(card_header("Progress"), uiOutput("status"), verbatimTextOutput("log", placeholder = TRUE),
         style = "min-height: 70vh")),
  nav_panel("Results",
    card(card_header("10-fold CV results: mean (SE across folds). Lower MAE/RMSE is better"),
         DTOutput("summary"),
         p(class = "text-muted small",
           "Median AE = median absolute error. Bias = mean(prediction - actual). Within $10k = share predicted",
           "within $10,000. 'vs best' = MAE difference from the best model, with a paired SE (same folds).")),
    layout_columns(card(card_header("MAE by model"), plotOutput("cvplot", height = 380)),
                   card(card_header("All settings tried"), DTOutput("tuning")))),
  nav_panel("Model structure",
    navset_card_tab(
      nav_panel("Linear regression",
        markdown(paste(
          "**Ordinary least squares.** Prediction = intercept + sum of coefficient x predictor.",
          "Each category level is a 0/1 dummy column (the first level is the reference). Coefficients",
          "minimize the sum of squared errors on the training fold. No hyperparameters.")),
        DTOutput("ols_coef")),
      nav_panel("Random forest",
        markdown(paste(
          "**How each tree is built:**",
          "1. **Bootstrap:** draw *n* rows from the training data *with replacement*. About 63% of rows",
          "appear at least once; the other ~37% are the tree's *out-of-bag* (OOB) rows.",
          "2. At **every node**, pick `mtry` predictors at random (a new draw at each node).",
          "3. Among those, choose the variable and cut point that most reduce squared error.",
          "4. Keep splitting until a node has fewer than `min_n` rows (no pruning).",
          "5. A leaf predicts the mean earnings of its training rows.",
          "",
          "**Prediction** = the average over all trees. Category levels are sorted by mean earnings",
          "before growing (`respect.unordered.factors = \"order\"`). More trees never overfit: they only",
          "reduce the noise of the average, so the OOB curve below shows where adding trees stops helping.",
          sep = "\n")),
        layout_columns(card(card_header("Variable importance (impurity, share of total)"), plotOutput("rf_imp", height = 320)),
                       card(card_header("OOB MAE vs number of trees"), plotOutput("rf_oob", height = 320)))),
      nav_panel("Neural network",
        layout_columns(col_widths = c(7, 5),
          card(radioButtons("nn_draw_fs", NULL, c("Small set" = "small", "Large set" = "large"), "large", inline = TRUE),
               plotOutput("nn_diagram", height = 420)),
          card(markdown(paste(
            "**Forward pass:** each hidden unit computes `ReLU(w . x + b)`; the output unit is linear,",
            "giving earnings in $10k (times 10,000 = dollars).",
            "",
            "**Training (CS230 techniques):**",
            "- Standardized inputs (mean 0, SD 1), fit on the training fold",
            "- He initialization (ReLU layers), Xavier (output); output bias starts at the median",
            "- **Adam** on shuffled **mini-batches**",
            "- **L2 regularization** (Adam `weight_decay`)",
            "- Learning rate halves when validation loss stalls for 5 epochs",
            "- **Early stopping** on a 10% validation split of the training fold (by household);",
            "  best-epoch weights restored",
            "- Loss = absolute error (L1), the same quantity MAE measures",
            sep = "\n")), tableOutput("nn_weights"))),
        card(card_header("Training curves for the selected setting (one line per fold)"), plotOutput("nn_curves", height = 330))))
  )
)

# ---- server --------------------------------------------------------------------
server <- function(input, output, session) {
  job <- reactiveVal(NULL); done_tick <- reactiveVal(0); t0 <- reactiveVal(NULL); handled <- reactiveVal(TRUE)
  log_file <- file.path(RESULTS_DIR, "run_log.txt"); prog_file <- file.path(RESULTS_DIR, "run_progress.rds")

  build_cfg <- function() {
    cfg <- D
    cfg$models <- input$models; cfg$feature_sets <- input$fsets
    cfg$n_cores <- input$cores; cfg$refit_final <- input$refit
    cfg$rf$trees <- input$rf_trees; cfg$rf$mode <- input$rf_mode
    cfg$rf$mtry <- input$rf_mtry; cfg$rf$min_n <- input$rf_min_n
    cfg$rf$grid_mtry <- if (is.null(parse_nums(input$rf_grid_mtry))) NA else parse_nums(input$rf_grid_mtry)
    if (!is.null(parse_nums(input$rf_grid_min_n))) cfg$rf$grid_min_n <- parse_nums(input$rf_grid_min_n)
    cfg$rf$time_budget_min <- if (is.null(opt_num(input$rf_budget))) NA else input$rf_budget
    if (!is.null(parse_nums(input$nn_hidden))) cfg$nn$hidden <- parse_nums(input$nn_hidden)
    cfg$nn$loss <- input$nn_loss; cfg$nn$mode <- input$nn_mode
    for (k in c("lr", "l2")) if (!is.null(opt_num(input[[paste0("nn_", k)]]))) cfg$nn[[k]] <- input[[paste0("nn_", k)]]
    if (!is.null(opt_num(input$nn_bs))) cfg$nn$batch_size <- input$nn_bs
    cfg$nn$search_n <- if (is.null(opt_num(input$nn_search_n))) NA else input$nn_search_n
    if (!is.null(opt_num(input$nn_lr_lo)) && !is.null(opt_num(input$nn_lr_hi))) cfg$nn$search_lr <- c(input$nn_lr_lo, input$nn_lr_hi)
    if (!is.null(opt_num(input$nn_l2_lo)) && !is.null(opt_num(input$nn_l2_hi))) cfg$nn$search_l2 <- c(input$nn_l2_lo, input$nn_l2_hi)
    if (!is.null(parse_nums(input$nn_bs_set))) cfg$nn$search_batch_size <- parse_nums(input$nn_bs_set)
    if (!is.null(opt_num(input$nn_epochs))) cfg$nn$max_epochs <- input$nn_epochs
    if (!is.null(opt_num(input$nn_patience))) cfg$nn$patience <- input$nn_patience
    cfg$nn$time_budget_min <- if (is.null(opt_num(input$nn_budget))) NA else input$nn_budget
    if (is.na(cfg$nn$search_n) && is.na(cfg$nn$time_budget_min))
      stop("For the NN search, give a number of settings or a time budget (or both).")
    cfg
  }

  observeEvent(input$run, {
    if (!is.null(job()) && job()$is_alive()) { showNotification("A run is already in progress.", type = "warning"); return() }
    if (!length(input$models) || !length(input$fsets)) { showNotification("Pick at least one model and one feature set.", type = "error"); return() }
    cfg <- tryCatch(build_cfg(), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(cfg)) return()
    unlink(c(log_file, prog_file))
    job(callr::r_bg(function(root, cfg) {
      setwd(root); source(file.path("Models", "config.R")); source(file.path("Models", "R", "pipeline.R"))
      run_pipeline(cfg); invisible(TRUE)
    }, args = list(ROOT, cfg), supervise = TRUE, cleanup_tree = TRUE))
    t0(Sys.time()); handled(FALSE)
    nav_select("navbar", "Run"); showNotification("Run started.", type = "message")
  })
  observeEvent(input$stop, {
    if (!is.null(job()) && job()$is_alive()) { job()$kill_tree(); showNotification("Run stopped.", type = "warning") }
  })

  # poll the background job
  observe({
    invalidateLater(1000)
    j <- job(); if (is.null(j)) return()
    if (!j$is_alive() && !isolate(handled())) {
      handled(TRUE)
      res <- tryCatch({ j$get_result(); "ok" }, error = function(e) conditionMessage(e))
      if (identical(res, "ok")) { showNotification("Run finished. See the Results tab.", type = "message"); done_tick(done_tick() + 1) }
      else showNotification(paste("Run failed:", res), type = "error", duration = NULL)
    }
  })

  output$status <- renderUI({
    invalidateLater(1000)
    j <- job()
    if (is.null(j)) return(p(class = "text-muted", "Not running. Choose settings on the left and press Run."))
    pr <- if (file.exists(prog_file)) tryCatch(readRDS(prog_file), error = function(e) NULL)
    mins <- as.numeric(difftime(Sys.time(), t0(), units = "mins"))
    pct <- if (is.null(pr)) 0 else round(100 * pr$done / max(1, pr$total))
    state <- if (j$is_alive()) "Running" else "Finished"
    tagList(
      p(strong(state), sprintf(" | elapsed %.1f min", mins), if (!is.null(pr)) paste(" | stage:", pr$stage)),
      div(class = "progress", style = "height: 22px",
          div(class = "progress-bar", role = "progressbar", style = sprintf("width: %d%%", pct), sprintf("%d%%", pct))))
  })
  output$log <- renderText({
    invalidateLater(1500)
    if (!file.exists(log_file)) return("")
    paste(tail(readLines(log_file, warn = FALSE), 60), collapse = "\n")
  })

  # results (reload whenever a run finishes)
  res_file <- function(f) file.path(RESULTS_DIR, f)
  read_res <- function(f) { done_tick(); if (file.exists(res_file(f))) read.csv(res_file(f), check.names = FALSE) }
  output$summary <- renderDT({
    s <- read_res("summary_table.csv"); validate(need(!is.null(s), "No results yet: press Run."))
    f <- format_summary(s)
    datatable(f, rownames = FALSE, options = list(dom = "t", ordering = FALSE, scrollX = TRUE, pageLength = 50)) |>
      formatStyle("vs best (paired SE)", target = "row", fontWeight = styleEqual("best", "bold"),
                  backgroundColor = styleEqual("best", "#e8f5e9"))
  })
  output$cvplot <- renderPlot({
    s <- read_res("summary_table.csv"); validate(need(!is.null(s), "No results yet."))
    print(plot_cv(s, tempfile(fileext = ".png")))
  })
  output$tuning <- renderDT({
    t <- read_res("tuning_all_settings.csv"); validate(need(!is.null(t), "No results yet."))
    t <- t[c("model", "features", "settings_text", "MAE", "MAE_SE", "RMSE", "R2")]
    t$MAE <- round(t$MAE); t$MAE_SE <- round(t$MAE_SE); t$RMSE <- round(t$RMSE); t$R2 <- round(t$R2, 3)
    datatable(t[order(t$model, t$features, t$MAE), ], rownames = FALSE,
              colnames = c("Model", "Features", "Settings", "MAE", "SE", "RMSE", "R2"),
              options = list(pageLength = 8, scrollX = TRUE))
  })
  output$ols_coef <- renderDT({
    f <- list.files(RESULTS_DIR, "^ols_coefficients_.*csv$", full.names = TRUE)
    validate(need(length(f) > 0, "Run OLS to see its coefficients."))
    d <- do.call(rbind, lapply(f, function(x) cbind(features = sub(".*_(\\w+)\\.csv$", "\\1", x), read.csv(x))))
    d$coef_dollars <- round(d$coef_dollars)
    datatable(d[order(d$features, -abs(d$coef_dollars)), c("features", "term", "coef_dollars")], rownames = FALSE,
              colnames = c("Features", "Term", "Coefficient ($)"), options = list(pageLength = 10))
  })
  output$rf_imp <- renderPlot({
    d <- read_res("rf_importance.csv"); validate(need(!is.null(d), "Run the random forest to see importance."))
    d$variable <- factor(d$variable, levels = rev(d$variable))
    ggplot(d, aes(importance, variable)) + geom_col(fill = "#009E73") +
      scale_x_continuous(labels = scales::label_percent()) + labs(x = NULL, y = NULL) + theme_minimal(base_size = 13)
  })
  output$rf_oob <- renderPlot({
    d <- read_res("rf_oob_curve.csv"); validate(need(!is.null(d), "Run the random forest to see the OOB curve."))
    print(plot_oob(d, tempfile(fileext = ".png")))
  })
  output$nn_diagram <- renderPlot({
    h <- parse_nums(input$nn_hidden); if (is.null(h)) h <- D$nn$hidden
    draw_nn(NN_INPUTS[[input$nn_draw_fs]], h)
  })
  output$nn_weights <- renderTable({
    h <- parse_nums(input$nn_hidden); if (is.null(h)) h <- D$nn$hidden
    dims <- c(NN_INPUTS[[input$nn_draw_fs]], h, 1)
    data.frame(Layer = c(paste("Hidden", seq_along(h)), "Output"),
               Shape = sprintf("%d -> %d", head(dims, -1), dims[-1]),
               Weights = (head(dims, -1) + 1) * dims[-1]) |>
      rbind(data.frame(Layer = "Total", Shape = "", Weights = nn_n_weights(dims[1], h)))
  })
  output$nn_curves <- renderPlot({
    d <- read_res("nn_training_curves.csv"); validate(need(!is.null(d), "Run the neural network to see training curves."))
    print(plot_nn_curves(d, tempfile(fileext = ".png")))
  })
}

shinyApp(ui, server)
