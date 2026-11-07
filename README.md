# MS&E 226 Project: Predicting Post-9/11 Veteran Earnings (ACS 2024 PUMS)

Grace Hendricks, Ben Nguyen, Erick Rodriguez

## Run the models (Section 6) in 3 steps

**1. Open the project in RStudio.** Use **Session > Set Working Directory > Choose Directory** and pick this folder
(the one that contains `app.R`).

**2. Install the packages (one time only).** Paste this into the R console:
```r
install.packages(c("recipes", "dplyr", "ranger", "torch", "ggplot2", "scales",
                   "shiny", "bslib", "DT", "callr"))
torch::install_torch()   # one-time download of the torch engine (~560 MB)
```

**3. Press one button.** Pick either:

| Option | How | Use it when |
|---|---|---|
| **GUI** (recommended) | Open `app.R` and click **Run App** (top right of the editor) | You want to choose models, change settings, watch progress and browse results |
| **No GUI** | Open `Models/run_all.R` and click **Source** | You just want the default full run |

Both run the same thing: 10-fold cross-validation of **OLS, random forest, median random forest and neural network** on
the **small** and **large** feature sets, plus the median baseline. A default run takes about 75 minutes, mostly the
neural-net search and the median forest.

## Using the GUI
- **Left panel:** tick models and feature sets. Open a model's panel to change its settings:
  - **"Find the best settings"** searches automatically (grid for the random forest, random search for the neural net).
  - **"Use my values"** runs exactly the values you type.
  - **Time budget (optional):** stops the search after that many minutes and keeps the best setting found.
  - Leave any optional box blank to use the default.
- **Run tab:** progress bar and live log. **Stop** cancels the run.
- **Results tab:** the error table (MAE, RMSE, R², median absolute error, bias, % within $10k, gain vs baseline,
  and the difference from the best model with a paired SE), a plot, and every setting tried.
- **Model structure tab:** how each model works: OLS coefficients, the random forest's variable importance and
  error vs number of trees, and the neural network's diagram, weight counts and training curves.

## Where things are
```
app.R                     the GUI (click Run App)
Models/
  config.R                ALL default settings (trees, grids, NN architecture and search ranges, time budgets)
  run_all.R               one-click run without the GUI
  R/data.R                data loading, fixed recodes, feature sets, preprocessing, folds, metrics
  R/ols.R  R/rf.R  R/nn.R the three models (each file explains its method at the top)
  R/pipeline.R            the runner used by both the GUI and run_all.R
  dev_folds.csv           the fixed 10 CV folds (households kept together, seed 32)
  make_folds.R            rebuilds dev_folds.csv (not needed: it is already here)
  analysis/               Section 7 scenario table (scenario_full_time.R; run after the pipeline)
  results/                output: summary_table.csv, plots, tuning and fold-level tables
  final_model/            the best model refit on all dev data
  archive_v1/             the earlier version (nnet, pooled categories), kept for reference
Plots/                    Section 5 EDA scripts and figures
data/dev_raw.rds          development data (80%, n = 23,233). The ONLY data used in Part 1
```

## The models in one paragraph each
- **OLS:** earnings = intercept + sum of coefficient × predictor. Each category level gets a 0/1 dummy.
- **Random forest:** each tree is grown on a **bootstrap sample** (n rows drawn with replacement). At each split it
  considers a random `mtry` of the predictors and picks the best cut. Leaves predict mean earnings, and the
  forest averages all trees. Settings: `trees` (set it in the GUI or `config.R`), `mtry`, `min_n`.
- **Median random forest:** the same trees, but each leaf keeps all its training earnings, and the prediction is the
  **median** of the earnings in the leaves a person lands in, instead of the average. MAE, our metric, is minimized by the
  median. It uses the random forest's settings.
- **Neural network:** inputs → 4 ReLU units → 3 ReLU units → 1 output, built in torch. It is trained with **Adam** on
  **mini-batches**, with **L2** regularization, learning-rate decay and **early stopping**, on absolute-error loss (the same
  quantity MAE measures). The learning rate, L2 strength and batch size are tuned by random search.

All categories keep every level (no "other" group). The 170 college majors (FOD1P) are mapped to 13 named
families using the official 2024 PUMS dictionary labels.

## Data and reproducibility
- Download the 2024 ACS 1-Year PUMS person files (`psam_pusa.csv`, `psam_pusb.csv`) from
  https://www.census.gov/programs-surveys/acs/microdata/access.html and run `project_part1_data_extraction.Rmd`
  (edit the file paths at the top) to recreate the 80/20 split.
- Seed **32** is used for the split, the CV folds and every model.
- The fitted-model files (`*.rds`, `*.pt`) are gitignored; re-running recreates them. Exact package versions are in
  `Models/final_model/sessionInfo.txt`.
- **Holdout:** the 20% holdout is never used in Part 1. No script in `Plots/`, `Models/` or `app.R` reads it.
- Section 5 figures: `source("Plots/eda_outcome_plots.R")` and `source("Plots/eda_section5.R")`.
