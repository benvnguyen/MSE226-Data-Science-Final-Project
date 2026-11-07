# =============================================================================
# data.R: loading, fixed recoding, feature sets, recipes, CV folds, metrics.
# DEVELOPMENT DATA ONLY. Nothing here reads holdout_raw.rds or the raw CSV.
# =============================================================================

suppressPackageStartupMessages({ library(recipes); library(dplyr) })

Y_SCALE   <- 1e4                                   # models train on earnings / $10,000
DEV_PATH  <- file.path("data", "dev_raw.rds")
FOLD_PATH <- file.path("Models", "dev_folds.csv")

# ---- FOD1P (170 majors) -> 13 families ----------------------------------------
# Grouping uses the official 2024 PUMS dictionary labels (first two digits of the
# code = broad field). Majors with fewer than 25 dev veterans are folded into the
# closest named family. Every family has 150+ dev veterans; there is no catch-all.
FOD_FAMILY_PREFIX <- list(
  "Business"                         = "62",
  "Engineering & architecture"       = c("14", "24", "25"),
  "Computing, math & data"           = c("20", "21", "37"),
  "Health & nutrition"               = "61",
  "Life & physical sciences"         = c("36", "50", "51"),
  "Agriculture & environment"        = c("11", "13"),
  "Social sciences"                  = c("15", "55"),
  "Criminal justice, law & military" = c("32", "38", "53"),
  "Psychology & social services"     = c("52", "54"),
  "Education"                        = "23",
  "Humanities"                       = c("26", "33", "34", "35", "48", "49", "64"),
  "Arts & communications"            = c("19", "60"),
  "Applied & service fields"         = c("22", "29", "41", "56", "57", "59")
)
# "40xx" (interdisciplinary) codes are split by their dictionary labels:
FOD_FAMILY_EXCEPT <- c(
  "4000" = "Humanities",                     # Multi/Interdisciplinary Studies
  "4001" = "Social sciences",                # Intercultural And International Studies
  "4002" = "Health & nutrition",             # Nutrition Sciences
  "4005" = "Computing, math & data",         # Mathematics And Computer Science
  "4006" = "Psychology & social services",   # Cognitive Science And Biopsychology
  "4007" = "Social sciences",                # Interdisciplinary Social Sciences
  "4009" = "Computing, math & data")         # Data Science and Data Analytics
FOD_FAMILIES <- c("No bachelor's", names(FOD_FAMILY_PREFIX))

fod_family <- function(code) {
  lookup <- setNames(rep(names(FOD_FAMILY_PREFIX), lengths(FOD_FAMILY_PREFIX)),
                     unlist(FOD_FAMILY_PREFIX))
  fam <- unname(lookup[substr(code, 1, 2)])
  ex <- code %in% names(FOD_FAMILY_EXCEPT)
  fam[ex] <- FOD_FAMILY_EXCEPT[code[ex]]
  fam[is.na(code)] <- "No bachelor's"
  if (anyNA(fam)) stop("Unmapped FOD1P codes: ", paste(unique(code[is.na(fam)]), collapse = ", "))
  factor(fam, levels = FOD_FAMILIES)
}

# ---- prep_data(): fixed, rule-based recoding (nothing is learned from data) -----
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
    FOD1P    = fod_family(raw$FOD1P),
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

# ---- recipes: estimated on each training fold only ----------------------------
# Every category level is kept (no pooling into "other").
# step_novel: a level never seen in the training fold maps to "new". Its dummy is
# all zeros in training, so step_zv drops it and that person falls back to the
# reference level.
make_recipe <- function(vars, type = c("tree", "linear", "nn"), data) {
  type <- match.arg(type)
  rec <- recipe(reformulate(vars, response = "y"), data = data) |>
    step_novel(all_nominal_predictors())
  if (type != "tree")
    rec <- rec |> step_dummy(all_nominal_predictors()) |> step_zv(all_predictors())
  if (type == "nn") rec <- rec |> step_normalize(all_numeric_predictors())
  rec
}

# ---- fixed CV folds (household-grouped, built by Models/make_folds.R) -------------
load_folds <- function(dev) {
  fid <- read.csv(FOLD_PATH, colClasses = c("character", "character", "integer"))
  fold <- fid$fold[match(paste(dev$SERIALNO, dev$SPORDER), paste(fid$SERIALNO, fid$SPORDER))]
  stopifnot(!anyNA(fold))
  fold
}

# ---- metrics (inputs in $10k; reported in $) --------------------------------------
fold_metrics <- function(y, pred) {
  e <- (pred - y) * Y_SCALE
  c(MAE   = mean(abs(e)),
    RMSE  = sqrt(mean(e^2)),
    R2    = 1 - sum(e^2) / sum(((y - mean(y)) * Y_SCALE)^2),
    MedAE = median(abs(e)),                 # typical error for the middle person
    Bias  = mean(e),                        # + = over-predicts on average
    Within10k = mean(abs(e) <= 10000))      # share of people predicted within $10k
}

summarise_folds <- function(m) {          # m: one row per fold
  data.frame(metric = colnames(m), mean = colMeans(m),
             se = apply(m, 2, sd) / sqrt(nrow(m)), row.names = NULL)
}
