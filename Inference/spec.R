# =============================================================================
# spec.R: the LOCKED Section 3 regression specification.
#
# Chosen on development data only, before the holdout was used for inference.
# Track B fits it on the development data (3(a), 3(c), 3(d)); Track A fits the
# SAME specification, unchanged, on the holdout (3(b)). Do not edit after the
# holdout fit has been run.
#
#   Outcome:    annual earnings, 2024 dollars (PERNP_adj); $0 for non-workers.
#   Predictors: Part 1 records-only ("small") feature set:
#               age, sex, race, marital status, VA disability rating,
#               education (6 groups), state, field of degree (13 families).
#   Model:      ordinary least squares, inference with HC3 robust SEs (Lecture 9).
#
# Coding mirrors Models/R/data.R on main (same FOD1P families, same DRAT labels),
# except education is grouped into the 6 levels of Part 1's Figure 1.
#
# Reference (omitted) levels, so each coefficient is a difference from:
#   SEX = Male; RAC1P = White; MAR = Married; DRAT = No rating;
#   EDUC = HS/GED; STATE = TX (most dev veterans); FIELD = Business (largest field).
# Field of degree is only defined for bachelor's holders, so field dummies are 0
# for everyone else. A field coefficient compares that field with Business majors
# at the same education level; the Bachelor's / Master's / Prof. coefficients
# compare Business majors at that level with HS/GED holders.
# =============================================================================

# ---- field of degree: 170 majors -> 13 families (copied from Models/R/data.R) ----
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
FOD_FAMILY_EXCEPT <- c(
  "4000" = "Humanities", "4001" = "Social sciences", "4002" = "Health & nutrition",
  "4005" = "Computing, math & data", "4006" = "Psychology & social services",
  "4007" = "Social sciences", "4009" = "Computing, math & data")
FOD_FAMILIES <- c("No bachelor's", names(FOD_FAMILY_PREFIX))
FIELD_REF    <- "Business"

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

# ---- education: 24 SCHL codes -> 6 groups (reference first) ----------------------
EDUC_LEVELS <- c("HS/GED", "No HS diploma", "Some college/associate",
                 "Bachelor's", "Master's", "Prof./doctorate")
educ_group <- function(schl) {
  s <- as.integer(schl)
  g <- ifelse(s <= 15, "No HS diploma",
       ifelse(s <= 17, "HS/GED",
       ifelse(s <= 20, "Some college/associate",
       ifelse(s == 21, "Bachelor's",
       ifelse(s == 22, "Master's", "Prof./doctorate")))))
  factor(g, levels = EDUC_LEVELS)
}

# ---- state FIPS -> postal code; Texas first so it is the reference ---------------
STATE_FIPS <- c("01"="AL","02"="AK","04"="AZ","05"="AR","06"="CA","08"="CO","09"="CT",
  "10"="DE","11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL","18"="IN",
  "19"="IA","20"="KS","21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI",
  "27"="MN","28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ",
  "35"="NM","36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR","42"="PA",
  "44"="RI","45"="SC","46"="SD","47"="TN","48"="TX","49"="UT","50"="VT","51"="VA",
  "53"="WA","54"="WV","55"="WI","56"="WY")
STATE_LEVELS <- c("TX", setdiff(unname(STATE_FIPS), "TX"))

# ---- prep_inference(): raw PUMS rows -> analysis data (fixed rules, nothing learned) --
prep_inference <- function(raw) {
  stopifnot(all(raw$MIL == "2"), all(raw$VPSP == "01"))
  out <- data.frame(
    SERIALNO = raw$SERIALNO,
    SPORDER  = as.character(raw$SPORDER),
    earnings = as.numeric(raw$PERNP_adj),
    AGEP     = as.numeric(raw$AGEP),
    SEX      = factor(raw$SEX, levels = c("1", "2"), labels = c("Male", "Female")),
    RAC1P    = factor(raw$RAC1P, levels = as.character(1:9),
                      labels = c("White", "Black", "American Indian", "Alaska Native",
                                 "AIAN, tribe unspecified", "Asian", "Pacific Islander",
                                 "Other race", "Two or more races")),
    MAR      = factor(raw$MAR, levels = as.character(1:5),
                      labels = c("Married", "Widowed", "Divorced", "Separated", "Never married")),
    DRAT     = factor(ifelse(is.na(raw$DRAT), "none", raw$DRAT),
                      levels = c("none", "1", "2", "3", "4", "5", "6"),
                      labels = c("No rating", "0%", "10-20%", "30-40%", "50-60%",
                                 "70-100%", "Not reported")),
    EDUC     = educ_group(raw$SCHL),
    STATE    = factor(unname(STATE_FIPS[raw$STATE]), levels = STATE_LEVELS),
    FIELD    = fod_family(raw$FOD1P),
    stringsAsFactors = FALSE)
  vars <- c("earnings", "AGEP", "SEX", "RAC1P", "MAR", "DRAT", "EDUC", "STATE", "FIELD")
  if (anyNA(out[vars])) stop("Missing values after recoding: ",
                             paste(vars[colSums(is.na(out[vars])) > 0], collapse = ", "))
  out
}

# ---- build_X(): design matrix (intercept + dummies, reference levels omitted) ----
build_X <- function(df) {
  X <- model.matrix(~ AGEP + SEX + RAC1P + MAR + DRAT + EDUC + STATE, data = df)
  fams <- setdiff(names(FOD_FAMILY_PREFIX), FIELD_REF)
  Fd <- vapply(fams, function(f) as.numeric(df$FIELD == f), numeric(nrow(df)))
  colnames(Fd) <- paste0("FIELD", fams)
  cbind(X, Fd)
}

# ---- readable labels for each column of build_X() ---------------------------------
term_labels <- function(terms) {
  vars <- c(SEX = "Sex", RAC1P = "Race", MAR = "Marital status", DRAT = "VA rating",
            EDUC = "Education", STATE = "State", FIELD = "Field of degree")
  variable <- rep(NA_character_, length(terms)); level <- terms
  variable[terms == "(Intercept)"] <- "Intercept"; level[terms == "(Intercept)"] <- ""
  variable[terms == "AGEP"] <- "Age"; level[terms == "AGEP"] <- "per year"
  for (v in names(vars)) {
    hit <- startsWith(terms, v) & is.na(variable)
    variable[hit] <- vars[[v]]
    level[hit] <- substring(terms[hit], nchar(v) + 1)
  }
  data.frame(term = terms, variable = variable, level = level, stringsAsFactors = FALSE)
}

# ---- fit_inference(): OLS fit + conventional and HC3 robust inference -------------
# Returns the lm fit and a table with estimates, conventional SEs/CIs/p-values (the
# default regression output) and HC3 robust SEs/CIs/p-values (Lecture 9), using the
# large-sample normal approximation for the robust test (Lecture 11).
#
# Small groups (matters for the holdout, never for the development data, where
# every level has 17+ veterans): a category level with fewer than MIN_GROUP_N
# veterans in the sample is reported as NOT ESTIMABLE (NA). With 0 veterans its
# coefficient does not exist; with 1 veteran that person fully determines it, so
# their HC3 weight 1/(1 - leverage)^2 divides by zero and would turn EVERY robust
# SE into NaN. Those veterans are left out of the fit. The model is unchanged.
MIN_GROUP_N <- 2

fit_inference <- function(df, level = 0.95, min_n = MIN_GROUP_N) {
  X_full <- build_X(df); y_full <- df$earnings
  dummies <- setdiff(colnames(X_full), c("(Intercept)", "AGEP"))
  keep_row <- rep(TRUE, nrow(X_full)); small <- character()
  repeat {                                     # repeat: dropping a person can shrink another level
    n_lev <- colSums(X_full[keep_row, dummies, drop = FALSE])
    new_small <- setdiff(names(n_lev)[n_lev < min_n], small)
    if (!length(new_small)) break
    small <- c(small, new_small)
    keep_row <- keep_row & rowSums(X_full[, small, drop = FALSE]) == 0
  }
  n_level <- setNames(rep(NA_real_, ncol(X_full)), colnames(X_full))
  n_level[dummies] <- colSums(X_full[keep_row, dummies, drop = FALSE])
  n_level[small] <- colSums(X_full[, small, drop = FALSE])   # report the sample count
  X <- X_full[keep_row, setdiff(colnames(X_full), small), drop = FALSE]
  y <- y_full[keep_row]

  fit <- lm(y ~ 0 + X)
  names(fit$coefficients) <- colnames(X)
  if (anyNA(coef(fit))) stop("Aliased coefficients: ",
                             paste(colnames(X)[is.na(coef(fit))], collapse = ", "))
  if (max(hatvalues(fit)) > 1 - 1e-8) stop("A veteran has leverage 1; robust SEs undefined")
  z <- qnorm(1 - (1 - level) / 2)
  est <- coef(fit)
  conv <- summary(fit)$coefficients
  ci_conv <- suppressMessages(confint(fit, level = level))
  se_hc3 <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))
  part <- data.frame(
    term     = colnames(X),
    estimate = unname(est),
    se_conv  = unname(conv[, 2]),
    ci_conv_lo = unname(ci_conv[, 1]), ci_conv_hi = unname(ci_conv[, 2]),
    p_conv   = unname(conv[, 4]),
    se_hc3   = unname(se_hc3),
    ci_hc3_lo = unname(est - z * se_hc3), ci_hc3_hi = unname(est + z * se_hc3),
    p_hc3    = unname(2 * pnorm(-abs(est / se_hc3))),
    stringsAsFactors = FALSE)
  # one row per coefficient of the full model, NA where not estimable
  tab <- data.frame(term_labels(colnames(X_full)),
                    n_level = unname(n_level),
                    estimable = !(colnames(X_full) %in% small),
                    stringsAsFactors = FALSE)
  tab <- cbind(tab, part[match(tab$term, part$term), -1])
  rownames(tab) <- NULL
  if (length(small)) message("Not estimable (fewer than ", min_n, " veterans): ",
                             paste(small, collapse = ", "), "; ", sum(!keep_row),
                             " veteran(s) left out of the fit")
  # R^2 by hand: the intercept is a column of X, so summary(fit)$r.squared would be
  # measured against 0 instead of mean earnings and come out far too high.
  r2 <- 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  list(fit = fit, X = X, y = y, table = tab, r2 = r2,
       not_estimable = small, n_used = sum(keep_row), n_total = nrow(X_full))
}
