# =============================================================================
# track_b_holdout.R: Section 3(b), the locked model fit on the HOLDOUT.
#
# Run ONLY after Inference/spec.R and the development results are committed.
# Run from the repo root:
#   Rscript Inference/track_b_holdout.R                  # finds holdout_raw.rds
#   Rscript Inference/track_b_holdout.R path/to/holdout_raw.rds
#   Rscript Inference/track_b_holdout.R --test file.rds  # dry run on any other file;
#                                  # writes to holdout_test_output/ next to that file
# In RStudio: set the working directory to the repo root, then
#   source("Inference/track_b_holdout.R")
#
# What it does:
#   1. Fits the model in spec.R, unchanged, on the 5,809 holdout veterans
#      (HC3 robust SEs; levels with < 2 veterans reported as not estimable).
#   2. Compares every coefficient with the development fit: the difference, its SE
#      sqrt(SE_dev^2 + SE_holdout^2) (independent samples), and changes in sign
#      or significance.
#   3. Robustness: refits without holdout households that also have a member in
#      the development data (the split was by person, not household).
#
# Outputs (Inference/results/):
#   holdout_coefficients.csv   every coefficient: dev vs. holdout, comparison, robustness
#   holdout_summary.txt        counts and key comparisons for the 3(b) write-up
#   holdout_meta.csv           sample sizes, read by make_latex_tables.R
# =============================================================================

suppressPackageStartupMessages(library(sandwich))
source(file.path("Inference", "spec.R"))

ALPHA <- 0.05
args  <- commandArgs(trailingOnly = TRUE)
test_mode <- length(args) && args[1] == "--test"
find_file <- function(name) {
  p <- c(file.path("data", name), name)
  p[file.exists(p)][1]
}
if (test_mode) {
  if (length(args) < 2) stop("--test needs a file path")
  hold_path <- args[2]
  OUT <- file.path(dirname(hold_path), "holdout_test_output")   # never Inference/results
} else {
  hold_path <- if (length(args)) args[1] else find_file("holdout_raw.rds")
  OUT <- file.path("Inference", "results")
}
dev_path <- find_file("dev_raw.rds")
dev_csv  <- file.path("Inference", "results", "trackB_coefficients.csv")
if (is.na(hold_path) || !file.exists(hold_path)) stop("holdout_raw.rds not found; pass its path")
if (is.na(dev_path)) stop("dev_raw.rds not found (needed for the shared-household check)")
if (!file.exists(dev_csv)) stop("Run Inference/track_b_inference.R first (needs ", dev_csv, ")")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- load -------------------------------------------------------------------------------
raw <- readRDS(hold_path)
if (!test_mode) stopifnot(nrow(raw) == 5809)   # guard: this must be the holdout file
if (test_mode) cat("TEST MODE: not the holdout. Outputs go to", OUT, "\n")
hold <- prep_inference(raw)
dev_ids <- unique(as.character(readRDS(dev_path)$SERIALNO))
shared <- hold$SERIALNO %in% dev_ids

# ---- 1. fit on the holdout ------------------------------------------------------------------
res <- fit_inference(hold, level = 1 - ALPHA)
h <- res$table
d <- read.csv(dev_csv, stringsAsFactors = FALSE)
stopifnot(identical(d$term, h$term))           # same model, same coefficients, same order

# ---- 2. compare with the development fit ----------------------------------------------------
cmp <- data.frame(
  term = h$term, variable = h$variable, level = h$level,
  dev_estimate = d$estimate, dev_se = d$se_hc3,
  dev_ci_lo = d$ci_hc3_lo, dev_ci_hi = d$ci_hc3_hi, dev_p = d$p_hc3,
  h_n_level = h$n_level, h_estimable = h$estimable,
  h_estimate = h$estimate, h_se = h$se_hc3,
  h_ci_lo = h$ci_hc3_lo, h_ci_hi = h$ci_hc3_hi, h_p = h$p_hc3,
  stringsAsFactors = FALSE)
cmp$dev_sig  <- cmp$dev_p < ALPHA
cmp$h_sig    <- cmp$h_estimable & cmp$h_p < ALPHA
cmp$diff     <- cmp$h_estimate - cmp$dev_estimate
cmp$se_diff  <- sqrt(cmp$dev_se^2 + cmp$h_se^2)
cmp$z_diff   <- cmp$diff / cmp$se_diff
cmp$diff_large <- cmp$h_estimable & abs(cmp$z_diff) > qnorm(1 - ALPHA / 2)
cmp$sign_flip  <- cmp$h_estimable & sign(cmp$h_estimate) != sign(cmp$dev_estimate)
cmp$sig_status <- ifelse(!cmp$h_estimable, "not estimable in holdout",
                  ifelse(cmp$dev_sig & cmp$h_sig, "both",
                  ifelse(cmp$dev_sig, "development only",
                  ifelse(cmp$h_sig, "holdout only", "neither"))))

# ---- 3. robustness: drop households shared with the development data ----------------------
n_shared_hh <- length(unique(hold$SERIALNO[shared]))
rob_ok <- sum(!shared) >= 100
if (rob_ok) {
  rob <- suppressMessages(fit_inference(hold[!shared, ], level = 1 - ALPHA))$table
  cmp$x_estimable <- rob$estimable
  cmp$x_estimate  <- rob$estimate
  cmp$x_ci_lo <- rob$ci_hc3_lo; cmp$x_ci_hi <- rob$ci_hc3_hi; cmp$x_p <- rob$p_hc3
  cmp$x_sig <- rob$estimable & rob$p_hc3 < ALPHA
}

# ---- save --------------------------------------------------------------------------------------
out <- cmp
num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
p_cols <- intersect(c("dev_p", "h_p", "x_p"), num_cols)
dollar_cols <- setdiff(num_cols, c(p_cols, "z_diff", "h_n_level"))
out[dollar_cols] <- lapply(out[dollar_cols], round, 2)   # cents: avoids double rounding later
out[p_cols] <- lapply(out[p_cols], signif, 3)
out$z_diff <- round(out$z_diff, 2)
write.csv(out, file.path(OUT, "holdout_coefficients.csv"), row.names = FALSE)

slope <- cmp$term != "(Intercept)"
s <- cmp[slope, ]
est <- s[s$h_estimable, ]
write.csv(data.frame(n_total = res$n_total, n_used = res$n_used,
                     n_households = length(unique(hold$SERIALNO)),
                     m = nrow(s), m_estimable = nrow(est), n_sig = sum(est$h_sig)),
          file.path(OUT, "holdout_meta.csv"), row.names = FALSE)

# ---- summary for the write-up -------------------------------------------------------------------
k   <- function(x) sprintf("%.1f", x / 1000)
lab <- function(r) paste0(r$variable, " = ", r$level)
listing <- function(rows, title) {
  if (!nrow(rows)) return(paste0(title, ": none"))
  c(paste0(title, ":"), paste0("  ", lab(rows)))
}
key <- s$variable %in% c("Age", "Sex", "VA rating", "Education")
kt <- s[key, ]
sc <- ifelse(kt$term == "AGEP", 10, 1)          # age shown per 10 years, as in Table 2
kl <- ifelse(kt$term == "AGEP", "Age = per 10 years", lab(kt))
key_lines <- sprintf("  %-38s dev %7s [%7s, %7s]   holdout %7s [%7s, %7s]   z_diff %5s",
  kl, k(sc * kt$dev_estimate), k(sc * kt$dev_ci_lo), k(sc * kt$dev_ci_hi),
  ifelse(kt$h_estimable, k(sc * kt$h_estimate), "n/a"),
  ifelse(kt$h_estimable, k(sc * kt$h_ci_lo), ""), ifelse(kt$h_estimable, k(sc * kt$h_ci_hi), ""),
  ifelse(kt$h_estimable, sprintf("%.2f", kt$z_diff), ""))
se_ratio <- round(quantile(est$h_se / est$dev_se, c(0, .25, .5, .75, 1)), 2)

lines <- c(
  if (test_mode) "*** TEST MODE: NOT THE HOLDOUT ***",
  "TRACK B 3(b) SUMMARY: locked model fit on the holdout",
  sprintf("Holdout: %d veterans in %d households; %d used in the fit; R^2 = %.3f",
          res$n_total, length(unique(hold$SERIALNO)), res$n_used, res$r2),
  sprintf("Not estimable (fewer than %d holdout veterans): %s", MIN_GROUP_N,
          if (length(res$not_estimable)) paste(lab(cmp[cmp$term %in% res$not_estimable, ]),
                                               collapse = "; ") else "none"),
  "",
  sprintf("Significant at %.2f (HC3): development %d of %d; holdout %d of %d estimable",
          ALPHA, sum(s$dev_sig), nrow(s), sum(est$h_sig), nrow(est)),
  sprintf("  significant in both: %d | development only: %d | holdout only: %d | neither: %d",
          sum(est$sig_status == "both"), sum(est$sig_status == "development only"),
          sum(est$sig_status == "holdout only"), sum(est$sig_status == "neither")),
  "  ratio of holdout SE to development SE (min, 25%, median, 75%, max):",
  paste0("    ", paste(se_ratio, collapse = ", ")),
  "",
  sprintf("Differences larger than the combined uncertainty (|z_diff| > 1.96): %d of %d",
          sum(est$diff_large), nrow(est)),
  sprintf("  (about %.0f would exceed this by chance alone)", ALPHA * nrow(est)),
  listing(est[est$diff_large, ], "  coefficients"),
  listing(est[est$sign_flip, ], "Sign changes"),
  listing(est[est$sig_status == "development only", ], "Significant in development only"),
  listing(est[est$sig_status == "holdout only", ], "Significant in holdout only"),
  "",
  "Key coefficients ($ thousands; HC3 95% CIs):",
  key_lines,
  "")
if (rob_ok) {
  xs <- s[s$x_estimable, ]
  changed <- s[s$h_estimable & s$x_estimable & s$h_sig != s$x_sig, ]
  lines <- c(lines,
    sprintf("Robustness: %d holdout veterans in %d households share a household with development veterans",
            sum(shared), n_shared_hh),
    sprintf("  refit without them: %d of %d estimable coefficients significant",
            sum(xs$x_sig), nrow(xs)),
    sprintf("  largest change in a key coefficient: %s thousand",
            k(max(abs(kt$x_estimate - kt$h_estimate), na.rm = TRUE))),
    listing(changed, "  significance changes vs. the full holdout fit"))
} else {
  lines <- c(lines, sprintf("Robustness check skipped: only %d veterans outside shared households",
                            sum(!shared)))
}
writeLines(lines, file.path(OUT, "holdout_summary.txt"))
cat(lines, sep = "\n")
cat("\nSaved results to", OUT, "\n")
