# =============================================================================
# make_latex_tables.R: turn Track B results into LaTeX tables for Section 3.
# Run after track_b_inference.R (and track_b_holdout.R, once 3(b) is done),
# from the repo root:
#   Rscript Inference/make_latex_tables.R [holdout_results_dir] [output.tex]
# Writes Inference/results/trackB_tables.tex (needs \usepackage{booktabs} and
# \usepackage{float}: [H] keeps each table exactly where it is placed).
# Table 2's holdout columns are filled from holdout_coefficients.csv when it
# exists, and left empty otherwise.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
hold_dir <- if (length(args) >= 1) args[1] else file.path("Inference", "results")
out      <- if (length(args) >= 2) args[2] else file.path("Inference", "results", "trackB_tables.tex")

tab <- read.csv(file.path("Inference", "results", "trackB_coefficients.csv"),
                stringsAsFactors = FALSE)
hold_csv  <- file.path(hold_dir, "holdout_coefficients.csv")
hold_meta <- file.path(hold_dir, "holdout_meta.csv")
has_hold  <- file.exists(hold_csv) && file.exists(hold_meta)
if (has_hold) {
  hold <- read.csv(hold_csv, stringsAsFactors = FALSE)
  meta <- read.csv(hold_meta)
  stopifnot(identical(hold$term, tab$term))
}
slope <- tab$term != "(Intercept)"
m <- sum(slope)

# rows shown in the report, in order; age is rescaled to "per 10 years"
rows <- list(
  list(head = NULL, terms = c("AGEP", "SEXFemale")),
  list(head = "VA rating (vs.\\ no rating)",
       terms = paste0("DRAT", c("0%", "10-20%", "30-40%", "50-60%", "70-100%", "Not reported"))),
  list(head = "Education (vs.\\ HS/GED)",
       terms = paste0("EDUC", c("No HS diploma", "Some college/associate", "Bachelor's",
                                "Master's", "Prof./doctorate"))))

label <- function(r) {
  if (r$term == "AGEP") return("Age (per 10 years)")
  if (r$term == "SEXFemale") return("Female (vs.\\ male)")
  l <- sub("Some college/associate", "Some college/assoc.", r$level, fixed = TRUE)
  l <- gsub("-", "--", l)
  l <- gsub("%", "\\\\%", l)
  paste0("\\quad ", l)
}
k   <- function(x, r) x * ifelse(r$term == "AGEP", 10, 1) / 1000        # $ thousands
num <- function(x) sprintf("$%.1f$", x)
ci  <- function(lo, hi) sprintf("$[%.1f,\\ %.1f]$", lo, hi)
pv  <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))

body2 <- body3 <- character()
for (g in rows) {
  if (!is.null(g$head)) {
    body2 <- c(body2, sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", g$head))
    body3 <- c(body3, sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", g$head))
  }
  for (t in g$terms) {
    r <- tab[tab$term == t, ]
    stopifnot(nrow(r) == 1)
    hcells <- " & & "
    if (has_hold) {
      hr <- hold[hold$term == t, ]
      hcells <- if (isTRUE(hr$h_estimable))
        sprintf("%s & %s & %s", num(k(hr$h_estimate, r)),
                ci(k(hr$h_ci_lo, r), k(hr$h_ci_hi, r)), pv(hr$h_p))
      else "\\multicolumn{3}{c}{n/a}"
    }
    body2 <- c(body2, sprintf("%s & %s & %s & %s & %s \\\\", label(r),
      num(k(r$estimate, r)), ci(k(r$ci_hc3_lo, r), k(r$ci_hc3_hi, r)), pv(r$p_hc3), hcells))
    body3 <- c(body3, sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", label(r),
      ci(k(r$ci_conv_lo, r), k(r$ci_conv_hi, r)), ci(k(r$ci_hc3_lo, r), k(r$ci_hc3_hi, r)),
      ci(k(r$ci_boot_lo, r), k(r$ci_boot_hi, r)), pv(r$p_hc3), pv(r$p_bonf), pv(r$p_bh)))
  }
}

s <- tab[slope, ]
n_sig <- c(conv = sum(s$sig_conv), hc3 = sum(s$sig_hc3), boot = sum(s$sig_boot),
           bonf = sum(s$sig_bonf), bh = sum(s$sig_bh))

big <- function(n) format(n, big.mark = "{,}", scientific = FALSE)
hold_n   <- if (has_hold) big(meta$n_total) else "5{,}809"
hold_sig <- if (!has_hold) "" else if (meta$m_estimable == m) as.character(meta$n_sig) else
  sprintf("%d (of %d estimable)", meta$n_sig, meta$m_estimable)
hold_note <- if (has_hold && meta$n_used < meta$n_total)
  sprintf(" In the holdout, %d veteran(s) in groups with fewer than 2 members are left out and those coefficients are not estimable (n/a).",
          meta$n_total - meta$n_used) else ""

table2 <- c(
  "\\begin{table}[H]",
  "\\centering\\small",
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{3}{c}{Development ($n = 23{,}233$)} & \\multicolumn{3}{c}{Holdout ($n = %s$)} \\\\", hold_n),
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  " & Estimate & 95\\% CI & $p$ & Estimate & 95\\% CI & $p$ \\\\",
  "\\midrule",
  body2,
  "\\midrule",
  sprintf("Significant slopes (of %d) & \\multicolumn{3}{c}{%d} & \\multicolumn{3}{c}{%s} \\\\",
          m, n_sig[["hc3"]], hold_sig),
  "\\bottomrule",
  "\\end{tabular}",
  paste0("\\caption{Key coefficients of the linear regression of annual earnings on the ",
         "records-only predictors (difference in earnings vs.\\ the reference group, ",
         "\\$ thousands, 2024), fit separately on each sample. CIs and $p$-values use HC3 ",
         "robust standard errors. Degree rows compare Business majors with HS/GED holders; ",
         "the model also includes race, marital status, state and field of degree.",
         hold_note, "}"),
  "\\label{tab:coef-dev-holdout}",
  "\\end{table}")

table3 <- c(
  "\\begin{table}[H]",
  "\\centering\\small",
  "\\setlength{\\tabcolsep}{3.5pt}",
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{95\\% confidence interval (\\$ thousands)} & \\multicolumn{3}{c}{$p$-value} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  " & Conventional & HC3 robust & Bootstrap & Unadjusted & Bonferroni & BH \\\\",
  "\\midrule",
  body3,
  "\\midrule",
  sprintf("Significant slopes (of %d) & %d & %d & %d & %d & %d & %d \\\\", m,
          n_sig[["conv"]], n_sig[["hc3"]], n_sig[["boot"]], n_sig[["hc3"]],
          n_sig[["bonf"]], n_sig[["bh"]]),
  "\\bottomrule",
  "\\end{tabular}",
  paste0("\\caption{Development-data uncertainty for the key coefficients. Bootstrap: ",
         "percentile intervals from 2,000 household resamples. Adjusted $p$-values ",
         "correct the HC3 $p$-values for all ", m, " slope tests (Bonferroni; ",
         "Benjamini--Hochberg, BH). Significant: CI excludes 0, or $p < 0.05$.}"),
  "\\label{tab:bootstrap-mht}",
  "\\end{table}")

writeLines(c("% Generated by Inference/make_latex_tables.R -- do not edit by hand", "",
             table2, "", table3), out)
cat(readLines(out), sep = "\n")
