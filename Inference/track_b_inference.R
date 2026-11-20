# =============================================================================
# track_b_inference.R: Section 3(a), 3(c), 3(d), on DEVELOPMENT data only.
#
# Run from the repo root:   Rscript Inference/track_b_inference.R [B]
#   B = number of bootstrap resamples (default 2000).
#
# 3(a) Fit the locked model (Inference/spec.R) on the development data; report
#      conventional and HC3 robust SEs, CIs and p-values.
# 3(c) Household (block) bootstrap, Lecture 10 Example 4: resample households with
#      replacement, keep every veteran in a sampled household, refit OLS. Report
#      bootstrap SEs, percentile CIs (and normal CIs as a check).
# 3(d) Bonferroni and Benjamini-Hochberg adjustments of the HC3 p-values
#      (all slope coefficients; the intercept is not tested).
#
# Outputs (Inference/results/):
#   trackB_coefficients.csv      every coefficient, all three kinds of CI, adjusted p
#   trackB_key_coefficients.csv  the rows discussed in the report
#   trackB_summary.txt           counts and comparisons quoted in the report
#   trackB_ci_comparison.png     figure: three CIs for the key coefficients
#   trackB_boot_draws.rds        bootstrap draws (for re-use; not committed)
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(ggplot2) })
source(file.path("Inference", "spec.R"))

ALPHA <- 0.05
SEED  <- 32
args  <- commandArgs(trailingOnly = TRUE)
B     <- if (length(args)) as.integer(args[1]) else 2000L
OUT   <- file.path("Inference", "results")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- development data only ----------------------------------------------------------
dev_path <- c(file.path("data", "dev_raw.rds"), "dev_raw.rds")
dev_path <- dev_path[file.exists(dev_path)][1]
if (is.na(dev_path)) stop("dev_raw.rds not found (looked in data/ and the repo root)")
raw <- readRDS(dev_path)
stopifnot(nrow(raw) == 23233)                 # guard: this must be the development file
dev <- prep_inference(raw)

# ---- 3(a): fit on development data ----------------------------------------------------
res <- fit_inference(dev, level = 1 - ALPHA)
tab <- res$table
X <- res$X; y <- res$y
stopifnot(identical(tab$term, colnames(X)))   # dev data: every coefficient estimable
cat(sprintf("3(a) OLS on %d veterans, %d coefficients (%d slopes), R^2 = %.3f\n",
            nrow(X), ncol(X), ncol(X) - 1, res$r2))

# ---- 3(c): household block bootstrap --------------------------------------------------
set.seed(SEED)
rows_by_hh <- split(seq_len(nrow(dev)), dev$SERIALNO)
n_hh <- length(rows_by_hh)
draws <- matrix(NA_real_, B, ncol(X), dimnames = list(NULL, colnames(X)))
t0 <- Sys.time()
for (b in seq_len(B)) {
  idx <- unlist(rows_by_hh[sample.int(n_hh, n_hh, replace = TRUE)], use.names = FALSE)
  draws[b, ] <- lm.fit(X[idx, , drop = FALSE], y[idx])$coefficients
  if (b %% 250 == 0) cat(sprintf("  bootstrap %d / %d (%.1f min)\n", b, B,
                                 as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
saveRDS(list(draws = draws, seed = SEED, B = B, n_households = n_hh),
        file.path(OUT, "trackB_boot_draws.rds"))

z <- qnorm(1 - ALPHA / 2)
tab$boot_na    <- colSums(is.na(draws))   # resamples where a level was absent
tab$se_boot    <- apply(draws, 2, sd, na.rm = TRUE)
tab$ci_boot_lo <- apply(draws, 2, quantile, ALPHA / 2, na.rm = TRUE, names = FALSE)
tab$ci_boot_hi <- apply(draws, 2, quantile, 1 - ALPHA / 2, na.rm = TRUE, names = FALSE)
tab$ci_bootnorm_lo <- tab$estimate - z * tab$se_boot
tab$ci_bootnorm_hi <- tab$estimate + z * tab$se_boot

excludes0 <- function(lo, hi) lo > 0 | hi < 0
tab$sig_conv <- tab$p_conv < ALPHA
tab$sig_hc3  <- tab$p_hc3 < ALPHA
tab$sig_boot <- excludes0(tab$ci_boot_lo, tab$ci_boot_hi)

# ---- 3(d): multiple testing on the HC3 p-values ----------------------------------------
slope <- tab$term != "(Intercept)"
m <- sum(slope)
tab$p_bonf <- NA_real_; tab$p_bh <- NA_real_
tab$p_bonf[slope] <- p.adjust(tab$p_hc3[slope], "bonferroni")
tab$p_bh[slope]   <- p.adjust(tab$p_hc3[slope], "BH")
tab$sig_bonf <- !is.na(tab$p_bonf) & tab$p_bonf < ALPHA
tab$sig_bh   <- !is.na(tab$p_bh) & tab$p_bh < ALPHA

# ---- save tables ----------------------------------------------------------------------
dollar_cols <- c("estimate", grep("^(se|ci)_", names(tab), value = TRUE))
out <- tab
out[dollar_cols] <- lapply(out[dollar_cols], round)
p_cols <- grep("^p_", names(out), value = TRUE)
out[p_cols] <- lapply(out[p_cols], signif, 3)
write.csv(out, file.path(OUT, "trackB_coefficients.csv"), row.names = FALSE)

key <- tab$variable %in% c("Age", "Sex", "VA rating", "Education")
write.csv(out[key, ], file.path(OUT, "trackB_key_coefficients.csv"), row.names = FALSE)

# ---- summary for the report -------------------------------------------------------------
s <- tab[slope, ]
s$one <- 1
ratio <- function(a, b) round(quantile(a / b, c(0, .25, .5, .75, 1)), 2)
by_var <- aggregate(cbind(n = one, hc3 = sig_hc3, bonf = sig_bonf, bh = sig_bh) ~ variable,
                    data = s, FUN = sum)
by_var <- by_var[match(unique(s$variable), by_var$variable), ]
flip <- function(a, b, lab) {
  d <- s[a != b, c("variable", "level")]
  if (!nrow(d)) return(paste0(lab, ": none"))
  c(paste0(lab, ":"), paste0("  ", d$variable, " = ", d$level))
}
lines <- c(
  "TRACK B SUMMARY (development data only)",
  sprintf("n = %d veterans in %d households; %d slope coefficients; R^2 = %.3f",
          nrow(dev), n_hh, m, res$r2),
  sprintf("Significance level alpha = %.2f; bootstrap B = %d, seed = %d, unit = household",
          ALPHA, B, SEED),
  "",
  "3(a) Significant slopes (p < alpha):",
  sprintf("  conventional SEs: %d of %d", sum(s$sig_conv), m),
  sprintf("  HC3 robust SEs:   %d of %d", sum(s$sig_hc3), m),
  "",
  "3(c) Bootstrap vs. regression-output intervals:",
  sprintf("  bootstrap percentile CI excludes 0: %d of %d", sum(s$sig_boot), m),
  "  ratio of bootstrap SE to conventional SE (min, 25%, median, 75%, max):",
  paste0("    ", paste(ratio(s$se_boot, s$se_conv), collapse = ", ")),
  "  ratio of bootstrap SE to HC3 SE (min, 25%, median, 75%, max):",
  paste0("    ", paste(ratio(s$se_boot, s$se_hc3), collapse = ", ")),
  sprintf("  resamples with an absent level (max over coefficients): %d", max(s$boot_na)),
  flip(s$sig_conv, s$sig_boot, "  significance differs, conventional vs. bootstrap"),
  flip(s$sig_hc3, s$sig_boot, "  significance differs, HC3 vs. bootstrap"),
  "",
  sprintf("3(d) Multiple testing on HC3 p-values (m = %d tests):", m),
  sprintf("  Bonferroni cutoff alpha/m = %.6f", ALPHA / m),
  sprintf("  significant: unadjusted %d, Bonferroni %d, Benjamini-Hochberg %d",
          sum(s$sig_hc3), sum(s$sig_bonf), sum(s$sig_bh)),
  flip(s$sig_hc3, s$sig_bonf, "  lose significance under Bonferroni"),
  flip(s$sig_hc3, s$sig_bh, "  lose significance under Benjamini-Hochberg"),
  "",
  "Significant slopes by variable (HC3 / Bonferroni / BH, out of n):",
  sprintf("  %-16s %2d of %2d / %2d / %2d", by_var$variable, by_var$hc3, by_var$n,
          by_var$bonf, by_var$bh))
writeLines(lines, file.path(OUT, "trackB_summary.txt"))
cat(lines, sep = "\n")

# ---- figure: three CIs for the key coefficients -------------------------------------------
fk <- tab[key & tab$variable != "Age", ]
fk$label <- factor(paste0(fk$variable, ": ", fk$level), levels = rev(paste0(fk$variable, ": ", fk$level)))
long <- rbind(
  data.frame(label = fk$label, est = fk$estimate, lo = fk$ci_conv_lo, hi = fk$ci_conv_hi,
             type = "Conventional"),
  data.frame(label = fk$label, est = fk$estimate, lo = fk$ci_hc3_lo, hi = fk$ci_hc3_hi,
             type = "HC3 robust"),
  data.frame(label = fk$label, est = fk$estimate, lo = fk$ci_boot_lo, hi = fk$ci_boot_hi,
             type = "Bootstrap (percentile)"))
long$type <- factor(long$type, levels = c("Conventional", "HC3 robust", "Bootstrap (percentile)"))
p <- ggplot(long, aes(x = est / 1000, y = label, colour = type)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo / 1000, xmax = hi / 1000),
                  position = position_dodge(width = 0.6), size = 0.25) +
  scale_colour_manual(values = c("#7570B3", "#1B9E77", "#D95F02"), name = NULL) +
  labs(x = "Difference in annual earnings vs. reference group ($ thousands, 2024)", y = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(OUT, "trackB_ci_comparison.png"), p, width = 7, height = 4.5, dpi = 200)

writeLines(capture.output(sessionInfo()), file.path(OUT, "trackB_sessionInfo.txt"))
cat("\nSaved results to", OUT, "\n")
