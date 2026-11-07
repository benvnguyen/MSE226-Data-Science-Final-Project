# =============================================================================
# Section 5(b)-(e): association tables, covariate heatmap, subgroups, missingness
# DEVELOPMENT DATA ONLY. This script never reads holdout_raw.rds or the raw CSV.
#
# Run from the project root:  source("Plots/eda_section5.R")
# Needs only ggplot2 and scales. Prints tables to the console; saves
# Plots/5c_association_heatmap.png.
# =============================================================================

library(ggplot2)
library(scales)

d <- readRDS("data/dev_raw.rds")
stopifnot(nrow(d) == 23233)            # guard: make sure this is the dev file
y <- d$PERNP_adj

# ---- variable groups ---------------------------------------------------------
num_vars <- c("AGEP", "WKHP", "WKWN", "JWMNP")
cat_vars <- c("STATE", "SEX", "RAC1P", "MAR", "PAOC", "CIT", "NATIVITY", "LANX", "ENG", "WAOB",
              "DIS", "DEAR", "DEYE", "DPHY", "DREM", "DDRS", "DOUT", "DRATX", "DRAT",
              "SCHL", "FOD1P", "SCIENGRLP")

# Categorical as factors, with structural NA as its own level
fac <- function(x) { x <- as.character(x); x[is.na(x)] <- "N/A"; factor(x) }
cats <- lapply(d[cat_vars], fac)

# Ordinal numeric versions (for Spearman)
schl_num <- as.integer(d$SCHL)                                  # 1..24
drat_num <- ifelse(is.na(d$DRAT), 0, as.integer(d$DRAT))        # 0 = no rating, 1..5 = rating band
drat_num[drat_num == 6] <- NA                                   # 6 = not reported

# ---- helpers -----------------------------------------------------------------
eta2 <- function(v, f) {                    # share of variance in v explained by groups f
  ok <- !is.na(v); v <- v[ok]; f <- droplevels(f[ok])
  m <- ave(v, f); sum((m - mean(v))^2) / sum((v - mean(v))^2)
}
cramers_v <- function(a, b) {
  t <- table(a, b); t <- t[rowSums(t) > 0, colSums(t) > 0, drop = FALSE]
  chi <- suppressWarnings(chisq.test(t, correct = FALSE)$statistic)
  unname(sqrt(chi / (sum(t) * (min(dim(t)) - 1))))
}
spear <- function(a, b) cor(a, b, method = "spearman", use = "complete.obs")
r_y <- rank(y)

# =============================================================================
# 5(b) Association of every covariate with earnings
# =============================================================================
cat("\n================ 5(b) OUTCOME ASSOCIATIONS ================\n")
cat("Numeric / ordinal: Pearson r, Spearman rho (all, and workers only WKHP > 0)\n")
work <- d$WKHP > 0
num_tab <- do.call(rbind, lapply(list(AGEP = d$AGEP, WKHP = d$WKHP, WKWN = d$WKWN, JWMNP = d$JWMNP,
                                      SCHL_ord = schl_num, DRAT_ord = drat_num), function(v)
  data.frame(pearson = cor(v, y, use = "complete.obs"), spearman = spear(v, y),
             spearman_workers = spear(v[work], y[work]))))
print(round(num_tab, 3))

cat("\nAll covariates ranked by eta^2 (share of earnings variance explained by the variable's groups).\n")
cat("eta2_rank = same thing on rank(earnings), robust to the long tail. Numeric vars binned into deciles.\n")
binned <- lapply(d[num_vars], function(v) factor(findInterval(v, unique(quantile(v, 0:10 / 10)))))
all_f <- c(cats, binned)
eta_tab <- data.frame(var = names(all_f),
                      eta2 = sapply(all_f, eta2, v = y),
                      eta2_rank = sapply(all_f, eta2, v = r_y),
                      levels = sapply(all_f, nlevels))
eta_tab <- eta_tab[order(-eta_tab$eta2_rank), ]
print(format(eta_tab, digits = 3), row.names = FALSE)

cat("\nMedian / mean earnings and share with $0, by group (key categoricals)\n")
grp_summary <- function(f, name) {
  s <- do.call(rbind, lapply(split(y, f), function(v)
    data.frame(n = length(v), median = median(v), mean = mean(v), pct_zero = mean(v == 0))))
  cat("--", name, "\n"); print(format(s, big.mark = ",", digits = 3))
}
for (v in c("SEX", "MAR", "DRAT", "DIS", "SCHL", "RAC1P", "CIT", "LANX")) grp_summary(cats[[v]], v)

cat("\nHours nuance: share of people at exactly 40 hours; earnings by hours band (workers)\n")
cat("Share WKHP == 40:", round(mean(d$WKHP == 40), 3), " among workers:", round(mean(d$WKHP[work] == 40), 3), "\n")
band <- cut(d$WKHP, c(-1, 0, 19, 34, 39, 40, 49, 59, 99),
            labels = c("0 (no work)", "1-19", "20-34", "35-39", "40", "41-49", "50-59", "60+"))
grp_summary(band, "WKHP band")
cat("Zero earners:", sum(y == 0), " of which WKHP == 0:", sum(y == 0 & d$WKHP == 0), "\n")
cat("Full-year full-time (WKWN >= 50 & WKHP >= 35) share:", round(mean(d$WKWN >= 50 & d$WKHP >= 35), 3), "\n")

# =============================================================================
# 5(c) Covariate-covariate associations
# =============================================================================
cat("\n================ 5(c) COVARIATE ASSOCIATIONS ================\n")
cat("Measure: |Spearman| for num-num, Cramer's V for cat-cat, eta (sqrt eta^2) for num-cat.\n")
# Use the grouped FOD1P (15 top fields + Other + none) so 170 sparse levels don't inflate V
top15 <- names(sort(table(d$FOD1P), decreasing = TRUE))[1:15]
fod_g <- ifelse(is.na(d$FOD1P), "none", ifelse(d$FOD1P %in% top15, d$FOD1P, "other"))
cats$FOD1P <- factor(fod_g)
vars <- c(num_vars, cat_vars)
nums <- d[num_vars]
assoc <- function(a, b) {
  an <- a %in% num_vars; bn <- b %in% num_vars
  if (an && bn) return(abs(spear(nums[[a]], nums[[b]])))
  if (an) return(sqrt(eta2(nums[[a]], cats[[b]])))
  if (bn) return(sqrt(eta2(nums[[b]], cats[[a]])))
  cramers_v(cats[[a]], cats[[b]])
}
M <- matrix(NA, length(vars), length(vars), dimnames = list(vars, vars))
for (i in seq_along(vars)) for (j in seq_len(i)) M[i, j] <- M[j, i] <- if (i == j) 1 else assoc(vars[i], vars[j])

pairs <- which(upper.tri(M), arr.ind = TRUE)
pt <- data.frame(a = vars[pairs[, 1]], b = vars[pairs[, 2]], assoc = M[pairs])
pt <- pt[order(-pt$assoc), ]
cat("\nTop 30 most associated covariate pairs:\n"); print(format(head(pt, 30), digits = 3), row.names = FALSE)
cat("\nSigned Spearman among numeric + ordinal:\n")
ordm <- cbind(nums, SCHL_ord = schl_num, DRAT_ord = drat_num)
print(round(cor(ordm, method = "spearman", use = "pairwise.complete.obs"), 3))

# Heatmap (lower triangle)
ord <- vars
hm <- expand.grid(x = ord, y = ord, stringsAsFactors = FALSE)
hm$v <- M[cbind(hm$y, hm$x)]
hm <- hm[match(hm$x, ord) < match(hm$y, ord), ]
hm$x <- factor(hm$x, levels = ord); hm$y <- factor(hm$y, levels = rev(ord))
p <- ggplot(hm, aes(x, y, fill = v)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(v >= 0.3, sprintf("%.2f", v), "")), size = 2.6) +
  scale_fill_gradient(low = "#F7FBFF", high = "#08306B", limits = c(0, 1), name = "Association") +
  labs(title = "Association between covariates (dev data)",
       subtitle = "|Spearman| (numeric-numeric), Cramer's V (categorical-categorical), eta (numeric-categorical). Values >= 0.30 labelled.",
       x = NULL, y = NULL,
       caption = sprintf("Dev data (n = %s). Structural N/A kept as its own category. FOD1P grouped to top 15 fields + other + none.",
                         comma(nrow(d)))) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"), plot.caption = element_text(colour = "grey40", hjust = 0))
ggsave(file.path("Plots", "5c_association_heatmap.png"), p, width = 11, height = 9.5, dpi = 150, bg = "white")
if (interactive()) print(p)

cat("\nAge by education / rating (num-cat checks):\n")
print(round(tapply(d$AGEP, cut(schl_num, c(0, 15, 17, 20, 21, 24), labels = c("<HS", "HS/GED", "Some coll/AA", "BA", "Grad")), mean), 1))
print(round(tapply(d$AGEP, cats$DRAT, mean), 1))
cat("Share with any disability (DIS=1) by DRAT:\n"); print(round(tapply(d$DIS == "1", cats$DRAT, mean), 3))
cat("Share working (WKHP>0) by DRAT:\n"); print(round(tapply(work, cats$DRAT, mean), 3))
cat("Share working by DIS:\n"); print(round(tapply(work, cats$DIS, mean), 3))
cat("Share female by MAR:\n"); print(round(tapply(d$SEX == "2", cats$MAR, mean), 3))

# =============================================================================
# 5(d) Subgroups
# =============================================================================
cat("\n================ 5(d) SUBGROUPS ================\n")
profile <- function(g, name) {
  s <- do.call(rbind, lapply(split(seq_len(nrow(d)), g), function(i) data.frame(
    n = length(i), med_earn = median(y[i]), mean_earn = mean(y[i]), pct_zero = mean(y[i] == 0),
    mean_age = mean(d$AGEP[i]), pct_female = mean(d$SEX[i] == "2"),
    pct_BA_plus = mean(schl_num[i] >= 21), pct_any_dis = mean(d$DIS[i] == "1"),
    pct_working = mean(d$WKHP[i] > 0), mean_hrs_workers = mean(d$WKHP[i][d$WKHP[i] > 0]),
    pct_married = mean(d$MAR[i] == "1"))))
  cat("--", name, "\n"); print(format(s, digits = 3, big.mark = ","))
}
drat_g <- with(d, ifelse(is.na(DRAT), "1 No rating", ifelse(DRAT == "6", "4 Not reported",
               ifelse(DRAT %in% c("1", "2", "3", "4"), "2 Rated 0-60%", "3 Rated 70-100%"))))
profile(drat_g, "VA rating group")
profile(ifelse(d$SEX == "1", "Male", "Female"), "Sex")
profile(ifelse(schl_num >= 21, "BA or higher", "Below BA"), "Education")
profile(ifelse(grepl("GQ", d$SERIALNO), "Group quarters", "Household"), "Living situation")

# =============================================================================
# 5(e) Missingness
# =============================================================================
cat("\n================ 5(e) MISSINGNESS ================\n")
na <- sapply(d, function(x) mean(is.na(x))); print(round(na[na > 0], 3))
cat("DRAT 'not reported' (code 6, real item nonresponse):", sum(d$DRAT == "6", na.rm = TRUE),
    sprintf("(%.1f%% of rated)\n", 100 * mean(d$DRAT[!is.na(d$DRAT)] == "6")))
cat("Workers with JWMNP == 0 (work from home):", sum(work & d$JWMNP == 0),
    sprintf("(%.1f%% of workers)\n", 100 * mean(d$JWMNP[work] == 0)))
cat("Median earnings, workers WFH vs commuters:",
    median(y[work & d$JWMNP == 0]), "vs", median(y[work & d$JWMNP > 0]), "\n")
