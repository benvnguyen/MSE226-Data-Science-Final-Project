# =============================================================================
# report_figs_2_3.R: Figures 2 and 3 of the report.
#   Fig 2 (Section 5(b)): association of each candidate covariate with earnings
#     (R^2 for numeric, eta^2 for categorical: both = share of earnings variance explained).
#   Fig 3 (Section 6(e)): cross-validated MAE by algorithm and feature set.
# DEVELOPMENT DATA ONLY. This script never reads the holdout file or the raw CSV.
# Run from the project root, after the model pipeline: source("Plots/report_figs_2_3.R")
# Saves Plots/project_part_1/fig2_covariates_vs_earnings.png and fig3_cv_mae.png.
# =============================================================================
library(ggplot2)
library(scales)
source(file.path("Models", "R", "data.R"))        # fod_family(): the 13 degree families the models use

d <- readRDS("data/dev_raw.rds")
stopifnot(nrow(d) == 23233)                       # guard: dev file only
y <- d$PERNP_adj

# ---- Fig 2: strength of association of each covariate with earnings ----
# Both measures = share of the variance in earnings explained by the covariate (0-1 scale):
#   numeric covariate      -> R^2 of a simple linear regression of earnings on it (= Pearson r^2)
#   categorical covariate  -> eta^2 = between-group SS / total SS (= R^2 of earnings on its category dummies)
eta <- function(f) { f <- factor(f); m <- ave(y, f); sum((m - mean(y))^2) / sum((y - mean(y))^2) }
fac <- function(x) { x <- as.character(x); x[is.na(x)] <- "N/A"; x }

num <- list("Weeks worked (WKWN)" = d$WKWN, "Hours per week (WKHP)" = d$WKHP,
            "Commute time (JWMNP)" = d$JWMNP, "Age (AGEP)" = d$AGEP)
cat_vars <- list(
  "Educational attainment (SCHL)"   = fac(d$SCHL),
  "Field of degree (FOD1P)"         = fod_family(d$FOD1P),
  "S&E degree flag (SCIENGRLP)"     = fac(d$SCIENGRLP),
  "Marital status (MAR)"            = fac(d$MAR),
  "Any disability (DIS)"            = fac(d$DIS),
  "Independent-living diff. (DOUT)" = fac(d$DOUT),
  "Cognitive difficulty (DREM)"     = fac(d$DREM),
  "Ambulatory difficulty (DPHY)"    = fac(d$DPHY),
  "Self-care difficulty (DDRS)"     = fac(d$DDRS),
  "Sex (SEX)"                       = fac(d$SEX),
  "Own children, women (PAOC)"      = fac(d$PAOC),
  "Race (RAC1P)"                    = fac(d$RAC1P),
  "VA disability rating (DRAT)"     = fac(d$DRAT),
  "State (STATE)"                   = fac(d$STATE),
  "Hearing difficulty (DEAR)"       = fac(d$DEAR),
  "Vision difficulty (DEYE)"        = fac(d$DEYE),
  "Has VA rating (DRATX)"           = fac(d$DRATX),
  "Place of birth (WAOB)"           = fac(d$WAOB),
  "English ability (ENG)"           = fac(d$ENG),
  "Other language (LANX)"           = fac(d$LANX),
  "Citizenship (CIT)"               = fac(d$CIT),
  "Nativity (NATIVITY)"             = fac(d$NATIVITY))
used <- c("AGEP", "SEX", "RAC1P", "MAR", "DRAT", "SCHL", "STATE", "FOD1P", "WKHP", "WKWN", "JWMNP")

f2 <- rbind(data.frame(var = names(num), value = sapply(num, function(v) cor(v, y)^2), measure = "r"),
            data.frame(var = names(cat_vars), value = sapply(cat_vars, eta), measure = "eta"))
f2$code <- sub(".*\\((.*)\\)", "\\1", f2$var)
f2$group <- ifelse(f2$code %in% c("WKHP", "WKWN", "JWMNP"), "Work schedule (large set only)",
            ifelse(f2$code %in% used, "Background (both feature sets)", "Not used in our models"))
f2$group <- factor(f2$group, levels = c("Work schedule (large set only)", "Background (both feature sets)", "Not used in our models"))
f2$var <- factor(f2$var, levels = f2$var[order(f2$value)])
f2$lab <- sprintf("%.3f (%s)", f2$value, ifelse(f2$measure == "r", "R²", "η²"))
stopifnot(nrow(f2) == 26)

p2 <- ggplot(f2, aes(value, var, fill = group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = lab), hjust = -0.1, size = 2.4, colour = "grey25") +
  scale_fill_manual(values = c("#d95f02", "#1b9e77", "grey75"), name = NULL) +
  scale_x_continuous(limits = c(0, 0.235), breaks = seq(0, 0.2, 0.05), expand = c(0, 0)) +
  labs(x = expression("Share of earnings variance explained: "*R^2*" (numeric) or "*eta^2*" (categorical)"), y = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = c(0.72, 0.18), legend.background = element_rect(fill = "white", colour = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 7.5))
ggsave("Plots/project_part_1/fig2_covariates_vs_earnings.png", p2, width = 5.2, height = 4.6, dpi = 300, bg = "white")
print(f2[order(-f2$value), c("code", "value", "measure", "group")], row.names = FALSE, digits = 3)

# ---- Fig 3: CV MAE by algorithm x feature set, from the official pipeline output ----
st <- read.csv(file.path("Models", "results", "summary_table.csv"))
base <- st$MAE[st$Features == "-"]
f3 <- st[st$Features != "-" & st$Model != "Random forest", c("Model", "Features", "MAE", "MAE_SE")]  # report omits the mean forest
f3$Model <- factor(sub("Linear regression \\(OLS\\)", "OLS", sub("Random forest \\(median\\)", "Median random forest", f3$Model)),
                   levels = c("OLS", "Neural network", "Median random forest"))
stopifnot(nrow(f3) == 6, !anyNA(f3$Model))
f3$Features <- factor(ifelse(f3$Features == "small", "Small: records only (D1)", "Large: records + work schedule (D2)"),
                      levels = c("Small: records only (D1)", "Large: records + work schedule (D2)"))

p3 <- ggplot(f3, aes(Model, MAE, fill = Features)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = MAE - MAE_SE, ymax = MAE + MAE_SE), position = position_dodge(0.8), width = 0.25, colour = "grey30") +
  geom_text(aes(y = MAE + MAE_SE, label = dollar(MAE, scale = 1e-3, accuracy = 0.1, suffix = "k")),
            position = position_dodge(0.8), vjust = -0.5, size = 2.5, colour = "grey20") +
  geom_hline(yintercept = base, linetype = "dashed", colour = "grey30") +
  annotate("text", x = 0.45, y = base, label = sprintf("Baseline MAE (predict the median for everyone): $%.1fk", base / 1e3),
           hjust = 0, vjust = -0.5, size = 2.6, colour = "grey30") +
  scale_fill_manual(values = c("#7570b3", "#d95f02"), name = NULL) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), limits = c(0, 50000), expand = c(0, 0)) +
  labs(x = NULL, y = "Cross-validated MAE (2024 $)") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave("Plots/project_part_1/fig3_cv_mae.png", p3, width = 4.6, height = 4.6, dpi = 300, bg = "white")
cat("saved fig2_covariates_vs_earnings.png and fig3_cv_mae.png in Plots/project_part_1/\n")
