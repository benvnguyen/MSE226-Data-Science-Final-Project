# =============================================================================
# report_fig_5b.R: Figure 1 of the report (Section 5(b)). Earnings by education,
# VA disability rating and usual weekly hours, in three panels on one scale.
# DEVELOPMENT DATA ONLY. This script never reads the holdout file or the raw CSV.
# Run from the project root:  source("Plots/report_fig_5b.R")
# Saves Plots/project_part_1/fig1_earnings_by_group.png. Needs only ggplot2 and scales.
# =============================================================================
library(ggplot2)
library(scales)

d <- readRDS("data/dev_raw.rds")
stopifnot(nrow(d) == 23233)                      # guard: dev file only
y <- d$PERNP_adj
s <- as.integer(as.character(d$SCHL))

edu <- cut(s, c(0, 15, 17, 20, 21, 22, 24),
           labels = c("No HS\ndiploma", "HS /\nGED", "Some coll.\n/ assoc.", "Bach-\nelor's", "Mas-\nter's", "Prof. /\ndoctor."))
va  <- ifelse(is.na(d$DRAT), "No rating",
       ifelse(d$DRAT == "6", "Not\nreported", ifelse(d$DRAT == "5", "70-100%", "0-60%")))
va  <- factor(va, levels = c("No rating", "0-60%", "70-100%", "Not\nreported"))
hrs <- cut(d$WKHP, c(-1, 0, 34, 39, 40, 49, 99),
           labels = c("0 (did not\nwork)", "1-34", "35-39", "40", "41-49", "50+"))

panel <- function(g, name) data.frame(panel = name, group = as.character(g), y = y, lev = as.integer(g))
long <- rbind(panel(edu, "(a) Educational attainment"),
              panel(va,  "(b) VA disability rating"),
              panel(hrs, "(c) Usual hours worked per week"))
long$panel <- factor(long$panel, levels = unique(long$panel))
long$key <- factor(paste(long$panel, long$group, sep = "|"),
                   levels = unique(paste(long$panel, long$group, sep = "|")[order(long$panel, long$lev)]))

lab <- aggregate(y ~ key + panel, long, function(v) c(n = length(v), med = median(v), zero = mean(v == 0)))
lab <- data.frame(key = lab$key, panel = lab$panel, lab$y)
lab$text <- ifelse(grepl("^\\(c\\)", lab$panel), sprintf("n=%s", comma(lab$n)),     # hours: $0 only at 0 h
                   sprintf("n=%s\n%.0f%% $0", comma(lab$n), 100 * lab$zero))

cap <- 355338                                    # 99th percentile on dev, so boxes stay readable
p <- ggplot(long, aes(key, y)) +
  geom_boxplot(outlier.shape = NA, fill = "#9ecae1", colour = "grey25", width = 0.65) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 1.6, fill = "white") +
  geom_label(data = lab, aes(key, cap * 0.93, label = text), size = 2.4, lineheight = 0.9, colour = "grey30",
             fill = "white", label.size = 0, label.padding = unit(0.08, "lines")) +
  facet_wrap(~ panel, scales = "free_x", nrow = 1) +
  scale_x_discrete(labels = function(k) sub(".*\\|", "", k)) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), breaks = seq(0, 300000, 100000)) +
  coord_cartesian(ylim = c(-5000, cap)) +
  labs(x = NULL, y = "Annual earnings (2024 $)") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", hjust = 0), panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 7.5))

ggsave("Plots/project_part_1/fig1_earnings_by_group.png", p, width = 10, height = 3.4, dpi = 300, bg = "white")
cat("saved Plots/project_part_1/fig1_earnings_by_group.png\n")
print(lab[, c("key", "n", "med", "zero")], row.names = FALSE)
