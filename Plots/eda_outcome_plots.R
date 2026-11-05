# =============================================================================
# Section 5(b): every candidate covariate vs. earnings (PERNP_adj)
# DEVELOPMENT DATA ONLY. This script never reads holdout_raw.rds or the raw CSV.
#
# Run from the project root:  source("Plots/eda_outcome_plots.R")
# (in RStudio: Session > Set Working Directory > Choose Directory > Final Project).
# Needs only ggplot2 and scales.
# Output: 20 PNGs in Plots/, next to this script. In RStudio they also show in the Plots pane.
#
# Codes and labels: 2024 ACS PUMS Data Dictionary.
# =============================================================================

library(ggplot2)
library(scales)

d <- readRDS("dev_raw.rds")
stopifnot(nrow(d) == 23233)            # guard: make sure this is the dev file
y <- d$PERNP_adj

out_dir <- "Plots"
dir.create(out_dir, showWarnings = FALSE)

# ---- shared look ------------------------------------------------------------
y_hi <- unname(quantile(y, 0.99))      # clip y-axis at the 99th percentile for readability
y_lo <- -10000                         # PERNP bottom-code
y_lim <- coord_cartesian(ylim = c(y_lo, y_hi))
y_scale <- scale_y_continuous(labels = label_dollar(), name = "Earnings (2024 $)")
cap_box <- sprintf(paste0("Dev data (n = %s). Y-axis clipped at the 99th percentile (%s); ",
                          "means and boxes use all data.\nBox = middle 50%%, line = median, ",
                          "diamond = mean."), comma(length(y)), dollar(y_hi))
cap_num <- sprintf(paste0("Dev data (n = %s). Y-axis clipped at the 99th percentile (%s). ",
                          "Shading = number of people (log scale)."), comma(length(y)), dollar(y_hi))

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
           "#D55E00", "#CC79A7", "#882255", "#999999")   # colorblind-safe
na_grey <- "#BDBDBD"

theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  plot.caption = element_text(colour = "grey40", hjust = 0),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank()))

save_plot <- function(p, file, w = 9, h = 5.5) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 150, bg = "white")
  if (interactive()) print(p)
}

wrap <- function(s, width = 14) vapply(s, function(x) paste(strwrap(x, width), collapse = "\n"), "")

# Recode a raw code vector into a labelled factor. NA can get its own label.
recode <- function(x, map, na_label = NULL) {
  x <- as.character(x)
  if (!is.null(na_label)) { x[is.na(x)] <- "NA_"; map <- c(map, NA_ = na_label) }
  factor(unname(map[x]), levels = unname(map))
}

# ---- boxplot for a categorical covariate -------------------------------------
cat_plot <- function(f, title, xlab, file, colours = NULL, legend = TRUE,
                     order_by_median = FALSE, angle = 0, wrap_width = 14, w = 9) {
  f <- droplevels(f)
  if (order_by_median) f <- reorder(f, y, FUN = median)
  n <- table(f)
  axis_lab <- paste0(wrap(levels(f), wrap_width), "\n(n=", comma(as.vector(n)), ")")
  if (angle != 0) axis_lab <- levels(f)                       # crowded axes: no n, no wrap
  df <- data.frame(f = f, y = y)
  df$x <- factor(as.integer(df$f), labels = axis_lab)
  if (is.null(colours)) colours <- rep(okabe[5], nlevels(f))
  names(colours) <- levels(f)

  p <- ggplot(df, aes(x, y, fill = f)) +
    geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.85) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.3, fill = "white") +
    scale_fill_manual(values = colours, name = xlab) +
    y_scale + y_lim +
    labs(title = title, x = xlab, caption = cap_box)
  if (!legend) p <- p + theme(legend.position = "none")
  if (angle != 0) p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1))
  save_plot(p, file, w = w)
}

# ---- binned scatter for a numeric covariate ----------------------------------
num_plot <- function(var, title, xlab, file, bw, subtitle = NULL) {
  df <- data.frame(x = d[[var]], y = y)
  bin <- floor(df$x / bw) * bw + bw / 2
  m <- aggregate(y ~ bin, data = data.frame(bin, y = df$y), FUN = mean)
  m$n <- as.vector(table(bin))
  m <- m[m$n >= 30, ]                                          # only bins with enough people
  p <- ggplot(df, aes(x, y)) +
    geom_bin2d(binwidth = c(bw, (y_hi - y_lo) / 40)) +
    scale_fill_viridis_c(name = "People", trans = "log10", labels = comma, option = "mako",
                         direction = -1) +
    geom_line(data = m, aes(bin, y, colour = "Mean earnings"), inherit.aes = FALSE,
              linewidth = 1.1) +
    geom_point(data = m, aes(bin, y, colour = "Mean earnings"), inherit.aes = FALSE, size = 1.4) +
    scale_colour_manual(values = c("Mean earnings" = "#D55E00"), name = NULL) +
    y_scale + y_lim +
    labs(title = title, subtitle = subtitle, x = xlab, caption = cap_num)
  save_plot(p, file)
}

# =============================================================================
# Categorical covariates (16 plots)
# =============================================================================

# 01 SEX
cat_plot(recode(d$SEX, c("1" = "Male", "2" = "Female")),
         "Earnings by sex", "Sex", "01_SEX.png", colours = okabe[c(5, 6)], w = 7)

# 02 RAC1P
cat_plot(recode(d$RAC1P, c("1" = "White", "2" = "Black", "3" = "American Indian",
                           "4" = "Alaska Native", "5" = "AIAN, tribe specified",
                           "6" = "Asian", "7" = "Native Hawaiian / Pac. Islander",
                           "8" = "Some other race", "9" = "Two or more races")),
         "Earnings by race", "Race (RAC1P)", "02_RAC1P.png", colours = okabe, w = 11)

# 03 MAR
cat_plot(recode(d$MAR, c("1" = "Married", "2" = "Widowed", "3" = "Divorced",
                         "4" = "Separated", "5" = "Never married")),
         "Earnings by marital status", "Marital status (MAR)", "03_MAR.png", colours = okabe[1:5])

# 04 PAOC (women only; N/A = men and group quarters)
cat_plot(recode(d$PAOC, c("1" = "Children under 6 only", "2" = "Children 6-17 only",
                          "3" = "Children under 6 and 6-17", "4" = "No own children"),
                na_label = "N/A (male or group quarters)"),
         "Earnings by presence of own children (women only)", "Own children (PAOC)",
         "04_PAOC.png", colours = c(okabe[1:4], na_grey))

# 05 STATE (ordered by median)
state_abbr <- c("01"="AL","02"="AK","04"="AZ","05"="AR","06"="CA","08"="CO","09"="CT","10"="DE",
                "11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL","18"="IN","19"="IA",
                "20"="KS","21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI","27"="MN",
                "28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ","35"="NM",
                "36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR","42"="PA","44"="RI",
                "45"="SC","46"="SD","47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA",
                "54"="WV","55"="WI","56"="WY")
cat_plot(recode(d$STATE, state_abbr),
         "Earnings by state (ordered by median)", "State", "05_STATE.png",
         legend = FALSE, order_by_median = TRUE, angle = 90, w = 13)

# 06 CIT
cat_plot(recode(d$CIT, c("1" = "Born in the US", "2" = "Born in PR / US islands",
                         "3" = "Born abroad to US parent(s)", "4" = "Naturalized citizen",
                         "5" = "Not a citizen")),
         "Earnings by citizenship", "Citizenship (CIT)", "06_CIT.png", colours = okabe[1:5])

# 07 NATIVITY
cat_plot(recode(d$NATIVITY, c("1" = "Native-born", "2" = "Foreign-born")),
         "Earnings by nativity", "Nativity", "07_NATIVITY.png", colours = okabe[c(3, 7)], w = 7)

# 08 WAOB
cat_plot(recode(d$WAOB, c("1" = "US state", "2" = "PR / US islands", "3" = "Latin America",
                          "4" = "Asia", "5" = "Europe", "6" = "Africa",
                          "7" = "Northern America", "8" = "Oceania / at sea")),
         "Earnings by world area of birth", "World area of birth (WAOB)", "08_WAOB.png",
         colours = okabe[1:8], w = 11)

# 09 LANX
cat_plot(recode(d$LANX, c("1" = "Speaks another language", "2" = "English only")),
         "Earnings by language spoken at home", "Language at home (LANX)", "09_LANX.png",
         colours = okabe[c(1, 2)], w = 7)

# 10 ENG (N/A = speaks only English)
cat_plot(recode(d$ENG, c("1" = "Very well", "2" = "Well", "3" = "Not well", "4" = "Not at all"),
                na_label = "Speaks only English"),
         "Earnings by English-speaking ability", "Ability to speak English (ENG)", "10_ENG.png",
         colours = c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7", na_grey))

# 11 DIS
cat_plot(recode(d$DIS, c("1" = "With a disability", "2" = "Without a disability")),
         "Earnings by disability status", "Disability (DIS)", "11_DIS.png",
         colours = okabe[c(6, 3)], w = 7)

# 12 Six difficulty types (faceted)
dvars <- c(DEAR = "Hearing", DEYE = "Vision", DPHY = "Ambulatory",
           DREM = "Cognitive", DDRS = "Self-care", DOUT = "Independent living")
long <- do.call(rbind, lapply(names(dvars), function(v) {
  yes_n <- sum(d[[v]] == "1")
  data.frame(type = sprintf("%s (%s)\nYes: n=%s", dvars[[v]], v, comma(yes_n)),
             has = factor(ifelse(d[[v]] == "1", "Yes", "No"), levels = c("Yes", "No")),
             y = y)
}))
long$type <- factor(long$type, levels = unique(long$type))
p12 <- ggplot(long, aes(has, y, fill = has)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.1, fill = "white") +
  facet_wrap(~type, nrow = 1) +
  scale_fill_manual(values = c(Yes = okabe[6], No = okabe[3]), name = "Has this difficulty") +
  y_scale + y_lim +
  labs(title = "Earnings by type of difficulty", x = "Has difficulty", caption = cap_box)
save_plot(p12, "12_difficulties.png", w = 12)

# 13 DRAT (+ DRATX: "No rating" = DRATX = No)
cat_plot(recode(d$DRAT, c("1" = "0%", "2" = "10-20%", "3" = "30-40%", "4" = "50-60%",
                          "5" = "70-100%", "6" = "Rating not reported"),
                na_label = "No service-connected rating"),
         "Earnings by VA service-connected disability rating",
         "VA disability rating (DRAT; 'No rating' = DRATX No)", "13_DRAT.png",
         colours = c("#FDD0A2", "#FDAE6B", "#FD8D3C", "#E6550D", "#A63603", na_grey, okabe[2]),
         w = 11)

# 14 SCHL (collapsed to 11 ordered groups)
schl_grp <- with(d, {
  s <- as.integer(SCHL)
  g <- ifelse(s <= 14, "01-14", sprintf("%02d", s))
  recode(g, c("01-14" = "Grade 11 or less", "15" = "12th grade, no diploma",
              "16" = "HS diploma", "17" = "GED", "18" = "Some college (<1 yr)",
              "19" = "1+ yrs college, no degree", "20" = "Associate's", "21" = "Bachelor's",
              "22" = "Master's", "23" = "Professional", "24" = "Doctorate"))
})
cat_plot(schl_grp, "Earnings by educational attainment", "Highest education (SCHL)",
         "14_SCHL.png", colours = hcl.colors(11, "Viridis"), legend = FALSE,
         wrap_width = 11, w = 13)

# 15 FOD1P (15 most common fields; others grouped)
fod_top <- c("6203" = "Business Mgmt & Admin", "5301" = "Criminal Justice & Fire Prot.",
             "6200" = "General Business", "5200" = "Psychology", "5506" = "Political Science",
             "6107" = "Nursing", "3600" = "Biology", "2102" = "Computer Science",
             "6402" = "History", "6201" = "Accounting", "2100" = "Computer & Info Systems",
             "2414" = "Mechanical Engineering", "6207" = "Finance",
             "4101" = "Physical Fitness & Recreation", "2408" = "Electrical Engineering")
fod <- ifelse(is.na(d$FOD1P), "NONE", ifelse(d$FOD1P %in% names(fod_top), d$FOD1P, "OTHER"))
fod_f <- recode(fod, c(fod_top, OTHER = "Other field (155 fields)", NONE = "No bachelor's degree"))
fod_f <- reorder(fod_f, y, FUN = median)
fod_cols <- setNames(rep(okabe[5], nlevels(fod_f)), levels(fod_f))
fod_cols[c("Other field (155 fields)", "No bachelor's degree")] <- c(okabe[9], na_grey)
cat_plot(fod_f, "Earnings by field of first degree (ordered by median)",
         "Field of degree (FOD1P)", "15_FOD1P.png", colours = unname(fod_cols),
         legend = FALSE, angle = 45, w = 13)

# 16 SCIENGRLP
cat_plot(recode(d$SCIENGRLP, c("1" = "Science/engineering-related", "2" = "Not S&E-related"),
                na_label = "No bachelor's degree"),
         "Earnings by science & engineering-related degree", "S&E-related degree (SCIENGRLP)",
         "16_SCIENGRLP.png", colours = c(okabe[3], okabe[1], na_grey), w = 8)

# =============================================================================
# Numeric covariates (4 plots)
# =============================================================================
num_plot("AGEP", "Earnings by age", "Age (years)", "17_AGEP.png", bw = 2)
num_plot("WKHP", "Earnings by usual hours worked per week", "Usual hours per week (WKHP)",
         "18_WKHP.png", bw = 5, subtitle = "0 = did not work in the past 12 months")
num_plot("WKWN", "Earnings by weeks worked in the past 12 months", "Weeks worked (WKWN)",
         "19_WKWN.png", bw = 4, subtitle = "0 = did not work in the past 12 months")
num_plot("JWMNP", "Earnings by commute time", "One-way commute (minutes, JWMNP)",
         "20_JWMNP.png", bw = 10,
         subtitle = "0 = not a worker OR works from home (both coded 0)")

message("Saved 20 plots to ", normalizePath(out_dir))
