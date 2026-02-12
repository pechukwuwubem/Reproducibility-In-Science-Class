## =========================
## Coding Challenge 2 Plots
## =========================

library(ggplot2)
library(dplyr)

# ---- Load data ----
don_raw <- read.csv("MycotoxinData.csv", header = TRUE, stringsAsFactors = FALSE)

# ---- Helper: find a column by pattern (case-insensitive) ----
find_col <- function(df, patterns) {
  nms <- names(df)
  hit <- which(sapply(nms, function(nm) any(grepl(paste(patterns, collapse="|"), nm, ignore.case = TRUE))))
  if (length(hit) == 0) return(NA_character_)
  nms[hit[1]]
}

# ---- Identify likely column names ----
don_col <- find_col(don_raw, c("^don$", "deoxynivalenol"))
trt_col <- find_col(don_raw, c("^treat", "treatment", "trt", "group"))
cul_col <- find_col(don_raw, c("^cultivar$", "cult", "variety", "wheat"))

# If your dataset uses unexpected names, this will tell you what to change
if (is.na(don_col) || is.na(trt_col) || is.na(cul_col)) {
  stop(
    paste0(
      "Could not automatically find required columns.\n",
      "Detected:\n",
      "  DON column: ", don_col, "\n",
      "  treatment column: ", trt_col, "\n",
      "  cultivar column: ", cul_col, "\n\n",
      "Here are your column names:\n",
      paste(names(don_raw), collapse = ", ")
    )
  )
}

# ---- Standardize to expected names ----
don <- don_raw %>%
  rename(
    DON = all_of(don_col),
    treatment = all_of(trt_col),
    cultivar = all_of(cul_col)
  ) %>%
  mutate(
    DON = as.numeric(DON),
    treatment = as.factor(treatment),
    cultivar = as.factor(cultivar)
  )

# Optional: set treatment order (uncomment if you want a specific order)
# don$treatment <- factor(don$treatment, levels = c("NTC","Fg","Fg+40","Fg+70","Fg+37"))

# ---- Common position for points (jitter + dodge) ----
pd <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)

# =========================
# 2) BOX PLOT (4 pts)
# DON (y) vs treatment (x), color mapped to cultivar
# Add points (Q4), facet by cultivar (Q5)
# =========================
p_box <- ggplot(don, aes(x = treatment, y = DON, color = cultivar)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers since we overlay all points
  geom_point(
    aes(fill = cultivar),
    shape = 21,
    color = "black",
    position = pd
  ) +
  facet_wrap(~cultivar) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic()

print(p_box)

# =========================
# 3) BAR CHART with SE via stat_summary (4 pts)
# mean bars + mean_se error bars, position = dodge
# Add points (Q4), facet by cultivar (Q5)
# =========================
p_bar <- ggplot(don, aes(x = treatment, y = DON, fill = cultivar)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.75)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = position_dodge(width = 0.75)
  ) +
  geom_point(
    aes(fill = cultivar),
    shape = 21,
    color = "black",
    position = pd
  ) +
  facet_wrap(~cultivar) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic()

print(p_bar)






