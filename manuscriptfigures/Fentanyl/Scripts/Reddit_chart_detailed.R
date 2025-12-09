# Vector of required packages

# Read me ----------------------------------------------------------------
# November 14
# Reddit visualization only

# Libraries --------------------------------------------------------------
packages <- c(
  "tidyverse",
  "zoo",
  "ggtext",
  "readr"
)

install_if_missing <- packages[
  !(packages %in% installed.packages()[, "Package"])
]
if (length(install_if_missing)) {
  install.packages(install_if_missing)
}

lapply(packages, library, character.only = TRUE)

# Load data
data <- readRDS("/Fentanyl/Data/Compiled/compiled_data.rds")

unindexeddata <- read_csv("/Fentanyl/Data/Raw/aggregated_drought_ratios.csv")

glimpse(data$data_reddit)
summary(data$data_reddit)
glimpse(unindexeddata)
summary(unindexeddata)

# Indexing procedure explanation ----------------------------------------
# The indexing procedure transforms the KeywordRatio (proportion of posts mentioning drought)
# to a scale where the pre-treatment period mean = 100.
# Pre-treatment period: Jan 2021 - May 2023 (29 months)
# Treatment period: June 2023 onwards (sometime after fentanyl supply disruption began)
# Formula: KeywordRatio_index = (KeywordRatio / PrePeriodMean) * 100
# Control limits are also indexed using the same transformation
# This allows for easier visual comparison of deviations from baseline

# Define pre and post periods
pre_period_end <- as.Date("2023-05-31")

# Re-index data to pre-period mean ---------------------------------------
# Calculate pre-period mean
raw_pre <- unindexeddata %>%
  filter(Period <= pre_period_end)

raw_post <- unindexeddata %>%
  filter(Period > pre_period_end)

pre_period_mean <- mean(raw_pre$KeywordRatio, na.rm = TRUE)
pre_period_sd <- sd(raw_pre$KeywordRatio, na.rm = TRUE)

# Create re-indexed dataset
reindexed_data <- unindexeddata %>%
  mutate(
    KeywordRatio_index = (KeywordRatio / pre_period_mean) * 100,
    UpperControlLimit_index = (UpperControlLimit / pre_period_mean) * 100,
    LowerControlLimit_index = (LowerControlLimit / pre_period_mean) * 100,
    period = as.yearmon(Period)
  )

# Calculate frozen control limits
frozen_ucl <- pre_period_mean + (3 * pre_period_sd)
frozen_lcl <- max(0, pre_period_mean - (3 * pre_period_sd))
frozen_ucl_indexed <- (frozen_ucl / pre_period_mean) * 100
frozen_lcl_indexed <- (frozen_lcl / pre_period_mean) * 100

# Calculate statistics for re-indexed data
indexed_pre <- reindexed_data %>%
  filter(Period <= pre_period_end)

indexed_post <- reindexed_data %>%
  filter(Period > pre_period_end)

cat("\n=== INDEXED DATA (Scale: Pre-period mean = 100) ===\n")
cat("\nPre-period (Jan 2021 - May 2023):\n")
cat("  Mean:  ", round(mean(indexed_pre$KeywordRatio_index, na.rm = TRUE), 2), "\n")
cat("  Min:   ", round(min(indexed_pre$KeywordRatio_index, na.rm = TRUE), 2), "\n")
cat("  Max:   ", round(max(indexed_pre$KeywordRatio_index, na.rm = TRUE), 2), "\n")

cat("\nPost-period (May 2023 onwards - drought peaks):\n")
cat("  Mean:  ", round(mean(indexed_post$KeywordRatio_index, na.rm = TRUE), 2), "\n")
cat("  Min:   ", round(min(indexed_post$KeywordRatio_index, na.rm = TRUE), 2), "\n")
cat("  Max:   ", round(max(indexed_post$KeywordRatio_index, na.rm = TRUE), 2), "\n")

cat("\n=== RAW PROPORTION DATA ===\n")
cat("\nPre-period (Jan 2021 - May 2023):\n")
cat("  Mean:  ", format(mean(raw_pre$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")
cat("  Min:   ", format(min(raw_pre$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")
cat("  Max:   ", format(max(raw_pre$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")

cat("\nPost-period (May 2023 onwards - drought peaks):\n")
cat("  Mean:  ", format(mean(raw_post$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")
cat("  Min:   ", format(min(raw_post$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")
cat("  Max:   ", format(max(raw_post$KeywordRatio, na.rm = TRUE), scientific = TRUE), "\n")


cat("\n=== RAW PROPORTION DATA ===\n")
cat("\nPre-period (Jan 2021 - May 2023):\n")
cat("  Mean:  ", format(mean(raw_pre$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")
cat("  Min:   ", format(min(raw_pre$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")
cat("  Max:   ", format(max(raw_pre$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")

cat("\nPost-period (June 2023 onwards - drought peaks):\n")
cat("  Mean:  ", format(mean(raw_post$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")
cat("  Min:   ", format(min(raw_post$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")
cat("  Max:   ", format(max(raw_post$KeywordRatio, na.rm = TRUE) * 100, nsmall = 4), "%\n")

cat("\n=== FROZEN CONTROL LIMITS ===\n")
cat("\nIndexed (Scale: Pre-period mean = 100):\n")
cat("  Upper Control Limit (UCL):  ", round(frozen_ucl_indexed, 2), "\n")
cat("  Lower Control Limit (LCL):  ", round(frozen_lcl_indexed, 2), "\n")

cat("\nRaw Proportion:\n")
cat("  Upper Control Limit (UCL):  ", format(frozen_ucl, scientific = TRUE), 
    " (", format(frozen_ucl * 100, nsmall = 4), "%)\n", sep = "")
cat("  Lower Control Limit (LCL):  ", format(frozen_lcl, scientific = TRUE),
    " (", format(frozen_lcl * 100, nsmall = 4), "%)\n", sep = "")

cat("\n=== DATASET DESCRIPTION ===\n")
cat("\nPre-period (Jan 2021 - May 2023):\n")
cat("  drought posts in pre-period on r/fentanyl:  ", sum(raw_pre$DroughtPostCount))
cat("  total posts in pre-period on r/fentanyl:  ", sum(raw_pre$TotalPostCount))

cat("\nPost-period (June 2023 onwards - drought peaks):\n")
cat("  drought posts in post-period on r/fentanyl:  ", sum(raw_post$DroughtPostCount))
cat("  total posts in post-period on r/fentanyl:  ", sum(raw_post$TotalPostCount))

cat("\nWhole Time Period (Jan 2021 - December 2024):\n")
cat("  drought posts on r/fentanyl 2021 - 2024:  ", sum(raw_post$DroughtPostCount))
cat("  total posts on r/fentanyl 2021 - 2024:  ", sum(raw_post$TotalPostCount))



# Plot theme
plot_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_line(color = "grey90", size = 0.2),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.caption = ggtext::element_textbox(
        width = unit(0.95, "npc"),
        hjust = 0,
        halign = 0,
        size = 10
      )
    )
}

# Reddit Plot with cumulative control limits
index_year <- "May 2023"
ban1 <- "January 2024"
ban2 <- "October 2024"

plot_reddit <- reindexed_data |>
  ggplot(aes(period)) +
  geom_line(aes(y = KeywordRatio_index)) +
  geom_point(aes(y = KeywordRatio_index)) +
  geom_line(
    aes(y = UpperControlLimit_index),
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_line(
    aes(y = LowerControlLimit_index),
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "black"
  ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban1)),
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban2)),
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban1)),
    y = max(reindexed_data$KeywordRatio_index, na.rm = TRUE) * 0.80,
    label = str_wrap("First Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.5,
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban2)) - .12,
    y = max(reindexed_data$KeywordRatio_index, na.rm = TRUE) * 1,
    label = str_wrap("Second Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.3,
    color = "red"
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 2000, 500)),
    expand = expansion(mult = c(0.01, 0.2))
  ) +
  scale_x_yearmon(
    breaks = zoo::as.yearmon(seq.Date(
      from = as.Date(min(reindexed_data$period)),
      to = as.Date(max(reindexed_data$period)),
      by = "6 months"
    )),
    labels = function(x) {
      ifelse(
        format(x, "%m") == "07",
        paste("July", format(x, "\n%Y")),
        ifelse(format(x, "%m") == "01", paste("Jan", format(x, "\n%Y")), "")
      )
    },
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = str_wrap("Monthly Proportion of Posts Mentioning Drought", 55),
    x = NULL,
    y = paste0("Indexed to pre-period mean (Jan 2021 - May 2023)"),
    caption = paste0(
      "\nDashed vertical red lines indicate timing of Reddit bans on discussing \"drought\"",
      "\nDashed vertical red lines indicate timing of Reddit bans on discussing \"drought\"",
      "\nDashed horizontal black line indicates the rolling average throughout the pre-period, and is fixed to the average of the pre-period (Jan 2021-May 2023) throughout the post period (June 2023 onwards)",
      "\nDashed horizontal blue lines indicate cumulative control limits throughout the pre-period (±3 SD above and below the rolling mean Jan 2021 - May 2023)"
    )
  ) +
  plot_theme()

plot_reddit

plot_reddit_nt <- plot_reddit +
  labs(title = NULL)

# Create plot with frozen control limits --------------------------------
# This plot applies control limits calculated from pre-period (Jan 2021 - April 2023)
# to the entire time series, showing how post-period values deviate from baseline

plot_reddit_frozen <- reindexed_data |>
  ggplot(aes(period)) +
  geom_line(aes(y = KeywordRatio_index)) +
  geom_point(aes(y = KeywordRatio_index)) +
  geom_hline(
    yintercept = frozen_ucl_indexed,
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = frozen_lcl_indexed,
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "black"
  ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban1)),
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban2)),
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban1)),
    y = max(reindexed_data$KeywordRatio_index, na.rm = TRUE) * 0.80,
    label = str_wrap("First Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.5,
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban2)) - .12,
    y = max(reindexed_data$KeywordRatio_index, na.rm = TRUE) * 1,
    label = str_wrap("Second Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.3,
    color = "red"
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 2000, 500)),
    expand = expansion(mult = c(0.01, 0.2))
  ) +
  scale_x_yearmon(
    breaks = zoo::as.yearmon(seq.Date(
      from = as.Date(min(reindexed_data$period)),
      to = as.Date(max(reindexed_data$period)),
      by = "6 months"
    )),
    labels = function(x) {
      ifelse(
        format(x, "%m") == "07",
        paste("July", format(x, "\n%Y")),
        ifelse(format(x, "%m") == "01", paste("Jan", format(x, "\n%Y")), "")
      )
    },
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = str_wrap("Monthly Proportion of Posts Mentioning Drought (Frozen Control Limits)", 55),
    x = NULL,
    y = paste0("Indexed to pre-period mean (Jan 2021 - May 2023)"),
    caption = paste0(
      "\nDashed vertical red lines indicate timing of Reddit bans on discussing \"drought\"",
      "\nDashed horizontal blue lines indicate upper and lower control limits (±3 SD from pre-period mean)",
      "\nDashed black line indicates pre-period mean (Jan 2021 - May 2023)"
      
    )
  ) +
  plot_theme()

plot_reddit_frozen

plot_reddit_frozen_nt <- plot_reddit_frozen +
  labs(title = NULL)

# Save plots
plot_dir <- "/Fentanyl/Plots"
png_dir <- file.path(plot_dir, "PNG")
pdf_dir <- file.path(plot_dir, "PDF")

dir.create(plot_dir, showWarnings = FALSE)
dir.create(png_dir, showWarnings = FALSE)
dir.create(pdf_dir, showWarnings = FALSE)

plot_names <- c("plot_reddit", "plot_reddit_nt", "plot_reddit_frozen", "plot_reddit_frozen_nt")
for (nm in plot_names) {
  plot_obj <- get(nm)
  
  ggsave(
    file.path(png_dir, paste0(nm, ".png")),
    plot = plot_obj,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  ggsave(
    file.path(pdf_dir, paste0(nm, ".pdf")),
    plot = plot_obj,
    width = 8,
    height = 6
  )
}