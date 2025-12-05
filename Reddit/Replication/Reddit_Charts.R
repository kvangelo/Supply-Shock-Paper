
setwd("G:/My Drive/Fentanyl Research/Supply shock paper/Science submission/Final_Revision")
#   
# To switch between unfrozen and frozen control limits:
#   
#   control_type <- "all" for unfrozen (cumulative mean/SD)
#   control_type <- "fixed" for frozen after July 2023
# 
# 
# Run the script and it will create all 10 charts (5 keywords × 2 chart types) and save them as PNG files
# 
# Load required libraries
library(tidyverse)
library(scales)

# Read in the data
keyword_data <- read_csv("fentanyl_reddit_keyword_control_data.csv")

# List of keywords to plot
keywords <- c("droughtmain", "droughtvariants", "drought", "shortage", "out_of_stock")

# Toggle: Set to "all" for unfrozen, "fixed" for frozen after July 2023
control_type <- "all"  # Change to "fixed" to freeze mean/SD after July 2023

# ============================================================================
# SECTION 1: CREATE CONTROL CHARTS
# ============================================================================

# Function to create control chart for counts
plot_count_control_chart <- function(data, keyword, type = "all") {
  
  # Create column names based on type
  mean_col <- paste0("cumulative_mean_", keyword, "_", type)
  sd_col <- paste0("cumulative_sd_", keyword, "_", type)
  ucl_col <- paste0("ucl_", keyword, "_", type)
  lcl_col <- paste0("lcl_", keyword, "_", type)
  outlier_col <- paste0("outlier_", keyword, "_", type)
  count_col <- paste0("n_", keyword)
  
  # Create the plot
  p <- ggplot(data, aes(x = month)) +
    geom_line(aes(y = get(count_col)), color = "black", size = 0.8) +
    geom_point(aes(y = get(count_col), color = get(outlier_col)), size = 2) +
    geom_line(aes(y = get(mean_col)), color = "blue", linetype = "dashed", size = 0.7) +
    geom_line(aes(y = get(ucl_col)), color = "red", linetype = "dotted", size = 0.7) +
    geom_line(aes(y = get(lcl_col)), color = "red", linetype = "dotted", size = 0.7) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                       labels = c("FALSE" = "In Control", "TRUE" = "Outlier"),
                       name = "") +
    labs(title = paste("Control Chart: Count of", keyword),
         subtitle = paste("Control limits:", ifelse(type == "all", "Unfrozen (cumulative)", "Frozen after July 2023")),
         x = "Month",
         y = "Count of Posts") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  return(p)
}

# Function to create control chart for proportions
plot_proportion_control_chart <- function(data, keyword, type = "all") {
  
  # Create column names
  ratio_col <- paste0("ratio_", keyword, "_to_total")
  mean_col <- paste0("cumulative_mean_", keyword, "_", type)
  sd_col <- paste0("cumulative_sd_", keyword, "_", type)
  ucl_col <- paste0("ucl_", keyword, "_", type)
  lcl_col <- paste0("lcl_", keyword, "_", type)
  outlier_col <- paste0("outlier_", keyword, "_", type)
  count_col <- paste0("n_", keyword)
  
  # Calculate proportion control limits
  data <- data %>%
    mutate(
      prop_mean = get(mean_col) / n_total_posts,
      prop_ucl = get(ucl_col) / n_total_posts,
      prop_lcl = get(lcl_col) / n_total_posts,
      # Outlier based on count, not proportion
      is_outlier = get(outlier_col)
    )
  
  # Create the plot
  p <- ggplot(data, aes(x = month)) +
    geom_line(aes(y = get(ratio_col)), color = "black", size = 0.8) +
    geom_point(aes(y = get(ratio_col), color = is_outlier), size = 2) +
    geom_line(aes(y = prop_mean), color = "blue", linetype = "dashed", size = 0.7) +
    geom_line(aes(y = prop_ucl), color = "red", linetype = "dotted", size = 0.7) +
    geom_line(aes(y = prop_lcl), color = "red", linetype = "dotted", size = 0.7) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                       labels = c("FALSE" = "In Control", "TRUE" = "Outlier"),
                       name = "") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste("Control Chart: Proportion of", keyword),
         subtitle = paste("Control limits:", ifelse(type == "all", "Unfrozen (cumulative)", "Frozen after July 2023")),
         x = "Month",
         y = "Proportion of Total Posts") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  return(p)
}

cat("\n=== CREATING CONTROL CHARTS ===\n\n")

# Create all plots
for (kw in keywords) {
  
  # Count control chart
  p_count <- plot_count_control_chart(keyword_data, kw, type = control_type)
  print(p_count)
  
  # Save count chart
  ggsave(filename = paste0("monthly post/control_chart_count_", kw, "_", control_type, ".png"),
         plot = p_count, width = 10, height = 6, dpi = 300)
  
  # Proportion control chart
  p_prop <- plot_proportion_control_chart(keyword_data, kw, type = control_type)
  print(p_prop)
  
  # Save proportion chart
  ggsave(filename = paste0("monthly post/control_chart_proportion_", kw, "_", control_type, ".png"),
         plot = p_prop, width = 10, height = 6, dpi = 300)
  
  cat("✓ Created charts for", kw, "\n")
}

cat("\n=== ALL CONTROL CHARTS CREATED ===\n")
cat("Control type:", control_type, "\n")
cat("Total charts created:", length(keywords) * 2, "\n\n")

# ============================================================================
# SECTION 2: SUMMARY STATISTICS AND STATISTICAL TESTS
# ============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("                        SUMMARY STATISTICS BY KEYWORD                           \n")
cat("================================================================================\n\n")

# Define cutoff date for pre/post comparison
cutoff_date <- as.Date("2023-07-01")

# Add period indicator to data
keyword_data <- keyword_data %>%
  mutate(period = ifelse(month < cutoff_date, "Pre-July 2023", "Post-July 2023"))

# Create summary table for all keywords
summary_results <- list()

for (kw in keywords) {
  
  cat("--------------------------------------------------------------------------------\n")
  cat("KEYWORD:", toupper(kw), "\n")
  cat("--------------------------------------------------------------------------------\n\n")
  
  # Get column names
  count_col <- paste0("n_", kw)
  ratio_col <- paste0("ratio_", kw, "_to_total")
  
  # Overall summary statistics
  cat("OVERALL STATISTICS:\n")
  cat("  Min count:       ", min(keyword_data[[count_col]]), "\n")
  cat("  Max count:       ", max(keyword_data[[count_col]]), "\n")
  cat("  Mean count:      ", round(mean(keyword_data[[count_col]]), 2), "\n")
  cat("  Median count:    ", median(keyword_data[[count_col]]), "\n")
  cat("  SD count:        ", round(sd(keyword_data[[count_col]]), 2), "\n\n")
  
  cat("  Min proportion:  ", round(min(keyword_data[[ratio_col]]) * 100, 3), "%\n")
  cat("  Max proportion:  ", round(max(keyword_data[[ratio_col]]) * 100, 3), "%\n")
  cat("  Mean proportion: ", round(mean(keyword_data[[ratio_col]]) * 100, 3), "%\n")
  cat("  Median proportion:", round(median(keyword_data[[ratio_col]]) * 100, 3), "%\n\n")
  
  # Pre/Post July 2023 comparison
  pre_data <- keyword_data %>% filter(period == "Pre-July 2023")
  post_data <- keyword_data %>% filter(period == "Post-July 2023")
  
  cat("PRE-JULY 2023 (n =", nrow(pre_data), "months):\n")
  cat("  Mean count:      ", round(mean(pre_data[[count_col]]), 2), "\n")
  cat("  SD count:        ", round(sd(pre_data[[count_col]]), 2), "\n")
  cat("  Mean proportion: ", round(mean(pre_data[[ratio_col]]) * 100, 3), "%\n")
  cat("  SD proportion:   ", round(sd(pre_data[[ratio_col]]) * 100, 3), "%\n\n")
  
  cat("POST-JULY 2023 (n =", nrow(post_data), "months):\n")
  cat("  Mean count:      ", round(mean(post_data[[count_col]]), 2), "\n")
  cat("  SD count:        ", round(sd(post_data[[count_col]]), 2), "\n")
  cat("  Mean proportion: ", round(mean(post_data[[ratio_col]]) * 100, 3), "%\n")
  cat("  SD proportion:   ", round(sd(post_data[[ratio_col]]) * 100, 3), "%\n\n")
  
  # Welch's t-test for counts
  cat("WELCH'S T-TEST (counts):\n")
  cat("  H0: No difference in mean counts before vs. after July 2023\n")
  t_test_count <- t.test(pre_data[[count_col]], post_data[[count_col]])
  cat("  t-statistic:     ", round(t_test_count$statistic, 3), "\n")
  cat("  df:              ", round(t_test_count$parameter, 2), "\n")
  cat("  p-value:         ", format.pval(t_test_count$p.value, digits = 3), "\n")
  cat("  95% CI:          [", round(t_test_count$conf.int[1], 2), ", ", 
      round(t_test_count$conf.int[2], 2), "]\n")
  cat("  Result:          ", ifelse(t_test_count$p.value < 0.05, 
                                    "SIGNIFICANT discontinuity detected (p < 0.05)", 
                                    "No significant discontinuity (p >= 0.05)"), "\n\n")
  
  # Welch's t-test for proportions
  cat("WELCH'S T-TEST (proportions):\n")
  cat("  H0: No difference in mean proportions before vs. after July 2023\n")
  t_test_prop <- t.test(pre_data[[ratio_col]], post_data[[ratio_col]])
  cat("  t-statistic:     ", round(t_test_prop$statistic, 3), "\n")
  cat("  df:              ", round(t_test_prop$parameter, 2), "\n")
  cat("  p-value:         ", format.pval(t_test_prop$p.value, digits = 3), "\n")
  cat("  95% CI:          [", round(t_test_prop$conf.int[1] * 100, 4), "%, ", 
      round(t_test_prop$conf.int[2] * 100, 4), "%]\n")
  cat("  Result:          ", ifelse(t_test_prop$p.value < 0.05, 
                                    "SIGNIFICANT discontinuity detected (p < 0.05)", 
                                    "No significant discontinuity (p >= 0.05)"), "\n\n")
  
  # Store results in a tibble for optional export
  summary_results[[kw]] <- tibble(
    keyword = kw,
    min_count = min(keyword_data[[count_col]]),
    max_count = max(keyword_data[[count_col]]),
    mean_count = mean(keyword_data[[count_col]]),
    min_proportion = min(keyword_data[[ratio_col]]) * 100,
    max_proportion = max(keyword_data[[ratio_col]]) * 100,
    mean_proportion = mean(keyword_data[[ratio_col]]) * 100,
    pre_mean_count = mean(pre_data[[count_col]]),
    post_mean_count = mean(post_data[[count_col]]),
    pre_mean_proportion = mean(pre_data[[ratio_col]]) * 100,
    post_mean_proportion = mean(post_data[[ratio_col]]) * 100,
    t_stat_count = t_test_count$statistic,
    p_value_count = t_test_count$p.value,
    t_stat_proportion = t_test_prop$statistic,
    p_value_proportion = t_test_prop$p.value,
    significant_count = t_test_count$p.value < 0.05,
    significant_proportion = t_test_prop$p.value < 0.05
  )
}

cat("================================================================================\n")
cat("                           END OF SUMMARY STATISTICS                            \n")
cat("================================================================================\n\n")

# Combine all summary results into one dataframe and save
summary_df <- bind_rows(summary_results)
write_csv(summary_df, "monthly post/keyword_summary_statistics.csv")
cat("✓ Summary statistics saved to: monthly post/keyword_summary_statistics.csv\n\n")








# ============================================================================
# SECTION 3: YEARLY SUMMARIES AND TOTALS
# ============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("                     YEARLY SUMMARIES BY KEYWORD                                \n")
cat("================================================================================\n\n")

# Extract year from month
keyword_data <- keyword_data %>%
  mutate(year = year(month))

for (kw in keywords) {
  
  cat("--------------------------------------------------------------------------------\n")
  cat("KEYWORD:", toupper(kw), "\n")
  cat("--------------------------------------------------------------------------------\n\n")
  
  # Get column names
  count_col <- paste0("n_", kw)
  
  # Calculate yearly summaries
  yearly_summary <- keyword_data %>%
    group_by(year) %>%
    summarise(
      total_posts_year = sum(n_total_posts),
      keyword_posts = sum(get(count_col)),
      yearly_proportion = sum(get(count_col)) / sum(n_total_posts) * 100,
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  cat("YEARLY BREAKDOWN:\n")
  print(yearly_summary, n = Inf, row.names = FALSE)
  cat("\n")
  
  # Calculate overall total
  total_keyword_posts <- sum(keyword_data[[count_col]])
  total_all_posts <- sum(keyword_data$n_total_posts)
  overall_proportion <- total_keyword_posts / total_all_posts * 100
  
  cat("TOTAL ACROSS ALL TIME:\n")
  cat("  Total posts with keyword: ", total_keyword_posts, "\n")
  cat("  Total posts (all):        ", total_all_posts, "\n")
  cat("  Overall proportion:       ", round(overall_proportion, 3), "%\n\n")
  
}

cat("================================================================================\n")
cat("                        END OF YEARLY SUMMARIES                                 \n")
cat("================================================================================\n\n")

# Optional: Save yearly summaries to CSV files
for (kw in keywords) {
  count_col <- paste0("n_", kw)
  
  yearly_summary <- keyword_data %>%
    group_by(year) %>%
    summarise(
      total_posts_year = sum(n_total_posts),
      keyword_posts = sum(get(count_col)),
      yearly_proportion = sum(get(count_col)) / sum(n_total_posts) * 100,
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  write_csv(yearly_summary, paste0("monthly post/", kw, "_yearly_summary.csv"))
}

cat("✓ Yearly summaries saved to individual CSV files\n\n")