################################################################################
# Appendix E: The Characteristic of Sample
# Complete version with both ADA and Democrat analyses
################################################################################

library(tidyverse)
library(knitr)
library(kableExtra)


# Create output directory
output_dir <- "."
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Open output file
output_file <- file.path(output_dir, "appendix_e_sample_characteristics.txt")
sink(output_file)

# Load main dataset
data <- read_csv("enricoall2.csv", show_col_types = FALSE)

# Load detailed results
ada_state <- read_csv("ada_by_state_detailed_results.csv", show_col_types = FALSE)
ada_year <- read_csv("ada_by_year_detailed_results.csv", show_col_types = FALSE)
dem_state <- read_csv("democrat_by_state_detailed_results.csv", show_col_types = FALSE)
dem_year <- read_csv("democrat_by_year_detailed_results.csv", show_col_types = FALSE)

cat("================================================================================\n")
cat("Appendix E: The Characteristic of Sample\n")
cat("================================================================================\n\n")

################################################################################
# E.1 Sample Summary
################################################################################

cat("E.1 Sample Summary\n")
cat("------------------\n\n")

# Calculate total observations in analysis (those with non-missing key variables)
analysis_data <- data %>%
  filter(!is.na(demvoteshare), !is.na(ada_vs))

# Count observations on each side
n_below <- sum(analysis_data$demvoteshare < 0.5, na.rm = TRUE)
n_above <- sum(analysis_data$demvoteshare >= 0.5, na.rm = TRUE)

# Number of unique states in full data
n_states_data <- length(unique(data$state))

# States included in state-level analysis (from the detailed results)
n_states_included <- nrow(ada_state)

# Number of election years
n_years <- length(unique(data$year))

# Average observations per included state
avg_obs_per_state <- round(nrow(analysis_data) / n_states_included)

# Average observations per year
avg_obs_per_year <- round(nrow(analysis_data) / n_years)

# Create summary table
sample_summary <- data.frame(
  Characteristic = c(
    "Time period",
    "Total observations (in analysis)",
    "Number of unique states in data",
    "States included in state-level analysis",
    "Inclusion criterion per state",
    "Number of election years",
    "Average observations per included state",
    "Average observations per year",
    "Total observations below 50% threshold",
    "Total observations above 50% threshold"
  ),
  Value = c(
    "1946-1992",
    format(nrow(analysis_data), big.mark = ","),
    as.character(n_states_data),
    as.character(n_states_included),
    "â‰¥15 observations on each side of 50% threshold",
    as.character(n_years),
    as.character(avg_obs_per_state),
    as.character(avg_obs_per_year),
    format(n_below, big.mark = ","),
    format(n_above, big.mark = ",")
  )
)

print(kable(sample_summary, 
            col.names = c("Characteristic", "Value"),
            align = c("l", "r"),
            format = "simple"))

# Save table as CSV
write_csv(sample_summary, file.path(output_dir, "table_e1_sample_summary.csv"))

cat("\n")
cat("Table E.1: Summary of Sample Characteristics\n\n")
cat("States with fewer than 15 observations on either side of the 50% Democratic\n")
cat("vote share threshold are excluded from state-level analysis to ensure stable\n")
cat("group-specific estimation via local polynomial regression. All group-level\n")
cat("estimates use common MSE-optimal bandwidth computed from pooled data (see\n")
cat("Section 3.2.1). The number of observations varies substantially across years and\n")
cat("states due to redistricting events and variation in the number of competitive\n")
cat("races.\n\n")

################################################################################
# E.2 Temporal Coverage: Sample Size Over Time
################################################################################

cat("E.2 Temporal Coverage: Sample Size Over Time\n")
cat("---------------------------------------------\n\n")

# Calculate observations by year for both analyses
obs_by_year_ada <- ada_year %>%
  mutate(
    n_obs = n_eff_left + n_eff_right,
    analysis = "ADA"
  ) %>%
  select(year, n_obs, analysis) %>%
  arrange(year)

obs_by_year_dem <- dem_year %>%
  mutate(
    n_obs = n_eff_left + n_eff_right,
    analysis = "Democrat"
  ) %>%
  select(year, n_obs, analysis) %>%
  arrange(year)

obs_by_year_combined <- bind_rows(obs_by_year_ada, obs_by_year_dem)

# Redistricting years
redistricting_years <- c(1950, 1960, 1970, 1980, 1990)

cat("Sample Size by Year (ADA Analysis):\n\n")
print(kable(obs_by_year_ada %>% select(year, n_obs),
            col.names = c("Year", "Total Observations"),
            format = "simple"))
cat("\n\n")

cat("Sample Size by Year (Democrat Analysis):\n\n")
print(kable(obs_by_year_dem %>% select(year, n_obs),
            col.names = c("Year", "Total Observations"),
            format = "simple"))

# Save tables as CSV
write_csv(obs_by_year_ada %>% select(year, n_obs), 
          file.path(output_dir, "table_e2a_obs_by_year_ada.csv"))
write_csv(obs_by_year_dem %>% select(year, n_obs), 
          file.path(output_dir, "table_e2b_obs_by_year_democrat.csv"))

# Generate Figure E.1: Sample Size Over Time (Combined)
png(file.path(output_dir, "figure_e1_sample_size_over_time_combined.png"), 
    width = 12, height = 6, units = "in", res = 300)

ggplot(obs_by_year_combined, aes(x = year, y = n_obs, color = analysis, group = analysis)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = redistricting_years, 
             linetype = "dashed", color = "gray50", alpha = 0.6, size = 0.8) +
  annotate("text", x = 1952, y = max(obs_by_year_combined$n_obs) * 0.98, 
           label = "Redistricting", color = "gray40", size = 3.5, hjust = 0.5) +
  labs(
    title = "Sample Size Over Time (1946-1992)",
    subtitle = "Observations within MSE-optimal bandwidth. Dashed vertical lines mark redistricting years",
    x = "Election Year",
    y = "Total Observations",
    color = "Analysis"
  ) +
  scale_color_manual(values = c("ADA" = "#2E86AB", "Democrat" = "#E63946")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(breaks = seq(1946, 1992, by = 4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(obs_by_year_combined$n_obs) * 1.05))

dev.off()

# Generate separate figures for each analysis
png(file.path(output_dir, "figure_e1a_sample_size_ada.png"), 
    width = 10, height = 6, units = "in", res = 300)

ggplot(obs_by_year_ada, aes(x = year, y = n_obs)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#2E86AB", size = 3) +
  geom_vline(xintercept = redistricting_years, 
             linetype = "dashed", color = "#A23B72", alpha = 0.6, size = 0.8) +
  labs(
    title = "Sample Size Over Time - ADA Analysis (1946-1992)",
    subtitle = "Observations within MSE-optimal bandwidth (h* = [0.093, 0.066])",
    x = "Election Year",
    y = "Total Observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(breaks = seq(1946, 1992, by = 4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(obs_by_year_ada$n_obs) * 1.05))

dev.off()

png(file.path(output_dir, "figure_e1b_sample_size_democrat.png"), 
    width = 10, height = 6, units = "in", res = 300)

ggplot(obs_by_year_dem, aes(x = year, y = n_obs)) +
  geom_line(color = "#E63946", size = 1.2) +
  geom_point(color = "#E63946", size = 3) +
  geom_vline(xintercept = redistricting_years, 
             linetype = "dashed", color = "#A23B72", alpha = 0.6, size = 0.8) +
  labs(
    title = "Sample Size Over Time - Democrat Analysis (1946-1992)",
    subtitle = "Observations within MSE-optimal bandwidth (h* = [0.1, 0.057])",
    x = "Election Year",
    y = "Total Observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(breaks = seq(1946, 1992, by = 4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(obs_by_year_dem$n_obs) * 1.05))

dev.off()

cat("\n\nFigures saved: figure_e1_sample_size_over_time_combined.png\n")
cat("            figure_e1a_sample_size_ada.png\n")
cat("            figure_e1b_sample_size_democrat.png\n\n")

cat("Figure E.1: Sample Size Over Time (1946-1992)\n\n")
cat("Figures showing observations within MSE-optimal bandwidth by election year.\n")
cat("The ADA analysis uses bandwidth h* = [0.093, 0.066], while the Democrat analysis\n")
cat("uses bandwidth h* = [0.1, 0.057]. Dashed vertical lines mark redistricting years\n")
cat("(1950, 1960, 1970, 1980, 1990). Both analyses show substantial variation over time,\n")
cat("with notable drops around redistricting years, particularly in 1960.\n\n")

################################################################################
# E.3 Geographic Coverage: Sample Size by State
################################################################################

cat("E.3 Geographic Coverage: Sample Size by State\n")
cat("----------------------------------------------\n\n")

# Calculate observations by state for both analyses
state_obs_ada <- ada_state %>%
  mutate(
    state_name = str_replace(state_label, "^\\d+:\\s*", ""),
    n_total = n_eff_left + n_eff_right
  ) %>%
  select(state_name, n_eff_left, n_eff_right, n_total) %>%
  arrange(desc(n_total))

state_obs_dem <- dem_state %>%
  mutate(
    state_name = str_replace(state_label, "^\\d+:\\s*", ""),
    n_total = n_eff_left + n_eff_right
  ) %>%
  select(state_name, n_eff_left, n_eff_right, n_total) %>%
  arrange(desc(n_total))

cat("Sample Size by State - ADA Analysis (N=32 included states):\n\n")
print(kable(state_obs_ada,
            col.names = c("State", "N Left", "N Right", "Total Observations"),
            format = "simple"))

cat("\n\n")

cat("Sample Size by State - Democrat Analysis (N=32 included states):\n\n")
print(kable(state_obs_dem,
            col.names = c("State", "N Left", "N Right", "Total Observations"),
            format = "simple"))

# Save tables as CSV
write_csv(state_obs_ada, file.path(output_dir, "table_e3a_obs_by_state_ada.csv"))
write_csv(state_obs_dem, file.path(output_dir, "table_e3b_obs_by_state_democrat.csv"))

# Generate Figure E.2: Sample Size by State (ADA)
png(file.path(output_dir, "figure_e2a_sample_size_by_state_ada.png"), 
    width = 12, height = 8, units = "in", res = 300)

ggplot(state_obs_ada, aes(x = reorder(state_name, -n_total), y = n_total)) +
  geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8) +
  labs(
    title = "Sample Size by State - ADA Analysis (N=32 included states)",
    subtitle = "States ordered by descending total observations within bandwidth",
    x = "State",
    y = "Total Observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), 
                     limits = c(0, max(state_obs_ada$n_total) * 1.05))

dev.off()

# Generate Figure E.2: Sample Size by State (Democrat)
png(file.path(output_dir, "figure_e2b_sample_size_by_state_democrat.png"), 
    width = 12, height = 8, units = "in", res = 300)

ggplot(state_obs_dem, aes(x = reorder(state_name, -n_total), y = n_total)) +
  geom_bar(stat = "identity", fill = "#E63946", alpha = 0.8) +
  labs(
    title = "Sample Size by State - Democrat Analysis (N=32 included states)",
    subtitle = "States ordered by descending total observations within bandwidth",
    x = "State",
    y = "Total Observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), 
                     limits = c(0, max(state_obs_dem$n_total) * 1.05))

dev.off()

cat("\n\nFigures saved: figure_e2a_sample_size_by_state_ada.png\n")
cat("            figure_e2b_sample_size_by_state_democrat.png\n\n")

cat("Figure E.2: Sample Size by State (N=32 included states)\n\n")
cat("Bar charts showing total observations per state in descending order for both analyses.\n")
cat("The state-level sample exhibits substantial heterogeneity, with large states\n")
cat("(New York, California, Pennsylvania, Illinois, Ohio, Michigan) dominating the sample\n")
cat("with many observations, while smaller states contribute fewer observations.\n")
cat("The distribution is highly right-skewed, reflecting both state population\n")
cat("differences and the number of congressional districts.\n\n")

################################################################################
# E.4 Implicit Weighting: How Sample Composition Drives Weights
################################################################################

cat("E.4 Implicit Weighting: How Sample Composition Drives Weights\n")
cat("--------------------------------------------------------------\n\n")

# Top 5 years by sample size for ADA
top_years_ada <- ada_year %>%
  arrange(desc(n_eff_left + n_eff_right)) %>%
  head(5) %>%
  mutate(
    n_total = n_eff_left + n_eff_right,
    implicit_weight = weight_year,
    category = case_when(
      n_total >= 300 ~ "Large",
      TRUE ~ "Medium"
    ),
    analysis = "ADA"
  ) %>%
  select(analysis, year, n_total, implicit_weight, category)

# Top 5 years by sample size for Democrat
top_years_dem <- dem_year %>%
  arrange(desc(n_eff_left + n_eff_right)) %>%
  head(5) %>%
  mutate(
    n_total = n_eff_left + n_eff_right,
    implicit_weight = weight_year,
    category = case_when(
      n_total >= 300 ~ "Large",
      TRUE ~ "Medium"
    ),
    analysis = "Democrat"
  ) %>%
  select(analysis, year, n_total, implicit_weight, category)

# Top 5 states by sample size for ADA
top_states_ada <- ada_state %>%
  mutate(
    state_name = str_replace(state_label, "^\\d+:\\s*", ""),
    n_total = n_eff_left + n_eff_right
  ) %>%
  arrange(desc(n_total)) %>%
  head(5) %>%
  mutate(
    implicit_weight = weight_state,
    category = case_when(
      n_total >= 1000 ~ "Very large",
      n_total >= 500 ~ "Large",
      TRUE ~ "Medium"
    ),
    analysis = "ADA"
  ) %>%
  select(analysis, state_name, n_total, implicit_weight, category)

# Top 5 states by sample size for Democrat
top_states_dem <- dem_state %>%
  mutate(
    state_name = str_replace(state_label, "^\\d+:\\s*", ""),
    n_total = n_eff_left + n_eff_right
  ) %>%
  arrange(desc(n_total)) %>%
  head(5) %>%
  mutate(
    implicit_weight = weight_state,
    category = case_when(
      n_total >= 1000 ~ "Very large",
      n_total >= 500 ~ "Large",
      TRUE ~ "Medium"
    ),
    analysis = "Democrat"
  ) %>%
  select(analysis, state_name, n_total, implicit_weight, category)

# Combine tables
cat("Top 5 Years by Implicit Weight:\n\n")
cat("ADA Analysis:\n")
print(kable(top_years_ada %>% 
              mutate(Implicit_Weight = sprintf("%.1f%%", implicit_weight * 100)) %>%
              select(Year = year, N_Total = n_total, Implicit_Weight, Category = category),
            format = "simple"))

cat("\n\nDemocrat Analysis:\n")
print(kable(top_years_dem %>% 
              mutate(Implicit_Weight = sprintf("%.1f%%", implicit_weight * 100)) %>%
              select(Year = year, N_Total = n_total, Implicit_Weight, Category = category),
            format = "simple"))

cat("\n\nTop 5 States by Implicit Weight:\n\n")
cat("ADA Analysis:\n")
print(kable(top_states_ada %>% 
              mutate(Implicit_Weight = sprintf("%.1f%%", implicit_weight * 100)) %>%
              select(State = state_name, N_Total = n_total, Implicit_Weight, Category = category),
            format = "simple"))

cat("\n\nDemocrat Analysis:\n")
print(kable(top_states_dem %>% 
              mutate(Implicit_Weight = sprintf("%.1f%%", implicit_weight * 100)) %>%
              select(State = state_name, N_Total = n_total, Implicit_Weight, Category = category),
            format = "simple"))

# Save tables
write_csv(top_years_ada, file.path(output_dir, "table_e4a_implicit_weights_years_ada.csv"))
write_csv(top_years_dem, file.path(output_dir, "table_e4b_implicit_weights_years_democrat.csv"))
write_csv(top_states_ada, file.path(output_dir, "table_e4c_implicit_weights_states_ada.csv"))
write_csv(top_states_dem, file.path(output_dir, "table_e4d_implicit_weights_states_democrat.csv"))

# Generate combined implicit weighting figure
implicit_years_combined <- bind_rows(
  top_years_ada %>% mutate(Group_Type = "Top 5 Years", Group = paste("Year", year)),
  top_years_dem %>% mutate(Group_Type = "Top 5 Years", Group = paste("Year", year))
)

implicit_states_combined <- bind_rows(
  top_states_ada %>% mutate(Group_Type = "Top 5 States", Group = paste("State", state_name)),
  top_states_dem %>% mutate(Group_Type = "Top 5 States", Group = paste("State", state_name))
)

implicit_combined <- bind_rows(implicit_years_combined, implicit_states_combined)

png(file.path(output_dir, "figure_e3_implicit_weights_comparison.png"), 
    width = 14, height = 8, units = "in", res = 300)

implicit_combined %>%
  mutate(
    Group_Short = str_replace(Group, "Year |State ", ""),
    Implicit_Weight_Pct = implicit_weight * 100
  ) %>%
  ggplot(aes(x = reorder(Group_Short, -Implicit_Weight_Pct), 
             y = Implicit_Weight_Pct, 
             fill = analysis)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Implicit_Weight_Pct)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  facet_wrap(~Group_Type, scales = "free_x", ncol = 2) +
  labs(
    title = "Implicit Weighting in Pooled RDD",
    subtitle = "Weight automatically assigned based on sample size - comparison between ADA and Democrat analyses",
    x = "",
    y = "Implicit Weight (%)",
    fill = "Analysis"
  ) +
  scale_fill_manual(values = c("ADA" = "#2E86AB", "Democrat" = "#E63946")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(implicit_combined$implicit_weight) * 100 * 1.2))

dev.off()

cat("\n\nFigure saved: figure_e3_implicit_weights_comparison.png\n\n")

cat("Table E.2: Implicit Weighting Examples (Top Performers)\n\n")
cat("This table illustrates the implicit weighting problem discussed in Section 3.1. In\n")
cat("a standard pooled RDD (without explicit weighting), groups with larger sample\n")
cat("sizes automatically receive proportionally higher weight in the aggregated\n")
cat("estimate. The weights differ slightly between ADA and Democrat analyses due to\n")
cat("different MSE-optimal bandwidths, but the pattern remains consistent: large states\n")
cat("and high-observation years dominate. These data-driven weights reflect the empirical\n")
cat("distribution of close elections across space and time, not necessarily the\n")
cat("substantive importance of these contexts for answering the research question.\n")
cat("This is why our three-step framework allows researchers to make weighting\n")
cat("explicit and tailored to their specific objectives (Section 3.4).\n\n")

################################################################################
# E.5 Methodology Notes
################################################################################

cat("E.5 Methodology Notes\n")
cat("---------------------\n\n")

cat("Bandwidth and Estimation: Both outcome-specific analyses (ADA ideology score and\n")
cat("Democrat probability) use MSE-optimal bandwidth computed via the Calonico,\n")
cat("Cattaneo, and Titiunik (2014) procedure applied to pooled data. For ADA scores, the\n")
cat("optimal bandwidth is h* = [0.093, 0.066]. For Democrat probability, h* = [0.1, 0.057].\n")
cat("All group-specific estimates in Step 1 use these common bandwidths to ensure\n")
cat("comparability across groupsâ€”differences in estimated treatment effects reflect genuine\n")
cat("heterogeneity in true parameters, not differences in local smoothing choices.\n\n")

cat("Local Polynomial Specification: All estimates employ local linear regression\n")
cat("(polynomial order p=1) with triangular kernel. Standard errors are heteroskedasticity-\n")
cat("robust, computed via the rdrobust package (Calonico et al. 2015). Confidence intervals\n")
cat("are bias-corrected 95% intervals, following modern RDD best practices.\n\n")

cat("Inclusion Criteria: The 32 states included in state-level analysis are those with at least\n")
cat("15 observations on each side of the 50% threshold. This criterion ensures adequate data\n")
cat("for stable local polynomial estimation. The 23 election years span 1946-1992, with no\n")
cat("years excluded from the analysis sample.\n\n")

################################################################################
# E.6 Comparison with Original Lee (2004) Analysis
################################################################################

cat("E.6 Comparison with Original Lee (2004) Analysis\n")
cat("-------------------------------------------------\n\n")

cat("Our pooled RDD estimates closely align with Lee's (2004) reported results. The close\n")
cat("correspondence validates our implementation of standard RDD procedures and\n")
cat("confirms that our use of common bandwidth across groups produces results consistent\n")
cat("with prior work. The new contribution of this analysis is decomposing the pooled\n")
cat("estimates into group-specific components (Step 1) and making implicit weighting\n")
cat("explicit (Step 3), rather than reporting a single aggregated estimate.\n\n")

cat("================================================================================\n")
cat("End of Appendix E\n")
cat("================================================================================\n")

# Close output file
sink()

# Print completion message to console
cat("\n=== Appendix E Generation Complete ===\n")
cat("Output saved to:", output_dir, "\n")
cat("Files created:\n")
cat("  Text Output:\n")
cat("    - appendix_e_sample_characteristics.txt\n")
cat("  Tables:\n")
cat("    - table_e1_sample_summary.csv\n")
cat("    - table_e2a_obs_by_year_ada.csv\n")
cat("    - table_e2b_obs_by_year_democrat.csv\n")
cat("    - table_e3a_obs_by_state_ada.csv\n")
cat("    - table_e3b_obs_by_state_democrat.csv\n")
cat("    - table_e4a_implicit_weights_years_ada.csv\n")
cat("    - table_e4b_implicit_weights_years_democrat.csv\n")
cat("    - table_e4c_implicit_weights_states_ada.csv\n")
cat("    - table_e4d_implicit_weights_states_democrat.csv\n")
cat("  Figures:\n")
cat("    - figure_e1_sample_size_over_time_combined.png\n")
cat("    - figure_e1a_sample_size_ada.png\n")
cat("    - figure_e1b_sample_size_democrat.png\n")
cat("    - figure_e2a_sample_size_by_state_ada.png\n")
cat("    - figure_e2b_sample_size_by_state_democrat.png\n")
cat("    - figure_e3_implicit_weights_comparison.png\n")
cat("======================================\n")
