library(dplyr)

ada_state <- read.csv("ada_by_state_detailed_results.csv")
dem_state <- read.csv("democrat_by_state_detailed_results.csv")
pop_data <- read.csv("state_population_even_years_1946_1992.csv")

state_pop <- pop_data %>%
  group_by(state_abbr, state_name) %>%
  summarise(
    avg_population = mean(population, na.rm = TRUE),
    .groups = 'drop'
  )


compute_population_weighted_state <- function(results_data, pop_data, name, output_dir) {
  TE <- as.numeric(results_data$TE)
  SE <- as.numeric(results_data$se)
  State_raw <- results_data$state
  
  State_clean <- gsub("^[0-9]+:\\s*", "", State_raw)
  State_clean <- gsub("N\\. Carolina", "North Carolina", State_clean)
  State_clean <- gsub("S\\. Carolina", "South Carolina", State_clean)
  State_clean <- gsub("W\\. Virginia", "West Virginia", State_clean)
  
  merged <- data.frame(
    state_raw = State_raw,
    state_clean = State_clean, 
    TE = TE, 
    SE = SE,
    stringsAsFactors = FALSE
  ) %>%
    left_join(pop_data, by = c("state_clean" = "state_name"))

  weight <- merged$avg_population / sum(merged$avg_population, na.rm = TRUE)
  weight[is.na(weight)] <- 0
  
  pooled_estimate <- sum(TE * weight, na.rm = TRUE)
  pooled_se <- sqrt(sum((weight * SE)^2, na.rm = TRUE))

  result_summary <- data.frame(
    Analysis = name,
    Pooled_Estimate = pooled_estimate,
    SE = pooled_se,
    CI_Lower = pooled_estimate - 1.96*pooled_se,
    CI_Upper = pooled_estimate + 1.96*pooled_se,
    stringsAsFactors = FALSE
  )
  
  detailed_weights <- data.frame(
    State_Original = merged$state_raw,
    State_Name = merged$state_clean,
    State_Abbr = merged$state_abbr,
    TE = TE,
    SE = SE,
    Avg_Population = merged$avg_population,
    Weight = weight,
    stringsAsFactors = FALSE
  )
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write.csv(result_summary, 
            file.path(output_dir, paste0("pop_weighted_summary_", gsub(" ", "_", tolower(name)), ".csv")),
            row.names = FALSE)
  
  write.csv(detailed_weights,
            file.path(output_dir, paste0("pop_weighted_detailed_", gsub(" ", "_", tolower(name)), ".csv")),
            row.names = FALSE)
  
  return(list(estimate = pooled_estimate, se = pooled_se))
}



output_dir <- "."

ada_state_result <- compute_population_weighted_state(ada_state, state_pop, "ADA by State", output_dir)
dem_state_result <- compute_population_weighted_state(dem_state, state_pop, "Democrat by State", output_dir)

library(dplyr)
library(ggplot2)

ada_state_weights <- read.csv("/Users/wenqing/Desktop/Code/weight/population_state_weight/pop_weighted_detailed_ada_by_state.csv")
dem_state_weights <- read.csv("/Users/wenqing/Desktop/Code/weight/population_state_weight/pop_weighted_detailed_democrat_by_state.csv")

output_dir <- "."


# ADA by State
png(file.path(output_dir, "plot_ada_state_weights.png"), 
    width = 12, height = 8, units = "in", res = 300)

ada_state_weights %>%
  filter(!is.na(Avg_Population)) %>%
  arrange(TE) %>%
  mutate(State_Abbr = factor(State_Abbr, levels = State_Abbr)) %>%
  ggplot(aes(x = State_Abbr, y = TE)) +
  geom_point(aes(size = Weight * 100), color = "#2E86AB", alpha = 0.7) +
  geom_errorbar(aes(ymin = TE - 1.96*SE, ymax = TE + 1.96*SE), 
                width = 0.3, color = "#2E86AB", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ADA by State: Treatment Effects (Population Weighted)",
       x = "State",
       y = "Treatment Effect (ADA Score)",
       size = "Weight (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

dev.off()


# Democrat by State
png(file.path(output_dir, "plot_dem_state_weights.png"), 
    width = 12, height = 8, units = "in", res = 300)

dem_state_weights %>%
  filter(!is.na(Avg_Population)) %>%
  arrange(TE) %>%
  mutate(State_Abbr = factor(State_Abbr, levels = State_Abbr)) %>%
  ggplot(aes(x = State_Abbr, y = TE)) +
  geom_point(aes(size = Weight * 100), color = "#A23B72", alpha = 0.7) +
  geom_errorbar(aes(ymin = TE - 1.96*SE, ymax = TE + 1.96*SE), 
                width = 0.3, color = "#A23B72", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Democrat by State: Treatment Effects (Population Weighted)",
       x = "State",
       y = "Treatment Effect (Probability)",
       size = "Weight (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

dev.off()

# Figure2

png(file.path(output_dir, "plot_state_weight_distribution.png"), 
    width = 12, height = 6, units = "in", res = 300)

par(mfrow = c(1, 2))

# ADA
barplot(ada_state_weights$Weight[!is.na(ada_state_weights$Avg_Population)] * 100,
        names.arg = ada_state_weights$State_Abbr[!is.na(ada_state_weights$Avg_Population)],
        las = 2,
        col = "#2E86AB",
        main = "ADA by State: Population Weight Distribution",
        ylab = "Weight (%)",
        cex.names = 0.7)

# Democrat
barplot(dem_state_weights$Weight[!is.na(dem_state_weights$Avg_Population)] * 100,
        names.arg = dem_state_weights$State_Abbr[!is.na(dem_state_weights$Avg_Population)],
        las = 2,
        col = "#A23B72",
        main = "Democrat by State: Population Weight Distribution",
        ylab = "Weight (%)",
        cex.names = 0.7)

dev.off()

