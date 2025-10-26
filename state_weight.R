rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

ada_state <- read.csv("ada_by_state_detailed_results.csv", stringsAsFactors = FALSE)
dem_state <- read.csv("democrat_by_state_detailed_results.csv", stringsAsFactors = FALSE)
seats_data <- read.csv("seats_by_state_wide_1946_1992.csv", stringsAsFactors = FALSE)

baseline <- read.csv("baseline_pooled_results.csv", stringsAsFactors = FALSE)

ada_pop <- read.csv("pop_weighted_detailed_ada_by_state.csv", stringsAsFactors = FALSE)
dem_pop <- read.csv("pop_weighted_detailed_democrat_by_state.csv", stringsAsFactors = FALSE)

print(baseline[, c("Analysis", "TE", "SE")])

seats_long <- seats_data %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "seats") %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

state_seats <- seats_long %>%
  group_by(state) %>%
  summarise(avg_seats = mean(seats, na.rm = TRUE), .groups = 'drop')

ada_merged <- ada_state %>%
  left_join(state_seats, by = "state") %>%
  rename(congress_seats = avg_seats) %>%
  left_join(
    ada_pop %>% select(State_Original, Avg_Population, Weight),
    by = c("state" = "State_Original")
  ) %>%
  rename(
    population = Avg_Population,
    population_weight_original = Weight
  )

dem_merged <- dem_state %>%
  left_join(state_seats, by = "state") %>%
  rename(congress_seats = avg_seats) %>%
  left_join(
    dem_pop %>% select(State_Original, Avg_Population, Weight),
    by = c("state" = "State_Original")
  ) %>%
  rename(
    population = Avg_Population,
    population_weight_original = Weight
  )


ada_valid <- ada_merged %>%
  filter(!is.na(congress_seats) & !is.na(population)) %>%
  mutate(
    # 1. Equal weight
    n_states = n(),
    equal_weight = 1 / n_states,
    
    # 2. Congress seats weight
    congress_weight = congress_seats / sum(congress_seats),
    
    # 3. Population weight
    population_weight = population / sum(population),
    
    # 4. Inverse variance weight
    variance = se^2,
    inverse_variance = 1 / variance,
    inverse_variance_weight = inverse_variance / sum(inverse_variance)
  ) %>%
  mutate(
    equal_TE = TE * equal_weight,
    congress_TE = TE * congress_weight,
    population_TE = TE * population_weight,
    inverse_variance_TE = TE * inverse_variance_weight
  )

dem_valid <- dem_merged %>%
  filter(!is.na(congress_seats) & !is.na(population)) %>%
  mutate(
    n_states = n(),
    equal_weight = 1 / n_states,
    congress_weight = congress_seats / sum(congress_seats),
    population_weight = population / sum(population),
    variance = se^2,
    inverse_variance = 1 / variance,
    inverse_variance_weight = inverse_variance / sum(inverse_variance)
  ) %>%
  mutate(
    equal_TE = TE * equal_weight,
    congress_TE = TE * congress_weight,
    population_TE = TE * population_weight,
    inverse_variance_TE = TE * inverse_variance_weight
  )

ada_baseline_TE <- baseline$TE[baseline$Analysis == "ADA"]
ada_baseline_SE <- baseline$SE[baseline$Analysis == "ADA"]
dem_baseline_TE <- baseline$TE[baseline$Analysis == "Democrat"]
dem_baseline_SE <- baseline$SE[baseline$Analysis == "Democrat"]

ada_pooled <- data.frame(
  Analysis = "ADA by State",
  Scheme = c("Pooled RDD (baseline)", "Simple Average", "Population", "Congressional Seats", "Inverse Variance (1/SEÂ²)"),
  TE = c(
    ada_baseline_TE,
    sum(ada_valid$equal_TE),
    sum(ada_valid$population_TE),
    sum(ada_valid$congress_TE),
    sum(ada_valid$inverse_variance_TE)
  ),
  SE = c(
    ada_baseline_SE,
    sqrt(sum((ada_valid$equal_weight * ada_valid$se)^2)),
    sqrt(sum((ada_valid$population_weight * ada_valid$se)^2)),
    sqrt(sum((ada_valid$congress_weight * ada_valid$se)^2)),
    sqrt(sum((ada_valid$inverse_variance_weight * ada_valid$se)^2))
  ),
  N_States = c(NA, rep(nrow(ada_valid), 4)),
  stringsAsFactors = FALSE
) %>%
  mutate(
    CI_lower = round(TE - 1.96 * SE, 3),
    CI_upper = round(TE + 1.96 * SE, 3),
    Diff_from_Simple = round(TE - TE[1], 3),
    Pct_Diff = round((Diff_from_Simple / abs(TE[1])) * 100, 3),
    TE = round(TE, 3),
    SE = round(SE, 3)
  )

dem_pooled <- data.frame(
  Analysis = "Democrat by State",
  Scheme = c("Pooled RDD (baseline)", "Simple Average", "Population", "Congressional Seats", "Inverse Variance (1/SEÂ²)"),
  TE = c(
    dem_baseline_TE,
    sum(dem_valid$equal_TE),
    sum(dem_valid$population_TE),
    sum(dem_valid$congress_TE),
    sum(dem_valid$inverse_variance_TE)
  ),
  SE = c(
    dem_baseline_SE,
    sqrt(sum((dem_valid$equal_weight * dem_valid$se)^2)),
    sqrt(sum((dem_valid$population_weight * dem_valid$se)^2)),
    sqrt(sum((dem_valid$congress_weight * dem_valid$se)^2)),
    sqrt(sum((dem_valid$inverse_variance_weight * dem_valid$se)^2))
  ),
  N_States = c(NA, rep(nrow(dem_valid), 4)),
  stringsAsFactors = FALSE
) %>%
  mutate(
    CI_lower = round(TE - 1.96 * SE, 3),
    CI_upper = round(TE + 1.96 * SE, 3),
    Diff_from_Simple = round(TE - TE[1], 3),
    Pct_Diff = round((Diff_from_Simple / abs(TE[1])) * 100, 3),
    TE = round(TE, 3),
    SE = round(SE, 3)
  )

print(ada_pooled)

print(dem_pooled)

output_dir <- "."
if(!dir.exists(output_dir)) dir.create(output_dir)

write.csv(ada_pooled, file.path(output_dir, "ada_by_state_weighting_table.csv"), row.names = FALSE)
write.csv(dem_pooled, file.path(output_dir, "dem_by_state_weighting_table.csv"), row.names = FALSE)

ada_detail <- ada_valid %>%
  mutate(
    TE = round(TE, 3),
    se = round(se, 3),
    congress_weight = round(congress_weight, 3),
    congress_TE = round(congress_TE, 3),
    population_weight = round(population_weight, 3),
    population_TE = round(population_TE, 3),
    equal_weight = round(equal_weight, 3),
    equal_TE = round(equal_TE, 3),
    inverse_variance = round(inverse_variance, 3),
    inverse_variance_weight = round(inverse_variance_weight, 3),
    inverse_variance_TE = round(inverse_variance_TE, 3),
    ci_lower = round(ci_lower, 3),
    ci_upper = round(ci_upper, 3),
    p_value = round(p_value, 3)
  ) %>%
  select(state, state_label, TE, se,
         congress_seats, congress_weight, congress_TE,
         population, population_weight, population_TE,
         equal_weight, equal_TE,
         inverse_variance, inverse_variance_weight, inverse_variance_TE,
         ci_lower, ci_upper, p_value, significant)

dem_detail <- dem_valid %>%
  mutate(
    TE = round(TE, 3),
    se = round(se, 3),
    congress_weight = round(congress_weight, 3),
    congress_TE = round(congress_TE, 3),
    population_weight = round(population_weight, 3),
    population_TE = round(population_TE, 3),
    equal_weight = round(equal_weight, 3),
    equal_TE = round(equal_TE, 3),
    inverse_variance = round(inverse_variance, 3),
    inverse_variance_weight = round(inverse_variance_weight, 3),
    inverse_variance_TE = round(inverse_variance_TE, 3),
    ci_lower = round(ci_lower, 3),
    ci_upper = round(ci_upper, 3),
    p_value = round(p_value, 3)
  ) %>%
  select(state, state_label, TE, se,
         congress_seats, congress_weight, congress_TE,
         population, population_weight, population_TE,
         equal_weight, equal_TE,
         inverse_variance, inverse_variance_weight, inverse_variance_TE,
         ci_lower, ci_upper, p_value, significant)

write.csv(ada_detail, file.path(output_dir, "ada_by_state_weighted_detailed.csv"), row.names = FALSE)
write.csv(dem_detail, file.path(output_dir, "dem_by_state_weighted_detailed.csv"), row.names = FALSE)


create_comparison_plot <- function(results, title, filename, baseline_val, n_states) {
  plot_data <- results %>%
    filter(Scheme != "Pooled RDD (baseline)") %>%
    mutate(Scheme = factor(Scheme, levels = rev(Scheme)))
  
  p <- ggplot(plot_data, aes(x = TE, y = Scheme)) +
    geom_col(aes(fill = Scheme), alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                  width = 0.3, linewidth = 1.5) +  
    geom_vline(xintercept = baseline_val, linetype = "dashed", 
               color = "red", linewidth = 1.5) + 
    geom_text(aes(label = sprintf("%.3f", TE)), 
              hjust = -0.3, size = 7, fontface = "bold") + 
    scale_fill_manual(values = c(
      "Simple Average" = "gray60",
      "Population" = "#51A868",
      "Congressional Seats" = "#F5A623",
      "Inverse Variance (1/SEÂ²)" = "#4A90E2"
    )) +
    labs(
      title = title,
      subtitle = sprintf("Range: %.3f to %.3f | Based on %d states", 
                         min(plot_data$TE), max(plot_data$TE), n_states),
      x = "Pooled Treatment Effect",
      y = ""
    ) +
    theme_minimal(base_size = 20) +  
    theme(
      plot.title = element_text(face = "bold", size = 26, hjust = 0.5), 
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),  
      axis.text.y = element_text(size = 18, face = "bold"),  
      axis.text.x = element_text(size = 16), 
      axis.title.x = element_text(size = 20, face = "bold"), 
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    annotate("text", x = baseline_val, y = Inf, 
             label = sprintf("Pooled RDD\n%.3f", baseline_val),
             vjust = 2, color = "red", size = 6, fontface = "bold")  
  
  ggsave(filename, p, width = 14, height = 7, dpi = 300)  
}

n_states_ada <- nrow(ada_valid)
n_states_dem <- nrow(dem_valid)

create_comparison_plot(
  ada_pooled, 
  "ADA by State: Comparison of Weighting Schemes",
  file.path(output_dir, "ada_by_state_comparison_large.png"),
  ada_baseline_TE,
  n_states_ada
)

create_comparison_plot(
  dem_pooled,
  "Democrat by State: Comparison of Weighting Schemes",
  file.path(output_dir, "democrat_by_state_comparison_large.png"),
  dem_baseline_TE,
  n_states_dem
)


create_forest_plot <- function(data, title, filename, baseline_val) {
  plot_data <- data %>%
    filter(Scheme != "Pooled RDD (baseline)") %>%
    mutate(
      Scheme = factor(Scheme, levels = Scheme),
      ypos = seq_along(Scheme)
    )
  
  png(filename, width = 14, height = 8, units = "in", res = 300)
  
  par(mar = c(6, 12, 4, 3), cex.axis = 1.8, cex.lab = 1.8, cex.main = 2.0)
  
  plot(0, type = "n", 
       xlim = range(c(plot_data$CI_lower, plot_data$CI_upper)) * 1.1,
       ylim = c(0.5, max(plot_data$ypos) + 0.5),
       xlab = "", ylab = "", yaxt = "n",
       main = title)
  
  abline(v = seq(round(min(plot_data$CI_lower), 1), 
                 round(max(plot_data$CI_upper), 1), 0.02), 
         col = "gray90", lty = 1)
  
  abline(v = baseline_val, col = "red", lty = 2, lwd = 3)
  abline(v = 0, col = "gray40", lty = 3, lwd = 2)
  
  colors <- c("gray60", "#51A868", "#F5A623", "#4A90E2")
  for(i in 1:nrow(plot_data)) {
    segments(plot_data$CI_lower[i], plot_data$ypos[i],
             plot_data$CI_upper[i], plot_data$ypos[i],
             col = colors[i], lwd = 4)
    
    points(plot_data$TE[i], plot_data$ypos[i], 
           pch = 18, cex = 3.5, col = colors[i])
    
    text(plot_data$TE[i], plot_data$ypos[i] + 0.2, 
         sprintf("%.3f", plot_data$TE[i]), 
         cex = 1.6, font = 2)
  }
  
  axis(2, at = plot_data$ypos, labels = plot_data$Scheme, 
       las = 1, cex.axis = 1.6, font.axis = 2)
  
  mtext("Treatment Effect", side = 1, line = 4, cex = 1.8, font = 2)
  
  legend("topright", 
         legend = c(sprintf("Baseline (%.3f)", baseline_val), "No Effect"),
         col = c("red", "gray40"), lty = c(2, 3), lwd = c(3, 2),
         cex = 1.5, bty = "n")
  
  dev.off()
  cat("Forest plot saved:", filename, "\n")
}

create_forest_plot(
  ada_pooled,
  "ADA by State: Treatment Effects with Different Weights",
  file.path(output_dir, "ada_forest_plot_large.png"),
  ada_baseline_TE
)

create_forest_plot(
  dem_pooled,
  "Democrat by State: Treatment Effects with Different Weights",
  file.path(output_dir, "dem_forest_plot_large.png"),
  dem_baseline_TE
)


