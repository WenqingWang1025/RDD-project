rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)


ada_year <- read.csv("ada_by_year_detailed_results.csv", stringsAsFactors = FALSE)
dem_year <- read.csv("democrat_by_year_detailed_results.csv", stringsAsFactors = FALSE)

baseline <- read.csv("baseline_pooled_results.csv", stringsAsFactors = FALSE)

ada_pop <- read.csv("population_weighted_detailed_ada_by_year.csv", stringsAsFactors = FALSE)
dem_pop <- read.csv("population_weighted_detailed_democrat_by_year.csv", stringsAsFactors = FALSE)

print(baseline[, c("Analysis", "TE", "SE")])

# ADA -
ada_merged <- ada_year %>%
  left_join(ada_pop %>% select(Year, Population, Weight), by = c("year" = "Year")) %>%
  rename(
    population = Population,
    population_weight = Weight
  )

# Democrat
dem_merged <- dem_year %>%
  left_join(dem_pop %>% select(Year, Population, Weight), by = c("year" = "Year")) %>%
  rename(
    population = Population,
    population_weight = Weight
  )


#  -
calculate_year_weights <- function(data) {
  
  valid <- data %>%
    filter(!is.na(population))
  
  result <- valid %>%
    mutate(
      # 1.
      n_years = n(),
      equal_weight = 1 / n_years,
      
      # 2.
      # (population_weight)
      
      # 3.
      variance = se^2,
      inverse_variance = 1 / variance,
      inverse_variance_weight = inverse_variance / sum(inverse_variance)
    ) %>%
    mutate(
      equal_TE = TE * equal_weight,
      population_TE = TE * population_weight,
      inverse_variance_TE = TE * inverse_variance_weight
    )
  
  return(result)
}

ada_weighted <- calculate_year_weights(ada_merged)
dem_weighted <- calculate_year_weights(dem_merged)


ada_baseline_TE <- baseline$TE[baseline$Analysis == "ADA"]
ada_baseline_SE <- baseline$SE[baseline$Analysis == "ADA"]
dem_baseline_TE <- baseline$TE[baseline$Analysis == "Democrat"]
dem_baseline_SE <- baseline$SE[baseline$Analysis == "Democrat"]

# ADA - 3
ada_pooled <- data.frame(
  Analysis = "ADA by Year",
  Scheme = c("Pooled RDD (baseline)", "Simple Average", "Population", "Inverse Variance (1/SEÂ²)"),
  TE = c(
    ada_baseline_TE,
    sum(ada_weighted$equal_TE),
    sum(ada_weighted$population_TE),
    sum(ada_weighted$inverse_variance_TE)
  ),
  SE = c(
    ada_baseline_SE,
    sqrt(sum((ada_weighted$equal_weight * ada_weighted$se)^2)),
    sqrt(sum((ada_weighted$population_weight * ada_weighted$se)^2)),
    sqrt(sum((ada_weighted$inverse_variance_weight * ada_weighted$se)^2))
  ),
  stringsAsFactors = FALSE
)

# Democrat - 3
dem_pooled <- data.frame(
  Analysis = "Democrat by Year",
  Scheme = c("Pooled RDD (baseline)", "Simple Average", "Population", "Inverse Variance (1/SEÂ²)"),
  TE = c(
    dem_baseline_TE,
    sum(dem_weighted$equal_TE),
    sum(dem_weighted$population_TE),
    sum(dem_weighted$inverse_variance_TE)
  ),
  SE = c(
    dem_baseline_SE,
    sqrt(sum((dem_weighted$equal_weight * dem_weighted$se)^2)),
    sqrt(sum((dem_weighted$population_weight * dem_weighted$se)^2)),
    sqrt(sum((dem_weighted$inverse_variance_weight * dem_weighted$se)^2))
  ),
  stringsAsFactors = FALSE
)


ada_pooled <- ada_pooled %>%
  mutate(
    CI_lower = round(TE - 1.96 * SE, 3),
    CI_upper = round(TE + 1.96 * SE, 3),
    Diff_from_Simple = round(TE - TE[1], 3),
    Pct_Diff = round((Diff_from_Simple / abs(TE[1])) * 100, 3),
    TE = round(TE, 3),
    SE = round(SE, 3)
  )

dem_pooled <- dem_pooled %>%
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

write.csv(ada_pooled, file.path(output_dir, "ada_by_year_weighting_table.csv"), row.names = FALSE)
write.csv(dem_pooled, file.path(output_dir, "dem_by_year_weighting_table.csv"), row.names = FALSE)

#  -
ada_detail <- ada_weighted %>%
  mutate(
    TE = round(TE, 3),
    se = round(se, 3),
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
  select(year, year_label, TE, se,
         population, population_weight, population_TE,
         equal_weight, equal_TE,
         inverse_variance, inverse_variance_weight, inverse_variance_TE,
         ci_lower, ci_upper, p_value, significant)

dem_detail <- dem_weighted %>%
  mutate(
    TE = round(TE, 3),
    se = round(se, 3),
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
  select(year, year_label, TE, se,
         population, population_weight, population_TE,
         equal_weight, equal_TE,
         inverse_variance, inverse_variance_weight, inverse_variance_TE,
         ci_lower, ci_upper, p_value, significant)

write.csv(ada_detail, file.path(output_dir, "ada_by_year_weighted_detailed.csv"), row.names = FALSE)
write.csv(dem_detail, file.path(output_dir, "dem_by_year_weighted_detailed.csv"), row.names = FALSE)


#  - 3
create_comparison_plot <- function(results, title, filename, baseline_val, n_years) {
  plot_data <- results %>%
    filter(Scheme != "Pooled RDD (baseline)") %>%
    mutate(Scheme = factor(Scheme, levels = rev(Scheme)))
  
  # x
  x_min <- min(0, min(plot_data$CI_lower))
  x_max <- max(plot_data$CI_upper) * 1.15  # 15%
  
  p <- ggplot(plot_data, aes(x = TE, y = Scheme)) +
    geom_col(aes(fill = Scheme), alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                  width = 0.3, linewidth = 1.5) +  
    geom_vline(xintercept = baseline_val, linetype = "dashed", 
               color = "red", linewidth = 1.5) + 
    # error bar
    geom_text(aes(x = CI_upper, label = sprintf("%.3f", TE)), 
              hjust = -0.2, size = 7, fontface = "bold") +  
    scale_fill_manual(values = c(
      "Simple Average" = "gray60",
      "Population" = "#51A868",
      "Inverse Variance (1/SEÂ²)" = "#4A90E2"
    )) +
    scale_x_continuous(limits = c(x_min, x_max), expand = c(0.02, 0)) +  # x
    labs(
      title = title,
      subtitle = sprintf("Range: %.3f to %.3f | Based on %d years", 
                         min(plot_data$TE), max(plot_data$TE), n_years),
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

n_years_ada <- nrow(ada_weighted)
n_years_dem <- nrow(dem_weighted)

# ADA
create_comparison_plot(
  ada_pooled, 
  "ADA by Year: Comparison of Weighting Schemes",
  file.path(output_dir, "ada_by_year_comparison.png"),
  ada_baseline_TE,
  n_years_ada
)

# Democrat
create_comparison_plot(
  dem_pooled,
  "Democrat by Year: Comparison of Weighting Schemes",
  file.path(output_dir, "democrat_by_year_comparison.png"),
  dem_baseline_TE,
  n_years_dem
)



create_time_series_plot <- function(data, title, filename) {
  
  plot_data <- data %>%
    select(year, year_label, TE, se, ci_lower, ci_upper) %>%
    mutate(year_num = as.numeric(year))
  
  p <- ggplot(plot_data, aes(x = year_num, y = TE)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.2, fill = "#4A90E2") +
    geom_line(linewidth = 1.5, color = "#4A90E2") +
    geom_point(size = 5, color = "#4A90E2", fill = "white", shape = 21, stroke = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1.2) +
    geom_text(aes(label = sprintf("%.2f", TE)), 
              vjust = -1.5, size = 5, fontface = "bold") +
    scale_x_continuous(breaks = seq(1950, 1990, 5)) +
    labs(
      title = title,
      subtitle = "Treatment Effects Over Time with 95% Confidence Intervals",
      x = "Year",
      y = "Treatment Effect"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 20, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(filename, p, width = 16, height = 8, dpi = 300)
  cat("Time series plot saved:", filename, "\n")
}

create_time_series_plot(
  ada_weighted,
  "ADA Treatment Effects by Year",
  file.path(output_dir, "ada_time_series.png")
)

create_time_series_plot(
  dem_weighted,
  "Democrat Probability Effects by Year",
  file.path(output_dir, "dem_time_series.png")
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
  
  colors <- c("gray60", "#51A868", "#4A90E2")
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
  "ADA by Year: Treatment Effects with Different Weights",
  file.path(output_dir, "ada_year_forest_plot.png"),
  ada_baseline_TE
)

create_forest_plot(
  dem_pooled,
  "Democrat by Year: Treatment Effects with Different Weights",
  file.path(output_dir, "dem_year_forest_plot.png"),
  dem_baseline_TE
)


cat("- ada_by_year_comparison.png \n")
cat("- democrat_by_year_comparison.png \n")
cat("- ada_time_series.png \n")
cat("- dem_time_series.png \n")
cat("- ada_year_forest_plot.png \n")
cat("- dem_year_forest_plot.png \n")

