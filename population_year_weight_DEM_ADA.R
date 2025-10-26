library(dplyr)

ada_year <- read.csv("ada_by_year_detailed_results.csv")
dem_year <- read.csv("democrat_by_year_detailed_results.csv")
pop_data <- read.csv("state_population_even_years_1946_1992.csv")

national_pop <- pop_data %>%
  group_by(year) %>%
  summarise(total_population = sum(population, na.rm = TRUE))


compute_population_weighted <- function(results_data, pop_data, name, output_dir) {
  
  TE <- as.numeric(results_data$TE)
  SE <- as.numeric(results_data$se)
  Year <- results_data$year
  
  merged <- data.frame(year = Year, TE = TE, SE = SE) %>%
    left_join(pop_data, by = "year")
  
  weight <- merged$total_population / sum(merged$total_population, na.rm = TRUE)
  pooled_estimate <- sum(TE * weight, na.rm = TRUE)
  pooled_se <- sqrt(sum((weight * SE)^2, na.rm = TRUE))
  

  result_summary <- data.frame(
    Analysis = name,
    Pooled_Estimate = pooled_estimate,
    SE = pooled_se,
    CI_Lower = pooled_estimate - 1.96*pooled_se,
    CI_Upper = pooled_estimate + 1.96*pooled_se
  )
  
  detailed_weights <- data.frame(
    Year = merged$year,
    TE = TE,
    SE = SE,
    Population = merged$total_population,
    Weight = weight
  )
  
  write.csv(result_summary, 
            file.path(output_dir, paste0("population_weighted_summary_", gsub(" ", "_", tolower(name)), ".csv")),
            row.names = FALSE)
  
  write.csv(detailed_weights,
            file.path(output_dir, paste0("population_weighted_detailed_", gsub(" ", "_", tolower(name)), ".csv")),
            row.names = FALSE)
  
  return(list(estimate = pooled_estimate, se = pooled_se, weights = detailed_weights))
}


output_dir <- "."

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

ada_result <- compute_population_weighted(ada_year, national_pop, "ADA by Year", output_dir)
dem_result <- compute_population_weighted(dem_year, national_pop, "Democrat by Year", output_dir)


# Figure1
png(file.path(output_dir, "plot_year_weights_combined.png"), 
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# ADA by Year
ada_weights <- ada_result$weights
plot(ada_weights$Year, ada_weights$TE, 
     type = "n",
     ylim = range(c(ada_weights$TE - 1.96*ada_weights$SE, 
                    ada_weights$TE + 1.96*ada_weights$SE)),
     xlab = "Year", ylab = "Treatment Effect (ADA Score)",
     main = "ADA by Year: Treatment Effects\n(Population Weighted)",
     cex.lab = 1.2, cex.main = 1.3)

polygon(c(ada_weights$Year, rev(ada_weights$Year)),
        c(ada_weights$TE - 1.96*ada_weights$SE, 
          rev(ada_weights$TE + 1.96*ada_weights$SE)),
        col = rgb(0.18, 0.53, 0.67, 0.3), border = NA)

point_sizes <- ada_weights$Weight * 100 * 3
lines(ada_weights$Year, ada_weights$TE, col = "#2E86AB", lwd = 2)
points(ada_weights$Year, ada_weights$TE, 
       pch = 21, bg = "#2E86AB", col = "white", 
       cex = point_sizes, lwd = 1.5)

abline(h = 0, lty = 2, col = "red")
abline(h = ada_result$estimate, lty = 2, col = "darkgreen", lwd = 2)

legend("topright", 
       legend = c("Point Estimate", "95% CI", "Weighted Average", "No Effect"),
       col = c("#2E86AB", "#2E86AB", "darkgreen", "red"),
       lty = c(1, 0, 2, 2), lwd = c(2, 0, 2, 1),
       pch = c(21, 15, NA, NA),
       pt.bg = c("#2E86AB", "#2E86AB", NA, NA),
       bty = "n", cex = 0.9)

# Democrat by Year
dem_weights <- dem_result$weights
plot(dem_weights$Year, dem_weights$TE, 
     type = "n",
     ylim = range(c(dem_weights$TE - 1.96*dem_weights$SE, 
                    dem_weights$TE + 1.96*dem_weights$SE)),
     xlab = "Year", ylab = "Treatment Effect (Probability)",
     main = "Democrat by Year: Treatment Effects\n(Population Weighted)",
     cex.lab = 1.2, cex.main = 1.3)

polygon(c(dem_weights$Year, rev(dem_weights$Year)),
        c(dem_weights$TE - 1.96*dem_weights$SE, 
          rev(dem_weights$TE + 1.96*dem_weights$SE)),
        col = rgb(0.64, 0.23, 0.45, 0.3), border = NA)

point_sizes <- dem_weights$Weight * 100 * 3
lines(dem_weights$Year, dem_weights$TE, col = "#A23B72", lwd = 2)
points(dem_weights$Year, dem_weights$TE, 
       pch = 21, bg = "#A23B72", col = "white", 
       cex = point_sizes, lwd = 1.5)

abline(h = 0, lty = 2, col = "red")
abline(h = dem_result$estimate, lty = 2, col = "darkgreen", lwd = 2)

legend("topright", 
       legend = c("Point Estimate", "95% CI", "Weighted Average", "No Effect"),
       col = c("#A23B72", "#A23B72", "darkgreen", "red"),
       lty = c(1, 0, 2, 2), lwd = c(2, 0, 2, 1),
       pch = c(21, 15, NA, NA),
       pt.bg = c("#A23B72", "#A23B72", NA, NA),
       bty = "n", cex = 0.9)

dev.off()

# Figure2
png(file.path(output_dir, "plot_weight_distribution_over_time.png"), 
    width = 12, height = 6, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# ADA
barplot(ada_weights$Weight * 100,
        names.arg = ada_weights$Year,
        col = "#2E86AB",
        main = "ADA by Year: Population Weight Distribution",
        xlab = "Year",
        ylab = "Weight (%)",
        cex.names = 0.8,
        las = 2)

# Democrat
barplot(dem_weights$Weight * 100,
        names.arg = dem_weights$Year,
        col = "#A23B72",
        main = "Democrat by Year: Population Weight Distribution",
        xlab = "Year",
        ylab = "Weight (%)",
        cex.names = 0.8,
        las = 2)

dev.off()


