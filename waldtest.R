

library(dplyr)

perform_wald_from_results <- function(results_data, test_name, output_file) {
  

  TE <- as.numeric(results_data$TE)
  SE <- as.numeric(results_data$se)
  n_groups <- length(TE)
  
  
  weights <- 1 / (SE^2)
  TE_pooled <- sum(TE * weights) / sum(weights)
  
  cat("weight average TE:", sprintf("%.4f", TE_pooled), "\n\n")
  
  # Cochran's Q 
  Q <- sum(weights * (TE - TE_pooled)^2)
  df <- n_groups - 1
  p_value <- pchisq(Q, df, lower.tail = FALSE)
  
  # I
  I_squared <- max(0, 100 * (Q - df) / Q)
  
  
  if (p_value < 0.001) {
    cat("  *** p < 0.001ï¼Œvery high heterogeneity\n")
  } else if (p_value < 0.01) {
    cat("  ** : p < 0.01ï¼Œhigh heterogeneity\n")
  } else if (p_value < 0.05) {
    cat("  * : p < 0.05ï¼Œexist heterogeneity\n")
  } else {
    cat("  : p >= 0.05ï¼Œno heterogeneity\n")
  }
  
  cat("\n=== IÂ² ï¼‰===\n")
  cat("  IÂ² =", sprintf("%.2f%%", I_squared), "\n")
  
  if (I_squared < 25) {
    cat("  low heterogeneity\n")
  } else if (I_squared < 50) {
    cat("  middle heterogeneity\n")
  } else if (I_squared < 75) {
    cat("  high heterogeneity\n")
  } else {
    cat("  very high heterogeneity\n")
  }
  
  results_df <- data.frame(
    Test = test_name,
    N_Groups = n_groups,
    Q_statistic = Q,
    df = df,
    p_value = p_value,
    I_squared = I_squared,
    TE_pooled = TE_pooled,
    TE_sd = sd(TE),
    TE_range = max(TE) - min(TE),
    Significant = ifelse(p_value < 0.05, "Yes", "No")
  )
  
  write.csv(results_df, output_file, row.names = FALSE)
  cat("\n results save:", output_file, "\n")
  
  return(results_df)
}


ada_year <- read.csv("ada_by_year_detailed_results.csv")
ada_state <- read.csv("ada_by_state_detailed_results.csv")
dem_year <- read.csv("democrat_by_year_detailed_results.csv")
dem_state <- read.csv("democrat_by_state_detailed_results.csv")


output_dir <- "."

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. ADA by Year
cat("\n" , rep("=", 60), "\n", sep="")
cat("1/4: ADAæŒ‰å¹´ä»½å¼‚è´¨æ€§æ£€éªŒ\n")
cat(rep("=", 60), "\n", sep="")

ada_year_results <- perform_wald_from_results(
  results_data = ada_year,
  test_name = "ADA by Year",
  output_file = file.path(output_dir, "wald_ada_by_year.csv")
)

# 2. ADA by State

ada_state_results <- perform_wald_from_results(
  results_data = ada_state,
  test_name = "ADA by State",
  output_file = file.path(output_dir, "wald_ada_by_state.csv")
)

# 3. Democrat by Year

dem_year_results <- perform_wald_from_results(
  results_data = dem_year,
  test_name = "Democrat by Year",
  output_file = file.path(output_dir, "wald_dem_by_year.csv")
)

# 4. Democrat by State  

dem_state_results <- perform_wald_from_results(
  results_data = dem_state,
  test_name = "Democrat by State",
  output_file = file.path(output_dir, "wald_dem_by_state.csv")
)


all_results <- rbind(
  ada_year_results,
  ada_state_results,
  dem_year_results,
  dem_state_results
)

print(all_results)

write.csv(all_results, 
          file.path(output_dir, "wald_all_tests_summary.csv"),
          row.names = FALSE)

