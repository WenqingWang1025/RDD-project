rm(list = ls())

library(rdrobust)
library(dplyr)



data <- read.csv("enricoall2.csv")
data$rv <- data$lagdemvoteshare - 0.5
data_clean <- data %>% filter(!is.na(lagdemvoteshare) & !is.na(realada) & !is.na(democrat))



# ========================================
# Baseline 1: ADA
# ========================================

cat(">>> BASELINE - ADA <<<\n")


bw_ada <- rdbwselect(y = data_clean$realada, 
                     x = data_clean$rv, 
                     c = 0, p = 1,
                     kernel = "triangular",
                     bwselect = "msetwo")

h_left_ada <- bw_ada$bws[1, 1]
h_right_ada <- bw_ada$bws[1, 2]

cat("optiomal bandwidth: h_left =", sprintf("%.3f", h_left_ada), 
    ", h_right =", sprintf("%.3f", h_right_ada), "\n")

rd_ada <- rdrobust(y = data_clean$realada,
                   x = data_clean$rv,
                   c = 0, p = 1,
                   kernel = "triangular",
                   h = c(h_left_ada, h_right_ada))

baseline_ada <- data.frame(
  Analysis = "ADA",
  Specification = "Baseline (p=1, optimal h)",
  p_order = 1,
  h_left = rd_ada$bws[1, 1],
  h_right = rd_ada$bws[1, 2],
  n_left = rd_ada$N_h[1],
  n_right = rd_ada$N_h[2],
  TE = rd_ada$coef[1],
  SE = rd_ada$se[3],
  CI_lower = rd_ada$ci[3, 1],
  CI_upper = rd_ada$ci[3, 2],
  p_value = rd_ada$pv[3]
)

cat("  TE =", sprintf("%.3f", baseline_ada$TE), "\n")
cat("  SE =", sprintf("%.3f", baseline_ada$SE), "\n")
cat("  95% CI: [", sprintf("%.3f", baseline_ada$CI_lower), 
    ", ", sprintf("%.3f", baseline_ada$CI_upper), "]\n", sep="")
cat("  p-value =", sprintf("%.3f", baseline_ada$p_value), "\n")
cat("  N_left =", baseline_ada$n_left, ", N_right =", baseline_ada$n_right, "\n\n")

# ========================================
# Baseline 2: Democrat
# ========================================

cat(">>> BASELINE - Democrat <<<\n")

bw_dem <- rdbwselect(y = data_clean$democrat,
                     x = data_clean$rv,
                     c = 0, p = 1,
                     kernel = "triangular",
                     bwselect = "msetwo")

h_left_dem <- bw_dem$bws[1, 1]
h_right_dem <- bw_dem$bws[1, 2]

cat("optimal bandwidth: h_left =", sprintf("%.3f", h_left_dem),
    ", h_right =", sprintf("%.3f", h_right_dem), "\n")


rd_dem <- rdrobust(y = data_clean$democrat,
                   x = data_clean$rv,
                   c = 0, p = 1,
                   kernel = "triangular",
                   h = c(h_left_dem, h_right_dem))


baseline_dem <- data.frame(
  Analysis = "Democrat",
  Specification = "Baseline (p=1, optimal h)",
  p_order = 1,
  h_left = rd_dem$bws[1, 1],
  h_right = rd_dem$bws[1, 2],
  n_left = rd_dem$N_h[1],
  n_right = rd_dem$N_h[2],
  TE = rd_dem$coef[1],
  SE = rd_dem$se[3],
  CI_lower = rd_dem$ci[3, 1],
  CI_upper = rd_dem$ci[3, 2],
  p_value = rd_dem$pv[3]
)


cat("  TE =", sprintf("%.3f", baseline_dem$TE), "\n")
cat("  SE =", sprintf("%.3f", baseline_dem$SE), "\n")
cat("  95% CI: [", sprintf("%.3f", baseline_dem$CI_lower),
    ", ", sprintf("%.3f", baseline_dem$CI_upper), "]\n", sep="")
cat("  p-value =", sprintf("%.3f", baseline_dem$p_value), "\n")
cat("  N_left =", baseline_dem$n_left, ", N_right =", baseline_dem$n_right, "\n\n")

# save result

output_dir <- "."
if(!dir.exists(output_dir)) dir.create(output_dir)

# export 3 decimal
baseline_results_export <- data.frame(
  Analysis = c(baseline_ada$Analysis, baseline_dem$Analysis),
  Specification = c(baseline_ada$Specification, baseline_dem$Specification),
  p_order = c(baseline_ada$p_order, baseline_dem$p_order),
  h_left = round(c(baseline_ada$h_left, baseline_dem$h_left), 3),
  h_right = round(c(baseline_ada$h_right, baseline_dem$h_right), 3),
  n_left = c(baseline_ada$n_left, baseline_dem$n_left),
  n_right = c(baseline_ada$n_right, baseline_dem$n_right),
  TE = round(c(baseline_ada$TE, baseline_dem$TE), 3),
  SE = round(c(baseline_ada$SE, baseline_dem$SE), 3),
  CI_lower = round(c(baseline_ada$CI_lower, baseline_dem$CI_lower), 3),
  CI_upper = round(c(baseline_ada$CI_upper, baseline_dem$CI_upper), 3),
  p_value = round(c(baseline_ada$p_value, baseline_dem$p_value), 3)
)

write.csv(baseline_results_export, 
          file.path(output_dir, "baseline_pooled_results.csv"), 
          row.names = FALSE)

print(baseline_results_export[, c("Analysis", "TE", "SE", "CI_lower", "CI_upper", "p_value")])


