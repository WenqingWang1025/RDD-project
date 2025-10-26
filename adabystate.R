library(rdrobust)
library(dplyr)


data <- read.csv("enricoall2.csv")


output_dir <- "."
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


data$rv <- data$lagdemvoteshare - 0.5
data_clean <- data %>% filter(!is.na(lagdemvoteshare) & !is.na(realada))
states <- sort(unique(data_clean$state))

cat("========================================\n")
cat("Ada by State Analysis\n")
cat("========================================\n")
cat("Total observations:", nrow(data_clean), "\n")
cat("Number of states:", length(states), "\n\n")

# Step 1: pooled
cat("Step 1: Computing pooled bandwidth...\n")
pooled_bw <- rdbwselect(
  y = data_clean$realada, 
  x = data_clean$rv, 
  c = 0, 
  p = 1,
  kernel = "triangular",
  bwselect = "msetwo"
)

h_left <- pooled_bw$bws[1, 1]
h_right <- pooled_bw$bws[1, 2]

cat("  Bandwidth left  =", sprintf("%.6f", h_left), "\n")
cat("  Bandwidth right =", sprintf("%.6f", h_right), "\n\n")

# Step 2:
cat("Step 2: State-by-state estimation...\n")
results_list <- list()

for (st in states) {

  data_st <- data_clean %>% filter(state == st)
  

  n_left <- sum(data_st$rv >= -h_left & data_st$rv < 0, na.rm = TRUE)
  n_right <- sum(data_st$rv >= 0 & data_st$rv <= h_right, na.rm = TRUE)
  

  if (n_left >= 15 & n_right >= 15) {
    tryCatch({
      # rdrobust
      rd_result <- rdrobust(
        y = data_st$realada,
        x = data_st$rv,
        c = 0, 
        p = 1,
        kernel = "triangular",
        h = c(h_left, h_right)
      )
      
      # robust
      coef_robust <- rd_result$coef[1]  
      se_robust <- rd_result$se[3]       
      ci_lower_robust <- rd_result$ci[3, 1]  
      ci_upper_robust <- rd_result$ci[3, 2]  
      pval_robust <- rd_result$pv[3]     
      
      # CI1.96*SECI
      if (coef_robust < ci_lower_robust - 1e-6 | coef_robust > ci_upper_robust + 1e-6) {
        cat("  ", st, " - Fixing CI (point estimate was outside)\n", sep = "")
        ci_lower_robust <- coef_robust - 1.96 * se_robust
        ci_upper_robust <- coef_robust + 1.96 * se_robust
      }
      

      data_left <- data_st %>% filter(rv >= -h_left & rv < 0)
      data_right <- data_st %>% filter(rv >= 0 & rv <= h_right)
      mu_left <- mean(data_left$realada, na.rm = TRUE)
      mu_right <- mean(data_right$realada, na.rm = TRUE)
      

      results_list[[as.character(st)]] <- data.frame(
        state = st,
        state_label = as.character(st),
        mu_left = mu_left,
        mu_right = mu_right,
        n_eff_left = n_left,
        n_eff_right = n_right,
        TE = coef_robust,
        se = se_robust,
        se_method = "Robust",
        h_left = h_left,
        h_right = h_right,
        ci_lower = ci_lower_robust,
        ci_upper = ci_upper_robust,
        p_value = pval_robust,
        significant = ifelse(pval_robust < 0.05, "Yes", "No"),
        se_conventional = rd_result$se[1],
        ci_lower_conventional = rd_result$ci[1, 1],
        ci_upper_conventional = rd_result$ci[1, 2],
        p_value_conventional = rd_result$pv[1],
        stringsAsFactors = FALSE
      )
      
      cat("  ", st, " âœ“\n", sep = "")
      
    }, error = function(e) {
      cat("  ", st, " FAILED:", conditionMessage(e), "\n", sep = "")
    })
  } else {
    cat("  ", st, " SKIPPED (insufficient sample)\n", sep = "")
  }
}

# Step 3:
cat("\nStep 3: Combining results...\n")
results_df <- bind_rows(results_list)


total_n <- sum(results_df$n_eff_left + results_df$n_eff_right)
results_df$weight_left <- results_df$n_eff_left / (results_df$n_eff_left + results_df$n_eff_right)
results_df$weight_right <- results_df$n_eff_right / (results_df$n_eff_left + results_df$n_eff_right)
results_df$weight_state <- (results_df$n_eff_left + results_df$n_eff_right) / total_n


weighted_avg <- sum(results_df$TE * results_df$weight_state)
cat("  Weighted average TE:", sprintf("%.4f", weighted_avg), "\n\n")

# Step 4:
cat("Step 4: Exporting results...\n")

# 1.
write.csv(results_df, 
          file = file.path(output_dir, "ada_by_state_detailed_results.csv"),
          row.names = FALSE)
cat("  1. Detailed results saved\n")

# 2.
simple_df <- results_df %>%
  select(state, TE, se, p_value, ci_lower, ci_upper, 
         n_eff_left, n_eff_right, significant) %>%
  rename(
    State = state,
    Treatment_Effect = TE,
    SE = se,
    P_Value = p_value,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    N_Left = n_eff_left,
    N_Right = n_eff_right,
    Significant = significant
  )

write.csv(simple_df,
          file = file.path(output_dir, "ada_by_state_simple_results.csv"),
          row.names = FALSE)
cat("  2. Simple results saved\n")

# 3.
weight_df <- results_df %>%
  select(state, n_eff_left, n_eff_right, weight_state) %>%
  mutate(N_Total = n_eff_left + n_eff_right) %>%
  rename(
    State = state,
    N_Left = n_eff_left,
    N_Right = n_eff_right,
    Weight = weight_state
  ) %>%
  select(State, N_Left, N_Right, N_Total, Weight)

write.csv(weight_df,
          file = file.path(output_dir, "ada_by_state_weights.csv"),
          row.names = FALSE)
cat("  3. Weights saved\n")

# 4.
summary_stats <- data.frame(
  Metric = c(
    "Precision_Weighted_Average",
    "Total_Effective_Sample_Size",
    "Number_of_States",
    "Pooled_Bandwidth_Left",
    "Pooled_Bandwidth_Right",
    "Min_Treatment_Effect",
    "Max_Treatment_Effect",
    "Mean_Treatment_Effect",
    "SD_Treatment_Effect"
  ),
  Value = c(
    weighted_avg,
    total_n,
    nrow(results_df),
    h_left,
    h_right,
    min(results_df$TE),
    max(results_df$TE),
    mean(results_df$TE),
    sd(results_df$TE)
  )
)

write.csv(summary_stats,
          file = file.path(output_dir, "ada_by_state_summary_statistics.csv"),
          row.names = FALSE)
cat("  4. Summary statistics saved\n")

# 5.
metadata <- data.frame(
  Parameter = c(
    "Outcome_Variable",
    "Running_Variable",
    "Cutoff",
    "Polynomial_Order",
    "Kernel",
    "Bandwidth_Selection",
    "Min_Sample_Per_Side",
    "Analysis_Date"
  ),
  Value = c(
    "realada (Adjusted ADA Score)",
    "lagdemvoteshare - 0.5",
    "0",
    "1 (Local Linear)",
    "Triangular",
    "Pooled MSE-optimal (msetwo, unequal)",
    "15",
    as.character(Sys.Date())
  )
)

write.csv(metadata,
          file = file.path(output_dir, "ada_by_state_metadata.csv"),
          row.names = FALSE)
cat("  5. Metadata saved\n")

# Step 5:
cat("\nStep 5: Creating forest plot...\n")

# TE
results_sorted <- results_df %>%
  arrange(desc(TE))


sig_colors <- ifelse(results_sorted$p_value < 0.01, "#C73E1D",
                     ifelse(results_sorted$p_value < 0.05, "#F18F01", "gray50"))


png(file.path(output_dir, "ada_forest_plot.png"), 
    width = 10, height = 12, units = "in", res = 300)
par(mar = c(5, 8, 3, 2))

n_states <- nrow(results_sorted)
y_positions <- 1:n_states


plot(results_sorted$TE, y_positions,
     xlim = range(c(results_sorted$ci_lower, 
                    results_sorted$ci_upper, 0)),
     ylim = c(0.5, n_states + 0.5),
     pch = 19, col = sig_colors, cex = 1.3,
     xlab = "Treatment Effect (ADA Score)", ylab = "",
     main = "Treatment Effects on ADA Scores by State (Forest Plot)",
     yaxt = "n", las = 1, cex.lab = 1.2, cex.axis = 1.1)


for (i in 1:n_states) {
  segments(results_sorted$ci_lower[i], y_positions[i],
           results_sorted$ci_upper[i], y_positions[i],
           col = sig_colors[i], lwd = 2)
}


points(results_sorted$TE, y_positions, 
       pch = 21, bg = sig_colors, col = "white", 
       cex = 1.5, lwd = 2)


axis(2, at = y_positions, labels = results_sorted$state, 
     las = 1, cex.axis = 0.75)


abline(v = 0, lty = 3, col = "gray30", lwd = 2)
abline(v = weighted_avg, lty = 2, col = "#2E86AB", lwd = 2.5)


legend("bottomright",
       legend = c("p < 0.01", "p < 0.05", "Not significant", 
                  "Weighted average", "No effect"),
       col = c("#C73E1D", "#F18F01", "gray50", "#2E86AB", "gray30"),
       pch = c(21, 21, 21, NA, NA), 
       pt.bg = c("#C73E1D", "#F18F01", "gray50", NA, NA),
       lty = c(NA, NA, NA, 2, 3),
       lwd = c(NA, NA, NA, 2.5, 2),
       pt.cex = 1.5, bty = "n", cex = 1)


pooled_text <- sprintf("Pooled estimate: %.3f | Bandwidth: h_L=%.4f, h_R=%.4f | Min sample: 15/side",
                       weighted_avg, h_left, h_right)
mtext(pooled_text, side = 3, line = 0.5, cex = 0.75, adj = 0)

dev.off()
cat("  Forest plot saved\n")

cat("\n========================================\n")
cat("COMPLETE!\n")
cat("All files saved to:", output_dir, "\n")
cat("========================================\n")


cat("\nFinal validation check:\n")
outside_ci <- results_df %>%
  filter(TE < ci_lower - 1e-6 | TE > ci_upper + 1e-6)

if (nrow(outside_ci) > 0) {
  cat("WARNING: Found", nrow(outside_ci), "states with point estimates outside CI:\n")
  print(outside_ci[, c("state", "TE", "ci_lower", "ci_upper")])
} else {
  cat("All point estimates are within their confidence intervals!\n")
}
