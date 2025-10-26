library(rdrobust)
library(dplyr)
library(ggplot2)

baseline <- read.csv("baseline_pooled_results.csv")

print(baseline[, c("Analysis", "TE", "SE", "CI_lower", "CI_upper")])
cat("\n")

data <- read.csv("enricoall2.csv")
data$rv <- data$lagdemvoteshare - 0.5
data_clean <- data %>% filter(!is.na(lagdemvoteshare) & !is.na(realada) & !is.na(democrat))

# p=2: ADA

bw_ada_p2 <- rdbwselect(y = data_clean$realada,
                        x = data_clean$rv,
                        c = 0, p = 2,
                        kernel = "triangular",
                        bwselect = "msetwo")

h_left_ada_p2 <- bw_ada_p2$bws[1, 1]
h_right_ada_p2 <- bw_ada_p2$bws[1, 2]

cat("optimal bandwidth (p=2): h_left =", sprintf("%.6f", h_left_ada_p2),
    ", h_right =", sprintf("%.6f", h_right_ada_p2), "\n")

rd_ada_p2 <- rdrobust(y = data_clean$realada,
                      x = data_clean$rv,
                      c = 0, p = 2,
                      kernel = "triangular",
                      h = c(h_left_ada_p2, h_right_ada_p2))

robustness_ada_p2 <- data.frame(
  Analysis = "ADA",
  Specification = "Polynomial p=2",
  p_order = 2,
  h_left = h_left_ada_p2,
  h_right = h_right_ada_p2,
  n_left = rd_ada_p2$N_h[1],
  n_right = rd_ada_p2$N_h[2],
  TE = rd_ada_p2$coef[1],
  SE = rd_ada_p2$se[3],
  CI_lower = rd_ada_p2$ci[3, 1],
  CI_upper = rd_ada_p2$ci[3, 2],
  p_value = rd_ada_p2$pv[3]
)

# p=2: Democrat

bw_dem_p2 <- rdbwselect(y = data_clean$democrat,
                        x = data_clean$rv,
                        c = 0, p = 2,
                        kernel = "triangular",
                        bwselect = "msetwo")

h_left_dem_p2 <- bw_dem_p2$bws[1, 1]
h_right_dem_p2 <- bw_dem_p2$bws[1, 2]

cat("optimal bandwidth (p=2): h_left =", sprintf("%.6f", h_left_dem_p2),
    ", h_right =", sprintf("%.6f", h_right_dem_p2), "\n")

rd_dem_p2 <- rdrobust(y = data_clean$democrat,
                      x = data_clean$rv,
                      c = 0, p = 2,
                      kernel = "triangular",
                      h = c(h_left_dem_p2, h_right_dem_p2))

robustness_dem_p2 <- data.frame(
  Analysis = "Democrat",
  Specification = "Polynomial p=2",
  p_order = 2,
  h_left = h_left_dem_p2,
  h_right = h_right_dem_p2,
  n_left = rd_dem_p2$N_h[1],
  n_right = rd_dem_p2$N_h[2],
  TE = rd_dem_p2$coef[1],
  SE = rd_dem_p2$se[3],
  CI_lower = rd_dem_p2$ci[3, 1],
  CI_upper = rd_dem_p2$ci[3, 2],
  p_value = rd_dem_p2$pv[3]
)


robustness_p2 <- rbind(robustness_ada_p2, robustness_dem_p2)
all_results <- rbind(baseline, robustness_p2)

comparison_table <- all_results %>%
  group_by(Analysis) %>%
  mutate(
    Diff_from_Baseline = TE - TE[Specification == "Baseline (p=1, optimal h)"],
    Pct_Diff = (Diff_from_Baseline / abs(TE[Specification == "Baseline (p=1, optimal h)"])) * 100
  ) %>%
  ungroup()


output_dir <- "."
if(!dir.exists(output_dir)) dir.create(output_dir)

write.csv(all_results, 
          file.path(output_dir, "polynomial_robustness_full.csv"), 
          row.names = FALSE)

write.csv(comparison_table, 
          file.path(output_dir, "polynomial_robustness_comparison.csv"), 
          row.names = FALSE)

print(comparison_table[, c("Analysis", "Specification", "TE", "SE", "Diff_from_Baseline", "Pct_Diff")])


plot_data <- comparison_table %>%
  select(Analysis, Specification, TE, SE, CI_lower, CI_upper, Diff_from_Baseline, Pct_Diff) %>%
  mutate(
    Spec_short = ifelse(grepl("Baseline", Specification), "p=1\n(Baseline)", "p=2"),
    Spec_order = ifelse(grepl("Baseline", Specification), 1, 2)
  ) %>%
  arrange(Analysis, Spec_order)

# ADA
ada_data <- plot_data %>% filter(Analysis == "ADA")

p_ada <- ggplot(ada_data, aes(x = factor(Spec_order), y = TE)) +
  geom_point(size = 5, color = "#2E86AB") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.15, size = 1.2, color = "#2E86AB") +
  geom_hline(yintercept = ada_data$TE[ada_data$Spec_order == 1], 
             linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = sprintf("%.3f", TE)), 
            vjust = -1.5, size = 5, fontface = "bold") +
  scale_x_discrete(labels = ada_data$Spec_short) +
  labs(x = "Specification", 
       y = "Treatment Effect (ADA Score)",
       title = "ADA: Polynomial Order Robustness Check",
       subtitle = "Comparing p=1 (baseline) vs p=2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  expand_limits(y = c(min(ada_data$CI_lower) * 0.9, max(ada_data$CI_upper) * 1.1))

ggsave(file.path(output_dir, "ada_polynomial_robustness.png"), 
       p_ada, width = 8, height = 6, dpi = 300)

# Democrat
dem_data <- plot_data %>% filter(Analysis == "Democrat")

p_dem <- ggplot(dem_data, aes(x = factor(Spec_order), y = TE)) +
  geom_point(size = 5, color = "#A23B72") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.15, size = 1.2, color = "#A23B72") +
  geom_hline(yintercept = dem_data$TE[dem_data$Spec_order == 1],
             linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = sprintf("%.3f", TE)),
            vjust = -1.5, size = 5, fontface = "bold") +
  scale_x_discrete(labels = dem_data$Spec_short) +
  labs(x = "Specification",
       y = "Treatment Effect (Probability)",
       title = "Democrat: Polynomial Order Robustness Check",
       subtitle = "Comparing p=1 (baseline) vs p=2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  expand_limits(y = c(min(dem_data$CI_lower) * 0.9, max(dem_data$CI_upper) * 1.1))

ggsave(file.path(output_dir, "dem_polynomial_robustness.png"),
       p_dem, width = 8, height = 6, dpi = 300)

summary_table <- comparison_table %>%
  select(Analysis, Specification, TE, SE, CI_lower, CI_upper, Diff_from_Baseline, Pct_Diff) %>%
  mutate(
    TE = sprintf("%.4f", TE),
    SE = sprintf("%.4f", SE),
    CI_lower = sprintf("%.4f", CI_lower),
    CI_upper = sprintf("%.4f", CI_upper),
    Diff_from_Baseline = sprintf("%.4f", Diff_from_Baseline),
    Pct_Diff = sprintf("%.2f%%", Pct_Diff)
  )

write.csv(summary_table, 
          file.path(output_dir, "polynomial_robustness_table.csv"), 
          row.names = FALSE)


