library(rdrobust)
library(dplyr)
library(ggplot2)

baseline <- read.csv("baseline_pooled_results.csv")

print(baseline[, c("Analysis", "h_left", "h_right", "TE", "SE")])

data <- read.csv("enricoall2.csv")
data$rv <- data$lagdemvoteshare - 0.5
data_clean <- data %>% filter(!is.na(lagdemvoteshare) & !is.na(realada) & !is.na(democrat))

h_left_ada_baseline <- baseline$h_left[baseline$Analysis == "ADA"]
h_right_ada_baseline <- baseline$h_right[baseline$Analysis == "ADA"]

# -10%
h_left_ada_90 <- h_left_ada_baseline * 0.9
h_right_ada_90 <- h_right_ada_baseline * 0.9

# +10%
h_left_ada_110 <- h_left_ada_baseline * 1.1
h_right_ada_110 <- h_right_ada_baseline * 1.1

cat(" -10%:     h_left =", sprintf("%.6f", h_left_ada_90),
    ", h_right =", sprintf("%.6f", h_right_ada_90), "\n")
cat("Baseline:      h_left =", sprintf("%.6f", h_left_ada_baseline),
    ", h_right =", sprintf("%.6f", h_right_ada_baseline), "\n")
cat(" +10%:     h_left =", sprintf("%.6f", h_left_ada_110),
    ", h_right =", sprintf("%.6f", h_right_ada_110), "\n\n")

# -10%
rd_ada_bw90 <- rdrobust(y = data_clean$realada,
                        x = data_clean$rv,
                        c = 0, p = 1,
                        kernel = "triangular",
                        h = c(h_left_ada_90, h_right_ada_90))

robustness_ada_bw90 <- data.frame(
  Analysis = "ADA",
  Specification = "Bandwidth -10%",
  p_order = 1,
  h_left = h_left_ada_90,
  h_right = h_right_ada_90,
  n_left = rd_ada_bw90$N_h[1],
  n_right = rd_ada_bw90$N_h[2],
  TE = rd_ada_bw90$coef[1],
  SE = rd_ada_bw90$se[3],
  CI_lower = rd_ada_bw90$ci[3, 1],
  CI_upper = rd_ada_bw90$ci[3, 2],
  p_value = rd_ada_bw90$pv[3]
)

# +10%
rd_ada_bw110 <- rdrobust(y = data_clean$realada,
                         x = data_clean$rv,
                         c = 0, p = 1,
                         kernel = "triangular",
                         h = c(h_left_ada_110, h_right_ada_110))

robustness_ada_bw110 <- data.frame(
  Analysis = "ADA",
  Specification = "Bandwidth +10%",
  p_order = 1,
  h_left = h_left_ada_110,
  h_right = h_right_ada_110,
  n_left = rd_ada_bw110$N_h[1],
  n_right = rd_ada_bw110$N_h[2],
  TE = rd_ada_bw110$coef[1],
  SE = rd_ada_bw110$se[3],
  CI_lower = rd_ada_bw110$ci[3, 1],
  CI_upper = rd_ada_bw110$ci[3, 2],
  p_value = rd_ada_bw110$pv[3]
)

# Democrat

h_left_dem_baseline <- baseline$h_left[baseline$Analysis == "Democrat"]
h_right_dem_baseline <- baseline$h_right[baseline$Analysis == "Democrat"]

# -10%
h_left_dem_90 <- h_left_dem_baseline * 0.9
h_right_dem_90 <- h_right_dem_baseline * 0.9

# +10%
h_left_dem_110 <- h_left_dem_baseline * 1.1
h_right_dem_110 <- h_right_dem_baseline * 1.1

cat(" -10%:     h_left =", sprintf("%.6f", h_left_dem_90),
    ", h_right =", sprintf("%.6f", h_right_dem_90), "\n")
cat("Baseline:      h_left =", sprintf("%.6f", h_left_dem_baseline),
    ", h_right =", sprintf("%.6f", h_right_dem_baseline), "\n")
cat(" +10%:     h_left =", sprintf("%.6f", h_left_dem_110),
    ", h_right =", sprintf("%.6f", h_right_dem_110), "\n\n")

# -10%
rd_dem_bw90 <- rdrobust(y = data_clean$democrat,
                        x = data_clean$rv,
                        c = 0, p = 1,
                        kernel = "triangular",
                        h = c(h_left_dem_90, h_right_dem_90))

robustness_dem_bw90 <- data.frame(
  Analysis = "Democrat",
  Specification = "Bandwidth -10%",
  p_order = 1,
  h_left = h_left_dem_90,
  h_right = h_right_dem_90,
  n_left = rd_dem_bw90$N_h[1],
  n_right = rd_dem_bw90$N_h[2],
  TE = rd_dem_bw90$coef[1],
  SE = rd_dem_bw90$se[3],
  CI_lower = rd_dem_bw90$ci[3, 1],
  CI_upper = rd_dem_bw90$ci[3, 2],
  p_value = rd_dem_bw90$pv[3]
)

# +10%
rd_dem_bw110 <- rdrobust(y = data_clean$democrat,
                         x = data_clean$rv,
                         c = 0, p = 1,
                         kernel = "triangular",
                         h = c(h_left_dem_110, h_right_dem_110))

robustness_dem_bw110 <- data.frame(
  Analysis = "Democrat",
  Specification = "Bandwidth +10%",
  p_order = 1,
  h_left = h_left_dem_110,
  h_right = h_right_dem_110,
  n_left = rd_dem_bw110$N_h[1],
  n_right = rd_dem_bw110$N_h[2],
  TE = rd_dem_bw110$coef[1],
  SE = rd_dem_bw110$se[3],
  CI_lower = rd_dem_bw110$ci[3, 1],
  CI_upper = rd_dem_bw110$ci[3, 2],
  p_value = rd_dem_bw110$pv[3]
)

robustness_bw <- rbind(robustness_ada_bw90, robustness_ada_bw110,
                       robustness_dem_bw90, robustness_dem_bw110)

all_results <- rbind(baseline, robustness_bw)

all_results <- all_results %>%
  arrange(Analysis, 
          factor(Specification, 
                 levels = c("Bandwidth -10%", "Baseline (p=1, optimal h)", "Bandwidth +10%")))

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
          file.path(output_dir, "bandwidth_robustness_full.csv"), 
          row.names = FALSE)

write.csv(comparison_table, 
          file.path(output_dir, "bandwidth_robustness_comparison.csv"), 
          row.names = FALSE)

print(comparison_table[, c("Analysis", "Specification", "TE", "SE", "Diff_from_Baseline", "Pct_Diff")])

plot_data <- comparison_table %>%
  select(Analysis, Specification, TE, SE, CI_lower, CI_upper, 
         h_left, h_right, n_left, n_right, Diff_from_Baseline, Pct_Diff) %>%
  mutate(
    Spec_short = case_when(
      grepl("-10%", Specification) ~ "h - 10%",
      grepl("Baseline", Specification) ~ "Optimal h\n(Baseline)",
      grepl("\\+10%", Specification) ~ "h + 10%"
    ),
    Spec_order = case_when(
      grepl("-10%", Specification) ~ 1,
      grepl("Baseline", Specification) ~ 2,
      grepl("\\+10%", Specification) ~ 3
    )
  ) %>%
  arrange(Analysis, Spec_order)

# ADA
ada_data <- plot_data %>% filter(Analysis == "ADA")

p_ada <- ggplot(ada_data, aes(x = factor(Spec_order), y = TE)) +
  geom_point(size = 5, color = "#2E86AB") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.15, size = 1.2, color = "#2E86AB") +
  geom_line(aes(group = 1), color = "#2E86AB", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = ada_data$TE[ada_data$Spec_order == 2], 
             linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = sprintf("%.3f", TE)), 
            vjust = -1.5, size = 4.5, fontface = "bold") +
  scale_x_discrete(labels = ada_data$Spec_short) +
  labs(x = "Bandwidth Specification", 
       y = "Treatment Effect (ADA Score)",
       title = "ADA: Bandwidth Robustness Check",
       subtitle = sprintf("Baseline bandwidth: [%.4f, %.4f]", 
                          ada_data$h_left[2], ada_data$h_right[2])) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  expand_limits(y = c(min(ada_data$CI_lower) * 0.9, max(ada_data$CI_upper) * 1.1))

ggsave(file.path(output_dir, "ada_bandwidth_robustness.png"), 
       p_ada, width = 10, height = 6, dpi = 300)

# Democrat
dem_data <- plot_data %>% filter(Analysis == "Democrat")

p_dem <- ggplot(dem_data, aes(x = factor(Spec_order), y = TE)) +
  geom_point(size = 5, color = "#A23B72") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.15, size = 1.2, color = "#A23B72") +
  geom_line(aes(group = 1), color = "#A23B72", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = dem_data$TE[dem_data$Spec_order == 2],
             linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = sprintf("%.3f", TE)),
            vjust = -1.5, size = 4.5, fontface = "bold") +
  scale_x_discrete(labels = dem_data$Spec_short) +
  labs(x = "Bandwidth Specification",
       y = "Treatment Effect (Probability)",
       title = "Democrat: Bandwidth Robustness Check",
       subtitle = sprintf("Baseline bandwidth: [%.4f, %.4f]",
                          dem_data$h_left[2], dem_data$h_right[2])) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  expand_limits(y = c(min(dem_data$CI_lower) * 0.9, max(dem_data$CI_upper) * 1.1))

ggsave(file.path(output_dir, "dem_bandwidth_robustness.png"),
       p_dem, width = 10, height = 6, dpi = 300)

summary_table <- comparison_table %>%
  select(Analysis, Specification, h_left, h_right, n_left, n_right,
         TE, SE, CI_lower, CI_upper, Diff_from_Baseline, Pct_Diff) %>%
  mutate(
    h_left = sprintf("%.6f", h_left),
    h_right = sprintf("%.6f", h_right),
    TE = sprintf("%.4f", TE),
    SE = sprintf("%.4f", SE),
    CI_lower = sprintf("%.4f", CI_lower),
    CI_upper = sprintf("%.4f", CI_upper),
    Diff_from_Baseline = sprintf("%.4f", Diff_from_Baseline),
    Pct_Diff = sprintf("%.2f%%", Pct_Diff)
  )

write.csv(summary_table, 
          file.path(output_dir, "bandwidth_robustness_table.csv"), 
          row.names = FALSE)

