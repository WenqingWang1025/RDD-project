library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)


dem_state <- read.csv("democrat_by_state_detailed_results.csv", stringsAsFactors = FALSE)
dem_pop <- read.csv("pop_weighted_detailed_democrat_by_state.csv", stringsAsFactors = FALSE)
seats_data <- read.csv("seats_by_state_wide_1946_1992.csv", stringsAsFactors = FALSE)

plot_data <- dem_state %>%
  select(state, state_label, weight_state, TE, se) %>%
  left_join(
    dem_pop %>% select(State_Original, State_Name, State_Abbr, Avg_Population, Weight),
    by = c("state" = "State_Original")
  ) %>%
  left_join(
    seats_data %>% select(state, year_1990),
    by = "state"
  ) %>%
  rename(
    implicit_weight = weight_state,
    avg_population = Avg_Population,
    population_weight = Weight,
    seats_1990 = year_1990,
    state_abbr = State_Abbr
  ) %>%
  filter(!is.na(avg_population) & !is.na(seats_1990)) %>%
  mutate(
    pop_millions = avg_population / 1e6,
    seats_weight = seats_1990 / sum(seats_1990, na.rm = TRUE),
    implicit_weight_pct = implicit_weight * 100,
    population_weight_pct = population_weight * 100,
    deviation = implicit_weight - population_weight,
    deviation_pct = (deviation / population_weight) * 100,
    category = case_when(
      abs(deviation_pct) < 10 ~ "Balanced",
      deviation_pct >= 10 ~ "Over-weighted",
      deviation_pct <= -10 ~ "Under-weighted"
    )
  )


cor_pop <- cor(plot_data$pop_millions, plot_data$implicit_weight)
lm_pop <- lm(implicit_weight ~ pop_millions, data = plot_data)
bandwidth <- abs(coef(lm_pop)[1])

plot_data$pred_pop <- predict(lm_pop, newdata = plot_data)
plot_data$se_pred <- predict(lm_pop, newdata = plot_data, se.fit = TRUE)$se.fit
plot_data$ci_lower_pop <- (plot_data$pred_pop - 1.96 * plot_data$se_pred) * 100
plot_data$ci_upper_pop <- (plot_data$pred_pop + 1.96 * plot_data$se_pred) * 100

p1 <- ggplot(plot_data, aes(x = pop_millions, y = implicit_weight_pct)) +
  geom_ribbon(aes(ymin = ci_lower_pop, ymax = ci_upper_pop), 
              fill = "grey80", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#DC143C", 
              linetype = "dashed", linewidth = 1) +
  geom_point(aes(size = seats_1990, color = pop_millions), alpha = 0.7) +
  geom_text_repel(aes(label = state_abbr), size = 3, fontface = "bold", color = "black",
                  max.overlaps = 50, box.padding = 0.5, point.padding = 0.3) +
  scale_color_gradient(low = "#9370DB", high = "#4B0082", 
                       name = "Population\n(millions)", guide = "none") +
  scale_size_continuous(name = "House Seats\n(1990)", 
                        breaks = c(10, 30, 50), range = c(3, 15)) +
  labs(
    title = "Implicit RD Weights vs. State Population",
    subtitle = sprintf("Democrat Analysis | Correlation: %.3f (bandwidth = %.4f)", 
                       cor_pop, bandwidth),
    x = "Average Population (millions, 1946-1992)",
    y = "Implicit Weight in Pooled RDD"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 12)) +
  scale_x_continuous(limits = c(0, 26)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("dem_plot1_implicit_vs_population.png", p1, width = 10, height = 7, dpi = 300)

# Figure2

cor_seats <- cor(plot_data$seats_1990, plot_data$implicit_weight)
lm_seats <- lm(implicit_weight ~ seats_1990, data = plot_data)

plot_data$pred_seats <- predict(lm_seats, newdata = plot_data)
plot_data$se_pred_seats <- predict(lm_seats, newdata = plot_data, se.fit = TRUE)$se.fit
plot_data$ci_lower_seats <- (plot_data$pred_seats - 1.96 * plot_data$se_pred_seats) * 100
plot_data$ci_upper_seats <- (plot_data$pred_seats + 1.96 * plot_data$se_pred_seats) * 100

p2 <- ggplot(plot_data, aes(x = seats_1990, y = implicit_weight_pct)) +
  geom_ribbon(aes(ymin = ci_lower_seats, ymax = ci_upper_seats), 
              fill = "grey80", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#DC143C", 
              linetype = "dashed", linewidth = 1) +
  geom_point(aes(color = pop_millions), size = 5, alpha = 0.7) +
  geom_text_repel(aes(label = state_abbr), size = 3, fontface = "bold", color = "black",
                  max.overlaps = 50, box.padding = 0.5, point.padding = 0.3) +
  scale_color_gradient(low = "#BA55D3", high = "#8B008B", 
                       name = "Population\n(millions)") +
  labs(
    title = "Implicit RD Weights vs. Congressional Seats",
    subtitle = sprintf("Democrat Analysis | Correlation: %.3f", cor_seats),
    x = "Number of House Seats (1990)",
    y = "Implicit Weight in Pooled RDD"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 12.5)) +
  scale_x_continuous(limits = c(0, 55)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("dem_plot2_implicit_vs_seats.png", p2, width = 10, height = 7, dpi = 300)

# Figure3
cor_theory <- cor(plot_data$population_weight_pct, plot_data$implicit_weight_pct)

color_map <- c(
  "Balanced" = "#2ECC71",       
  "Over-weighted" = "#E74C3C", 
  "Under-weighted" = "#3498DB"
)

p3 <- ggplot(plot_data, aes(x = population_weight_pct, y = implicit_weight_pct)) +
  geom_abline(intercept = 0, slope = 1, color = "grey50", 
              linetype = "solid", linewidth = 1.2) +
  geom_point(aes(color = category, size = pop_millions), alpha = 0.8) +
  geom_text_repel(aes(label = state_abbr), size = 3, fontface = "bold", color = "black",
                  max.overlaps = 50, box.padding = 0.5, point.padding = 0.3) +
  scale_color_manual(values = color_map, name = "Deviation") +
  scale_size_continuous(name = "Population\n(millions)", range = c(3, 15)) +
  labs(
    title = "Implicit RD Weights vs. Population-Based Weights",
    subtitle = sprintf("Democrat Analysis | Correlation: %.3f | Gray line = perfect alignment", 
                       cor_theory),
    x = "Population-Based Weight (what it should be)",
    y = "Implicit Weight in Pooled RDD (what it actually is)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 11)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 11)) +
  coord_fixed(ratio = 1) +  # 1:1
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("dem_plot3_implicit_vs_theory.png", p3, width = 10, height = 7, dpi = 300)


print(table(plot_data$category))

print(plot_data %>% 
        filter(deviation > 0) %>% 
        arrange(desc(deviation)) %>% 
        select(state_abbr, State_Name, implicit_weight_pct, population_weight_pct, deviation_pct) %>%
        head(5))

print(plot_data %>% 
        filter(deviation < 0) %>% 
        arrange(deviation) %>% 
        select(state_abbr, State_Name, implicit_weight_pct, population_weight_pct, deviation_pct) %>%
        head(5))

