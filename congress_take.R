library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


data <- read_csv("enricoall2.csv")


seats_by_state_year <- data %>%
  filter(year >= 1946 & year <= 1992) %>%
  filter(!is.na(state), !is.na(year)) %>%
  group_by(state, year) %>%
  summarise(
    seats = n(),
    dem_seats = sum(democrat == 1, na.rm = TRUE),
    rep_seats = sum(republic == 1, na.rm = TRUE),
    dem_vote_share = mean(demvoteshare, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(state, year)


write_csv(seats_by_state_year, "seats_by_state_year_1946_1992.csv")


# Generatestate_weight.R
seats_by_state_wide <- seats_by_state_year %>%
  select(state, year, seats) %>%
  pivot_wider(
    names_from = year,
    values_from = seats,
    names_prefix = "year_"
  ) %>%
  arrange(state)

write_csv(seats_by_state_wide, "seats_by_state_wide_1946_1992.csv")
cat("\n已生成按州的宽格式席位数据：seats_by_state_wide_1946_1992.csv\n")


state_statistics <- seats_by_state_year %>%
  group_by(state) %>%
  summarise(
    avg_seats = mean(seats),
    min_seats = min(seats),
    max_seats = max(seats),
    seat_change = max(seats) - min(seats),
    years_covered = n_distinct(year),
    total_observations = n(),
    avg_dem_seats = mean(dem_seats),
    avg_rep_seats = mean(rep_seats)
  ) %>%
  arrange(desc(avg_seats))

print(state_statistics)
write_csv(state_statistics, "state_statistics_1946_1992.csv")

top_changers <- state_statistics %>%
  arrange(desc(seat_change)) %>%
  head(10)

print(top_changers)

seats_wide <- seats_by_state_year %>%
  select(state, year, seats) %>%
  pivot_wider(
    names_from = year,
    values_from = seats,
    names_prefix = "year_"
  ) %>%
  arrange(state)

write_csv(seats_wide, "seats_wide_format_1946_1992.csv")

redistricting_years <- c(1950, 1960, 1970, 1980, 1990)

redistricting_analysis <- seats_by_state_year %>%
  filter(year %in% redistricting_years) %>%
  select(state, year, seats) %>%
  pivot_wider(names_from = year, values_from = seats, names_prefix = "seats_") %>%
  mutate(
    change_1950_1960 = seats_1960 - seats_1950,
    change_1960_1970 = seats_1970 - seats_1960,
    change_1970_1980 = seats_1980 - seats_1970,
    change_1980_1990 = seats_1990 - seats_1980
  ) %>%
  arrange(desc(seats_1990))

print(redistricting_analysis)
write_csv(redistricting_analysis, "redistricting_analysis.csv")

library(scales)

change_data <- seats_by_state_year %>%
  filter(state %in% top_changers$state[1:15]) %>%
  group_by(state) %>%
  mutate(
    seat_pct_change = (seats / first(seats) - 1) * 100
  ) %>%
  ungroup()

p_change <- ggplot(change_data, aes(x = year, y = state, fill = seat_pct_change)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0,
    name = "% Change\nfrom 1946"
  ) +
  labs(
    title = "Congressional Seat Changes Relative to 1946",
    subtitle = "Top 15 States by Seat Volatility",
    x = "Year",
    y = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_change)
ggsave("seats_heatmap_change.png", plot = p_change, width = 12, height = 8, dpi = 300)

party_distribution <- seats_by_state_year %>%
  group_by(year) %>%
  summarise(
    total_dem = sum(dem_seats),
    total_rep = sum(rep_seats),
    total_seats = sum(seats)
  ) %>%
  pivot_longer(
    cols = c(total_dem, total_rep),
    names_to = "party",
    values_to = "seats"
  ) %>%
  mutate(party = ifelse(party == "total_dem", "Democrat", "Republican"))

p_party <- ggplot(party_distribution, aes(x = year, y = seats, fill = party)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("Democrat" = "#0015BC", "Republican" = "#E9141D")) +
  labs(
    title = "Congressional Seats by Party (1946-1992)",
    x = "Year",
    y = "Number of Seats",
    fill = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(p_party)
ggsave("seats_party_distribution.png", plot = p_party, width = 12, height = 7, dpi = 300)

top_10_states <- state_statistics %>%
  head(10) %>%
  pull(state)

summary_report <- seats_by_state_year %>%
  filter(state %in% top_10_states) %>%
  group_by(state) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    seats_1946 = seats[year == min(year)][1],
    seats_1992 = seats[year == max(year)][1],
    total_change = seats_1992 - seats_1946,
    pct_change = round((seats_1992 / seats_1946 - 1) * 100, 1),
    avg_dem_share = round(mean(dem_seats / seats * 100, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(seats_1992))

print(summary_report)
write_csv(summary_report, "top10_states_summary.csv")


cat("- seats_by_state_wide_1946_1992.csv \n")
cat("- seats_by_state_year_1946_1992.csv \n")
cat("- state_statistics_1946_1992.csv \n")
