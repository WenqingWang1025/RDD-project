library(dplyr)

pop_data <- read.csv("historical_state_population_by_year.csv", header = FALSE)

colnames(pop_data) <- c("state_abbr", "year", "population")

print(head(pop_data, 10))

cat("\nAK 1950å¹´:\n")
print(pop_data %>% filter(state_abbr == "AK" & year == 1950))

pop_filtered <- pop_data %>%
  filter(year >= 1946 & year <= 1992) %>%
  filter(year %% 2 == 0)

state_names <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                 "Kentucky", "Louisiana", "Maine", "Maryland",
                 "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                 "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                 "New Jersey", "New Mexico", "New York", "North Carolina", 
                 "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                 "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                 "Virginia", "Washington", "West Virginia", "Wisconsin", 
                 "Wyoming", "District of Columbia")
)

pop_final <- pop_filtered %>%
  left_join(state_names, by = "state_abbr") %>%
  select(state_abbr, state_name, year, population) %>%
  arrange(state_abbr, year)

write.csv(pop_final, "state_population_even_years_1946_1992.csv", row.names = FALSE)

print(head(pop_final %>% filter(state_abbr == "AK"), 5))

