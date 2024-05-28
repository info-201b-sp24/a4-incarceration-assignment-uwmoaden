# Assignment 3 - Summary.R file
# This R Script File is used exclusively for exploring and summarizing
# findings within the chosen data sets given (from READ.md).
# Mohamed Aden

# Loading necessary library applications
library(dplyr)
library(ggplot2)
# Only using data from NCRP and DCRP (with ASJ/COJ) data
# Loading in Data
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")
jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# peeking data
head(prison)
head(jail)

# Examining Data
prison_features_count <- ncol(prison)
jail_features_count <- ncol(jail)

prison_rows_count <- nrow(prison)
jail_rows_count <- nrow(jail)

# Filtering into new data frames for clearer analysis -- 2000-2013 for both prison and jail data frames
prison_years_filtered <- prison %>%
  filter(year > 1999 & year < 2014)

jail_year_filtered <- jail %>%
  filter(year > 1999 & year < 2014)

# Calculating average prison and jail population proportions for the most recent year (2013) in the data set 

# Average black prison population in most recent year across all counties
average_prison_black_pop_2013 <- prison_years_filtered %>%
  filter(year == 2013) %>%
  summarise(average_blk_prison_pop = mean(black_prison_pop, na.rm = TRUE)) %>%
  pull(average_blk_prison_pop)


# Average black jail population in 2013 across all counties
average_jail_black_pop_2013 <- jail_year_filtered %>%
  filter(year == 2013) %>%
  summarise(average_blk_jail_pop = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(average_blk_jail_pop)


# The average total prison population across all counties in 2013
average_prison_pop <- prison_years_filtered %>%
  filter(year == 2013) %>%
  summarise(avg_prison_total_pop = mean(total_pop, na.rm = TRUE)) %>%
  pull(avg_prison_total_pop)

# The average total pop across all counties in 2013
average_jail_pop <- jail_year_filtered %>%
  filter(year == 2013) %>%
  summarise(avr_jail_total_pop = mean(total_pop, na.rm = TRUE)) %>%
  pull(avr_jail_total_pop)

# calculating population proportions for black indivduals in jails and prisons in most recent year across all counties
population_proportion_black_prison_2013 <- (average_prison_black_pop_2013 / average_prison_pop)*100

population_proportion_black_jail_2013 <- (average_jail_black_pop_2013 / average_jail_pop)*100

# New dataframe for the proportion of black prison population over time (2000-2013) across all counties over full timespan year-by-year
black_prison_proportion_trend <- prison_years_filtered %>%
  group_by(year) %>%
  summarise(
    total_black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    total_prison_pop = sum(total_pop, na.rm = TRUE)
  ) %>%
  mutate(black_prison_proportion = (total_black_prison_pop / total_prison_pop) * 100)

# New data frame for the proportion of black jail population across all counties over full time span year-by-year
black_jail_proportion_trend <- jail_year_filtered %>%
  group_by(year) %>%
  summarise(
    total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
    total_jail_pop = sum(total_pop, na.rm = TRUE)
  ) %>%
  mutate(black_jail_proportion = (total_black_jail_pop / total_jail_pop) * 100)

# Calculating the average black jail population proportion over the entire time span 
black_jail_proportion <- black_jail_proportion_trend %>%
  summarise(black_jail_grouped = mean(black_jail_proportion)) %>%
  pull(black_jail_grouped)

# Calculating average black jail population proportions over the entire time span
black_prison_proportion <- black_prison_proportion_trend %>%
  summarise(black_prison_grouped = mean(black_prison_proportion)) %>%
  pull(black_prison_grouped)

# calculating black prison proportions by state
black_prison_proportion_by_state <- prison_years_filtered %>%
  group_by(state) %>%
  summarise(
    total_black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    total_prison_pop = sum(total_pop, na.rm = TRUE)
  ) %>%
  mutate(black_prison_proportion = (total_black_prison_pop / total_prison_pop) * 100)

