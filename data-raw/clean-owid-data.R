# This script to prepares a tidy dataset for your app
# no need to run but for your interest only.
library(tidyverse)
library(here)
# read the data
untidy_fuels <- read_csv(
  here("data-raw", "access-to-clean-fuels-for-cooking-vs-gdp-per-capita.csv")
) %>%
  janitor::clean_names()

# then clean up the names to something simpler
# restrict data to match that on the chart
fuels <- untidy_fuels %>%
  rename(
    cooking = access_to_clean_fuels_and_technologies_for_cooking_percent_of_population,
    gdp_per_capita = gdp_per_capita_ppp_constant_2017_international,
    total_population = total_population_gapminder_hyde_un
  ) %>%
  filter(year >= 2000, year <= 2016)

# look up table for continent
country_continent <- fuels %>%
  filter(year == 2015, !is.na(continent)) %>%
  select(entity, code, continent)

# restrict to only countries not upper level
# drop higher level entities like "Latin America" or "World"
# remove data for each country if missing on all vars
cooking_coutries_only <- fuels %>%
  select(-continent) %>%
  left_join(country_continent) %>%
  filter(!is.na(code), entity != "World") %>%
  group_by(entity) %>%
  mutate(all_gdp_miss = all(is.na(gdp_per_capita)),
         all_cooking_miss = all(is.na(cooking))) %>%
  ungroup()

tidy_fuels <- cooking_coutries_only %>%
  filter(!(all_gdp_miss | all_cooking_miss)) %>%
  select(continent, country = entity, code, year, cooking, gdp_per_capita, total_population)

# write out to the data directory
write_csv(tidy_fuels, file = here("data", "cooking.csv"))
