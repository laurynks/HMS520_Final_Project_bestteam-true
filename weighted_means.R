# this is copied and pasted from the specific case I had to do it for.

source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
source("/ihme/cc_resources/libraries/current/r/get_model_results.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

library(dplyr)
library(ggplot2)

# get in-facility delivery data
IFD <- get_covariate_estimates(
  gbd_round = 7,
  covariate_id = 51,
  year_id = 2020,
  #sex_id = 2,
  decomp_step = 'iterative'
)

IFD$mean_IFD <- IFD$mean_value

# Get inpatient utilization results
util <- get_model_results(
  gbd_team = 'epi',
  gbd_id = 25217,
  age_group_id = c(7, 8, 9, 10, 11, 12, 13, 14, 15), # mutually exclusive age groups for 10-54
  year_id = 2020,
  sex_id = 2,
  decomp_step = 'iterative'
)

# get population
pop <- get_population(
  age_group_id = c(7:15), # mutually exclusive age groups for 10-54
  year_id = 2020,
  sex_id = 2,
  location_id = unique(util$location_id), # pull in the same location_ids used in the utilization
  decomp_step = 'iterative'
)

# calculate a weighted mean by age of inpatient utilization
util_pop <- merge(util, pop, by = c('age_group_id', 'location_id'))
weighted_utils <- util_pop %>% group_by(location_id) %>% mutate(pop_share = population/sum(population)) %>% 
  mutate(weighted_mean_share = pop_share * mean) %>% summarise(weighted_mean = sum(weighted_mean_share))

weighted_utils$mean_utils <- weighted_utils$weighted_mean

# merge data
full_dat <- merge(IFD, weighted_utils, by='location_id')

loc_data <- get_location_metadata(location_set_id=22, gbd_round_id=7)

full_dat_locs <- merge(full_dat, loc_data, by = 'location_id', all.x = TRUE)