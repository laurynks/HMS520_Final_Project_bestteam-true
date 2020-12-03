################################################################################
# HMS 520
# Final Project
# Weighted Mean Function
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
source("/ihme/cc_resources/libraries/current/r/get_model_results.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

library(data.table)
library(dplyr)
library(ggplot2)

weighted_mean <- function(df) {
  
  # retrieve IHME population data
  pop <- get_population(
    # pull in the same specs that the input data has.
    age_group_id = unique(df$age_group_id),
    year_id = unique(df$year_id),
    sex_id = unique(df$sex_id),
    location_id = unique(df$location_id),
    # Assume decomp_step is always iterative
    decomp_step = 'iterative'
  )
  
  # merge the input data and the population data
  df_pop <- merge(df, pop, by = c('age_group_id', 'year_id', 'sex_id', 
                                  'location_id'))
  
  # THIS LINE NEEDS TO BE REWORKED; look at data.table suggestion from Will
  #df_pop[, mean := weighted.mean(mean, w=population), by="location_id"]
  #return(df_pop)
  
  weighted_df <- df_pop %>% 
    group_by(location_id) %>% 
    mutate(pop_share = population/sum(population)) %>% 
    mutate(weighted_mean_share = pop_share * mean) %>% 
    summarise(weighted_mean = sum(weighted_mean_share))  
  
  return(weighted_df)
}

# Testing
test_df <- get_model_results(
  gbd_team = 'epi',
  gbd_id = 25217,
  age_group_id = c(7, 8, 9, 10, 11, 12, 13, 14, 15), # mutually exclusive age groups for 10-54
  year_id = 2020,
  sex_id = 2,
  decomp_step = 'iterative'
)

weighted_mean(test_df)
