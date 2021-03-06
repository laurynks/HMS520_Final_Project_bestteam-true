################################################################################
# HMS 520
# Final Project
# Weighted Mean Function
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

# load functions and packages ==================================================
source("/ihme/cc_resources/libraries/current/r/get_model_results.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
library(dplyr)

# create function ==============================================================
weighted_mean <- function(df, 
                          weight_by, 
                          gbd_round_id = 7, 
                          decomp_step = "iterative", 
                          is_model_results = TRUE) {
  # Checks ---------------------------------------------------------------------
  # Make sure is_model_result is a logical
  if (!is.logical(is_model_results)) {
    stop("'is_model_results' must be TRUE or FALSE")
  }
  # Make sure gbd_round_id is valid
  if (!(gbd_round_id %in% 3:8)) {
    stop("Please enter a valid gbd_round_id")
  }
  # Make sure decomp_step is valid
  if (!(decomp_step %in% c("iterative", 2, 3))){
    stop("Please enter a valid decomp_step")
  }
  # if the input is not model_results, have the user enter the location_set_id
  if (!is_model_results) {
    location_set_id <- as.numeric(readline(prompt="Enter location_set_id: "))
  } else {
    # Otherwise, use a standard location_set_id
    # 22 (covariate computation) has everything 35 (model results) has and more
    location_set_id = 22
  }
  
  # retrieve and merge IHME population data ------------------------------------
  pop <- get_population(
    # pull in the same specs that the input data has.
    age_group_id = unique(df$age_group_id),
    year_id = unique(df$year_id),
    sex_id = unique(df$sex_id),
    location_id = unique(df$location_id),
    decomp_step = decomp_step
  )
  
  # merge the input data and the population data
  df_pop <- merge(df, pop, by = c('age_group_id', 'year_id', 'sex_id', 
                                  'location_id'))
  
  
  # calculate the weighted mean ------------------------------------------------
  
  weighted_df <- df_pop %>% 
    # group by the desired variables
    group_by(!!!syms(weight_by)) %>%
    # calculate the share of total population in each group
    mutate(pop_share = population/sum(population)) %>% 
    # calculate the weighted_mean share for each group
    mutate(weighted_mean_share = pop_share * mean) %>% 
    # sum the weighted mean for each group
    summarise(weighted_mean = sum(weighted_mean_share))
  
  # finalize and return --------------------------------------------------------
  # pull in location metadata when location_id is one of the weighting factors
  if ('location_id' %in% weight_by) {
    locs <- get_location_metadata(location_set_id = location_set_id, 
                                  gbd_round_id = gbd_round_id, 
                                  decomp_step = decomp_step)
    # left join the weighted means and the location metadata (to ensure no 
    # weighted mean data is lost)
    weighted_df_locs <- left_join(weighted_df, locs, by = "location_id")
    return(weighted_df_locs)
  } else {
    return(weighted_df)
  }
}

# Testing ======================================================================
test_df <- get_model_results(
  gbd_team = 'epi',
  gbd_id = 25217,
  age_group_id = c(7, 8, 9, 10, 11, 12, 13, 14, 15), # mutually exclusive age groups for 10-54
  year_id = 2020,
  sex_id = c(1,2),
  decomp_step = 'iterative'
)

t2 <- weighted_mean(test_df, c("sex_id", "age_group_id"), is_model_results = TRUE)
t2