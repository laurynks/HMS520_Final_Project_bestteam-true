################################################################################
# HMS 520
# Weighted Mean Function
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

#' Weighted Means Function to Age-Standardize GBD Outputs
#'
#' @param df A data frame produced using the shared function "get_model_results"
#' @param weight_by A character string of variables to weight by
#' @param gbd_round_id The gbd_round_id for the get_location_metadata shared function, the default value is 7
#' @param decomp_step The decomp_step for the get_population and get_location_metadata shared functions, the default value is "iterative"
#' @param is_model_results A boolean variable, set as 'TRUE' (the default) if the input is model_results; if 'FALSE', the user must enter a location_set_id
#'
#' @return A data frame with age-standardized data.
#' @export
#'
#' @examples
weighted_mean <- function(df,
                          weight_by,
                          gbd_round_id = 7,
                          decomp_step = "iterative",
                          is_model_results = TRUE) {
  # Sourcing Shared Functions ---------------------------------------------------------------------
  source("/ihme/cc_resources/libraries/current/r/get_population.R")
  source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

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
    location_set_id == 22
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
