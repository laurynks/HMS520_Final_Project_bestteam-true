################################################################################
# HMS 520
# MAD Outliering Function
# Original Authors: Mae Dirac, modified by Hannah Han, Jaimie Adelson, and Shaun Roberts
# Modified by: Lauryn Stafford and Jessica Bishai
################################################################################

#' MAD Outliering
#'
#' @param dt A data.table of data to be run through MAD outliering; typically a bundle or crosswalk version.
#' @param age_group_set_id The age_group_set_id for the get_age_metadata function, used to calculate age weights.
#' @param gbd_round_id The gbd_round_id for the get_age_metadata function, used to calculate age weights.
#' @param byvars A character string including the unique data identifier column names from dt.
#' @param outlier_val The value used for MAD outliering.
#'
#' @return A data.table with updated outliers, identified through MAD outliering, and noted by a value of 1 in "is_outlier" column and a comment in "note_modeler".
#' @export
#'
#' @examples mad_outliering(diabetes_bundle_version, 19, 7, c("location_id", "sex", "year_start", "year_end", "nid"), 2)
mad_outliering <- function(dt, age_group_set_id, gbd_round_id, byvars, outlier_val) {
  ## sourcing shared functions
  source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")

  ## checks
  ### make sure dt is a data.table
  if (!is.data.table(dt)) {
    stop("Input data must be a data.table")
  }
  ### make sure age_group_set_id is valid
  if (!(age_group_set_id %in% 1:23)) {
    stop("Please enter a valid age_group_set_id")
  }
  ### make sure gbd_round_id is valid
  if (!(gbd_round_id %in% 3:8)) {
    stop("Please enter a valid gbd_round_id")
  }
  ### make sure byvars is a character vector
  if (!is.character(byvars)) {
    stop("byvars must be a character vector")
  }
  ### make sure outlier_val is numeric
  if (!is.numeric(outlier_val)) {
    stop("outlier_val must be numeric")
  }

  ## make a set to be run through outlier script
  dt_inp <- copy(dt)

  # getting age weights
  all_fine_ages <- as.data.table(get_age_metadata(age_group_set_id, gbd_round_id))

  ## merge age table map and merge on to dataset
  all_fine_ages[, age_start := age_group_years_start]
  dt_inp <- merge(dt_inp, all_fine_ages, by = c("age_start"), all.x = T)

  ## create new age-weights for each data source
  dt_inp[, sum := sum(age_group_weight_value), by = byvars]
  dt_inp[, new_weight := age_group_weight_value/sum, by = byvars]

  ## age standardizing per location-year by sex
  dt_inp[, as_mean := mean * new_weight]
  dt_inp[, as_mean := sum(as_mean), by = byvars]

  ## mark as outlier if age standardized mean is 0 (either biased data or rare disease in small ppln)
  dt_inp[as_mean == 0, is_outlier := 1]
  dt_inp[as_mean == 0, note_modeler := paste0(note_modeler, " | outliered this location-year-sex-NID age-series because age standardized mean is 0")]

  ## log-transform to pick up low outliers
  dt_inp[as_mean != 0, as_mean := log(as_mean)]

  ## calculate median absolute deviation
  dt_inp[as_mean == 0, as_mean := NA] ## don't count zeros in median calculations
  dt_inp[,mad:=mad(as_mean,na.rm = T),by=c("sex")]
  dt_inp[,median:=median(as_mean,na.rm = T),by=c("sex")]

  dt_inp[as_mean>((outlier_val*mad)+median), is_outlier := 1]
  dt_inp[as_mean>((outlier_val*mad)+median), note_modeler := paste0(note_modeler, " | outliered because age-standardized mean for location-year-sex-NID is higher than ", outlier_val," MAD above median")]
  dt_inp[as_mean<(median-(outlier_val*mad)), is_outlier := 1]
  dt_inp[as_mean<(median-(outlier_val*mad)), note_modeler := paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is lower than ", outlier_val," MAD below median")]
  dt_inp[, c("sum", "new_weight", "as_mean", "median", "mad", "age_group_weight_value", "age_group_id") := NULL]

  dt_inp[is.na(lower), uncertainty_type_value := NA]

  ## making sure SE's are above 1
  dt_inp[standard_error > 1, standard_error := 1]

  return(dt_inp)
}
