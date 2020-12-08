###########################################################
### Authors: Mae Dirac, modified by Hannah Han, Jaimie Adelson, and Shaun Roberts
### Modified by: Lauryn Stafford and Jessica Bishai for HMS 520
###########################################################

rm(list=ls())

## Working environment
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/snfs1/"
  h_root <- "~/"
  l_root <- "/ihme/limited_use/"
} else {
  j_root <- "J:/"
  h_root <- "H:/"
  l_root <- "L:/"
}

## Source central functions
functions_dir <- "/ihme/cc_resources/libraries/current/r/"

source(paste0(functions_dir, "get_age_metadata.R"))
source(paste0(functions_dir, "save_crosswalk_version.R"))
source(paste0(functions_dir, "get_bundle_version.R"))

library("data.table")
library("ggplot2")
library("readr")
library("RMySQL")
library("openxlsx")
library("readxl")
library("stringr")
library("tidyr")
library("plyr")
library("dplyr")


## SET OBJECTS
outlier_val <- 2 

byvars <- c("location_id", "sex", "year_start", "year_end", "nid")

## INPUT DATA
dt <- get_bundle_version(bundle_version_id = 22562, fetch = "all")

## GET AGE WEIGHTS
all_fine_ages <- as.data.table(get_age_metadata(age_group_set_id=19, gbd_round_id = 7))

##FUNCTION
mad_outliering <- function(dt, all_fine_ages, byvars, outlier_val) {
  ##make a set to be run through outlier script
  dt_inp <- copy(dt)
  
  ##merge age table map and merge on to dataset
  all_fine_ages[, age_start := age_group_years_start]
  dt_inp <- merge(dt_inp, all_fine_ages, by = c("age_start"), all.x = T)
  
  ##create new age-weights for each data source
  dt_inp[, sum := sum(age_group_weight_value), by = byvars] #sum of standard age-weights for all the ages we are using, by location-age-sex-nid, sum will be same for all age-groups and possibly less than one
  dt_inp[, new_weight := age_group_weight_value/sum, by = byvars] #divide each age-group's standard weight by the sum of all the weights in their location-age-sex-nid group
  
  ##age standardizing per location-year by sex
  #add a column titled "age_std_mean" with the age-standardized mean for the location-year-sex-nid
  dt_inp[, as_mean := mean * new_weight] #initially just the weighted mean for that AGE-location-year-sex-nid
  dt_inp[, as_mean := sum(as_mean), by = byvars] #sum across ages within the location-year-sex-nid group, you now have age-standardized mean for that series
  
  ##mark as outlier if age standardized mean is 0 (either biased data or rare disease in small ppln)
  dt_inp[as_mean == 0, is_outlier := 1]
  dt_inp[as_mean == 0, note_modeler := paste0(note_modeler, " | outliered this location-year-sex-NID age-series because age standardized mean is 0")]
  
  ## log-transform to pick up low outliers
  dt_inp[as_mean != 0, as_mean := log(as_mean)]
  
  # calculate median absolute deviation
  dt_inp[as_mean == 0, as_mean := NA] ## don't count zeros in median calculations
  dt_inp[,mad:=mad(as_mean,na.rm = T),by=c("sex")]
  dt_inp[,median:=median(as_mean,na.rm = T),by=c("sex")]
  
  dt_inp[as_mean>((outlier_val*mad)+median), is_outlier := 1]
  dt_inp[as_mean>((outlier_val*mad)+median), note_modeler := paste0(note_modeler, " | outliered because age-standardized mean for location-year-sex-NID is higher than ", outlier_val," MAD above median")]
  dt_inp[as_mean<(median-(outlier_val*mad)), is_outlier := 1]
  dt_inp[as_mean<(median-(outlier_val*mad)), note_modeler := paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is lower than ", outlier_val," MAD below median")]
  dt_inp[, c("sum", "new_weight", "as_mean", "median", "mad", "age_group_weight_value", "age_group_id") := NULL]
  
  dt_inp[is.na(lower), uncertainty_type_value := NA]
  
  #Make sure so SE's are above 1. If there is an SE above 1, If os is above 1 turn to NULL.
  dt_inp[standard_error > 1, standard_error := 1]
  
  return(dt_inp)
}

mad_outliering_test <- mad_outliering(dt, all_fine_ages, byvars, outlier_val)
