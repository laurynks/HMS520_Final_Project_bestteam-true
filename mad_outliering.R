###########################################################
### Authors: Mae Dirac, modified by Hannah Han, Jaimie Adelson, and Shaun Roberts
### Modified by: Lauryn Stafford and Jessica Bishai for HMS 520
### Description: Accepts data as Excel spreadsheet as output from epi_dataversionreview.do, identifies data-series (defined by sex, location, year and NID) 
###              for which all observed values are 0 and those for which the age-adjusted mean is more than xMADs above or below median, marks them as outliers, 
###              exports file that can be uploaded to Epi DB.  Can specify which sources to include in automated process, but cannot include literature (which 
###              is usually less abundant and should be visually outliered)
### Based on: GBS hospital data prep code from Emma Nichols (12/2017)
### Modified so that a series is outliered by super region (lumping high-income and Eastern Europe, Central Europe & Asia together)
###########################################################
#***Marks user input needed; best to use Find for all ***

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

## Set objects

functions_dir <- "/ihme/cc_resources/libraries/current/r/"
temp_dir <- paste0(j_root, "temp/laurynks/")
date <- Sys.Date()
date <- gsub("-", "_", date)

outlier_val <- 2 # setting the value for MAD outliering ***

bundle_id <- 278 # setting the bundle ID ***
decomp_step <- 'iterative' # setting the decomp step ***
gbd_round_id <- 7 # setting the GBD round ID ***
bundle_version_id <- 22562 # setting the bundle version ID
cause_name <- cause_path <- "278_gbs_impairment" 

path_to_input_data <-  paste0("/snfs1/WORK/12_bundle/imp_gbs/278/03_review/02_upload/", date, "_", decomp_step, "_",bundle_id, "_sex_split_NID_less_50_outlier", ".xlsx") # setting path to input data ***
path_to_output_data <- paste0("/snfs1/WORK/12_bundle/imp_gbs/278/03_review/02_upload/", date, "_", decomp_step, "_",bundle_id, "MAD1", ".xlsx") # setting path to output data ***
description <- "2020 step2 sex split data, outliered MAD = 2" # setting description for output ***

## Source central functions
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
age_using <- c(2:3,388, 389, 238, 34,6:20, 30:32, 235 )

byvars <- c("location_id", "sex", "year_start", "year_end", "nid")

locs <- read.csv("/home/j/temp/jadelson/misc/ihme_loc_metadata_2020.csv")

## INPUT DATA
dt <- get_bundle_version(bundle_version_id, fetch = "all")

## GET AGE WEIGHTS
all_fine_ages <- as.data.table(get_age_metadata(age_group_set_id=19, gbd_round_id = gbd_round_id))

##FUNCTION
mad_outliering <- function(dt, all_fine_ages, locs, byvars, outlier_val) {
  ##make a set to be run through outlier script
  dt_inp <- copy(dt)
  
  ##merge age table map and merge on to dataset
  all_fine_ages[, age_start := age_group_years_start]
  dt_inp <- merge(dt_inp, all_fine_ages, by = c("age_start"), all.x = T)
  
  #calculate age-standardized prevalence/incidence
  
  ##create new age-weights for each data source
  dt_inp[, sum := sum(age_group_weight_value), by = byvars] #sum of standard age-weights for all the ages we are using, by location-age-sex-nid, sum will be same for all age-groups and possibly less than one
  dt_inp[, new_weight := age_group_weight_value/sum, by = byvars] #divide each age-group's standard weight by the sum of all the weights in their location-age-sex-nid group
  
  ##age standardizing per location-year by sex
  #add a column titled "age_std_mean" with the age-standardized mean for the location-year-sex-nid
  dt_inp[, as_mean := mean * new_weight] #initially just the weighted mean for that AGE-location-year-sex-nid
  dt_inp[, as_mean := sum(as_mean), by = byvars] #sum across ages within the location-year-sex-nid group, you now have age-standardized mean for that series
  
  #add super-region to data table and combine high-income and C/E Europe and C Asia due to small number of studies
  dt_inp <- join(dt_inp, locs[,c("location_id","super_region_name")], by="location_id")
  dt_inp[super_region_name=="Central Europe, Eastern Europe, and Central Asia", super_region_name:= "High-income"]
  
  ##mark as outlier if age standardized mean is 0 (either biased data or rare disease in small ppln)
  dt_inp[as_mean == 0, is_outlier := 1]
  dt_inp[as_mean == 0, note_modeler := paste0(note_modeler, " | outliered this location-year-sex-NID age-series because age standardized mean is 0")]
  
  ## log-transform to pick up low outliers
  dt_inp[as_mean != 0, as_mean := log(as_mean)]
  
  # calculate median absolute deviation
  dt_inp[as_mean == 0, as_mean := NA] ## don't count zeros in median calculations
  dt_inp[,mad:=mad(as_mean,na.rm = T),by=c("sex", "super_region_name")]
  dt_inp[,median:=median(as_mean,na.rm = T),by=c("sex", "super_region_name")]
  
  dt_inp[as_mean>((outlier_val*mad)+median), is_outlier := 1]
  dt_inp[as_mean>((outlier_val*mad)+median), note_modeler := paste0(note_modeler, " | outliered because age-standardized mean for location-year-sex-NID is higher than ", outlier_val," MAD above median")]
  dt_inp[as_mean<(median-(outlier_val*mad)), is_outlier := 1]
  dt_inp[as_mean<(median-(outlier_val*mad)), note_modeler := paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is lower than ", outlier_val," MAD below median")]
  dt_inp[, c("sum", "new_weight", "as_mean", "median", "mad", "age_group_weight_value", "age_group_id", "super_region_name") := NULL]
  
  dt_inp[is.na(lower), uncertainty_type_value := NA]
  
  #Make sure so SE's are above 1. If there is an SE above 1, If os is above 1 turn to NULL.
  dt_inp[standard_error > 1, standard_error := 1]
  
  write.xlsx(dt_inp, path_to_output_data, sheetName="extraction")
  
  save_crosswalk_version(
    bundle_version_id = bundle_version_id,
    data_filepath = path_to_output_data,
    description = description
  )
  
  print(sprintf("New crosswalk Version ID with outliers: %s", result$crosswalk_version_id))
}