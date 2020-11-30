#MAD OUTLIERING  -----------------------------------------------------------------------------------------------------------------------------------------
#this needs to be reconfigured w/ less comments and the outliering as a function
##working environment
rm(list=ls())

if (Sys.info()[1] == "Linux"){
  j_root <- "/snfs1/"
  h_root <- "~/"
} else if (Sys.info()[1] == "Windows"){
  j_root <- "J:/"
  h_root <- "H:/"
}
## Source central functions
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_bundle_version.R")
source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")
source("/ihme/cc_resources/libraries/current/r/save_crosswalk_version.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
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
## SET OBJETCS
b_id <- 217
out_dir <- paste0(j_root, "WORK/12_bundle/msk_pain_lowback/", b_id, "/03_review/02_upload/")
date <- gsub("-", "_", Sys.Date())
bv_id <-  8996
cv_id <- 4892
mrbrt_dir <- paste0(j_root, "temp/prhine22/MrBeRT/lbp/")
## GET DATA
cw_data <- get_crosswalk_version(cv_id, export = F) #use cvid used to run current model in epiviz that you showed theo
cw_out <- cw_data[ ,is_outlier := NA] #start over on the outliering
cw_mad <- copy(cw_out)
byvars <- c("location_id", "year_start", "year_end", "nid")
## GET AGE WEIGHTS
ages <- as.data.table(get_age_metadata(age_group_set_id=12)) #double check what this age group id coresponds too
setnames(ages, old = c("age_group_years_start", "age_group_years_end"), new = c("age_start", "age_end"))
ages[age_start >= 1 , age_end := age_end - 1]
all_ages <- copy(ages)
n <- 2 #set the number of deviations to outlier at
#DO THIS TO CW MAD
dt <- copy(cw_mad) #going to do all outliering at once
dt_out <- merge(dt, all_ages, by = c("age_start", "age_end"), all.x = T) ##merge age table map and merge on to dataset
#calculate age-standardized prevalence/incidence
##create new age-weights for each data source
dt_out[, sum := sum(age_group_weight_value), by = byvars] #sum of standard age-weights for all the ages we are using, by location-age-sex-nid, sum will be same for all age-groups and possibly less than one
dt_out[, new_weight := age_group_weight_value/sum, by = byvars] #divide each age-group's standard weight by the sum of all the weights in their locaiton-age-sex-nid group
##age standardizing per location-year by sex
#add a column titled "age_std_mean" with the age-standardized mean for the location-year-sex-nid
dt_out[, as_mean := mean * new_weight] #initially just the weighted mean for that AGE-location-year-sex-nid
dt_out[, as_mean := sum(as_mean), by = byvars] #sum across ages within the location-year-sex-nid group, you now have age-standardized mean for that series
##mark as outlier if age standardized mean is 0 (either biased data or rare disease in small ppln)
dt_out[as_mean == 0, `:=` (is_outlier = 1, note_modeler = "outliered this location-year-sex-NID-cdType age-series because age standardized mean is 0" )]
## log-transform to pick up low outliers
dt_out[as_mean != 0, as_mean := log(as_mean)] #outliering occurs using the 'as_mean' variable, which is the age_standardized mean for each series
# calculate MAD (median of the residuals from the data's true median)
dt_out[as_mean == 0, as_mean := NA] # don't count zeros in median calculations
dt_out[,median:=median(as_mean,na.rm = T),by=c("sex")] #unsure why the median is being done by sex and not by the "by_vars" (b/c we are outliering based on each series mad)
dt_out[,mad:=mad(as_mean,na.rm = T),by=c("sex")]
#***can change number of MAD to mark here
dt_outlier <- dt_out[as_mean > ((n*mad)+median), `:=` (is_outlier = 1,
                                                       note_modeler = paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is higher than 2 MAD above median"))]
dt_outlier <- dt_outlier[as_mean < (median - (n*mad)), `:=` (is_outlier = 1,
                                                             note_modeler = paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is lower than 2 MAD below median"))]
# dt_outlier[mean > 0.1 & is_outlier %in% c(0, NA), is_outlier := 1]
dt_outlier[is.na(is_outlier), is_outlier := 0]
print(paste(nrow(dt_outlier[is_outlier == 1]), "points were outliered with", n, "MAD"))
percent_outliered <- round((nrow(dt_outlier[is_outlier == 1]) / nrow(dt_outlier))*100, digits = 1)
print(paste("outliered", percent_outliered, "% of data"))
dropped_locs <- setdiff(unique(cw_data$country), unique(dt_outlier[is_outlier==0]$country))
print(paste("Dropped ", length(dropped_locs), " countries from model:", paste(dropped_locs, collapse = " ")))
dt_outlier[, c("sum", "new_weight", "as_mean", "median", "mad", "age_group_weight_value", "age_group_id") := NULL]
# dt_final <- rbind(cw_nomad, dt_out, fill=TRUE)
dt_final <- copy(dt_outlier)

version <- get_bundle_version(bv_id, export = F)
vers_seqs <- unique(version$seq)
del_seqs <- setdiff(unique(dt_final$seq), vers_seqs)
dt_final[seq %in% del_seqs, seq := ""]

dt_final[location_name == "Shanghai", is_outlier := 1] #NECK PAIN ONLY

path <- paste0(out_dir, "cwv_MAD2_outliering.xlsx")
write.xlsx(dt_final, path, sheetName = "extraction")

outlier_fpath <-  path
description <- paste("outliered data 2 MAD above/below median (", percent_outliered, "%),", paste("dropped", length(dropped_locs), "countries from model:", paste(dropped_locs, collapse = " ")))

xw_result <- save_crosswalk_version(
  bundle_version_id = bv_id,
  data_filepath=outlier_fpath,
  description=description)
# GRAPH -----------------------------------------------------------------------------------------------------------------------------------
# Prep data for plotting
graph_dt <- copy(dt_final)
graph_dt[, age_mid := (age_start + age_end)/2]
graph_dt[, age_group := paste0(age_start, "-", age_end)]
outlier_plot <- ggplot(graph_dt, aes(x = age_mid, y = mean, color = as.factor(is_outlier))) +
  facet_wrap(~clinical_data_type) +
  geom_point(alpha = 0.4) + theme_bw() +
  ggtitle(paste0("LBP - Outlier MAD > ", n)) + xlab("Age Group") + ylab("Mean")
print(outlier_plot)
ggsave(paste0(mrbrt_dir, "mad_trim_plot.pdf"))


##################################
## STEP 4 MAD OUTLIERING PROCESS
##################################

# SOURCE AND OBJECTS ----------------------------------------------------------
rm(list=ls())
##working environment
os <- .Platform$OS.type
if (os=="windows") {
  lib_path <- "C:/Users/hhan5/Documents/R/win-library/3.4"
  j_root<- "J:/"
  h_root<-"H:/"
  scratch <-"H:/outliering"
} else {
  lib_path <- "temp/central_comp/libraries/current/r/"
  j_root<- "/home/j/"
  h_root<-"/homes/bsheena/"
  scratch <- "/share/scratch/users/bsheena/outliering"
}
#set object
functions_dir <- paste0(j_root, "temp/central_comp/libraries/gbd_env/r/")
temp_dir <- paste0(j_root, "temp/prhine22/")
date <- Sys.Date()
date <- gsub("-", "_", date)
bv_id <- 15335 # new bundle version id
step4_cv_id <- 11054 # crosswalked data that you need to do MAD outliering on
b_id <- 6926
cause <- "msk_rheumarthritis"

## Source central functions
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_bundle_version.R")
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")
source("/ihme/cc_resources/libraries/current/r/save_crosswalk_version.R")
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
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
n <- 2 # this will be the number of MAD you will outlier
## GET DATA
#
original_data4 <- get_crosswalk_version(step4_cv_id)
dt_emr <- original_data4[measure == "mtexcess", ]
dt_other_measures <- original_data4[measure != "prevalence" & measure != "mtexcess"]
dt_outliers <- original_data4[measure == "prevalence" & is_outlier == 1]
dt <- original_data4[measure == "prevalence" & is_outlier == 0, ]
cv_drop <- c("cv_survey", "cv_hospital", "cv_literature", "cv_marketscan_all_2000", "cv_marketscan_inp_2000",
             "cv_marketscan_all_2010", "cv_marketscan_inp_2010", "cv_marketscan_all_2012", "cv_marketscan_inp_2012", "cv_inpatient")
byvars <- c("location_id", "sex", "year_start", "year_end", "nid")

## GET AGE WEIGHTS
all_fine_ages <- as.data.table(get_age_metadata(age_group_set_id=12))
not_babies <- all_fine_ages[!age_group_id %in% c(2:4)]
not_babies[, age_end := age_group_years_end-1]
group_babies <- all_fine_ages[ c(1:3)]
group_babies[, age_end := 0.999]
all_ages <- rbind(not_babies, group_babies)
all_ages[, age_start := age_group_years_start]
all_ages[, c("age_group_years_start", "age_group_years_end") := NULL]
all_ages <- rbind(all_ages, all_ages[20,])
all_ages <- all_ages[1:21,]

## SET UP DATATABLE
dt<- dt[!is.na(mean)]
dt_merged <- merge(dt, all_ages, by = c("age_start", "age_end"), all.x = T) # merge age table map and merge on to dataset

for (i in 1:nrow(dt_merged)) {
  if (is.na(dt_merged[i, age_group_weight_value])) {
    dt_merged[, effective_age_start := floor(age_start/5)*5]
    dt_merged[i, age_group_weight_value := sum(all_ages[c(which(all_ages$age_start == dt_merged[i, effective_age_start]), which(all_ages$age_start > dt_merged[i, effective_age_start] & all_ages$age_start <= (dt_merged[i, age_end]-4))), age_group_weight_value])]
    dt_merged$effective_age_start <- NULL
  }
}

#calculate age-standardized prevalence/incidence

##create new age-weights for each data source
dt_merged <- dt_merged[, sum := sum(age_group_weight_value), by = byvars] #sum of standard age-weights for all the ages we are using, by location-age-sex-nid, sum will be same for all age-groups and possibly less than one
dt_merged <- dt_merged[, new_weight := age_group_weight_value/sum, by = byvars] #divide each age-group's standard weight by the sum of all the weights in their locaiton-age-sex-nid group
##age standardizing per location-year by sex
#add a column titled "age_std_mean" with the age-standardized mean for the location-year-sex-nid
dt_merged[, as_mean := mean * new_weight] #initially just the weighted mean for that AGE-location-year-sex-nid
dt_merged[, as_mean := sum(as_mean), by = byvars] #sum across ages within the location-year-sex-nid group, you now have age-standardized mean for that series

##mark as outlier if age standardized mean is 0 (either biased data or rare disease in small ppln)
dt_merged[as_mean == 0, is_outlier_new := 1]
dt_merged[as_mean == 0, note_modeler := paste0(note_modeler, " | outliered this location-year-sex-NID age-series because age standardized mean is 0 ", date)]

## log-transform to pick up low outliers
dt_merged[as_mean != 0, as_mean := log(as_mean)]
# calculate median absolute deviation
dt_merged[as_mean == 0, as_mean := NA] ## don't count zeros in median calculations
dt_merged[,mad:=mad(as_mean,na.rm = T),by=c("sex")]
dt_merged[,median:=median(as_mean,na.rm = T),by=c("sex")]

#***can change number of MAD to mark here
dt_merged[as_mean>((n*mad)+median), is_outlier_new := 1]
dt_merged[as_mean>((n*mad)+median), note_modeler := paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is higher than ", n," MAD above median ", date)]
dt_merged <- dt_outlier[as_mean < (median - (n*mad)), `:=` (is_outlier_new = 1,
                                                            note_modeler = paste0(note_modeler, " | outliered because log age-standardized mean for location-year-sex-NID is lower than 2 MAD below median"))]
dt_merged[, c("sum", "new_weight", "as_mean", "median", "mad", "age_group_weight_value", "age_group_id") := NULL]

print(paste(nrow(dt_merged[is_outlier_new == 1]), "points were outliered with", n, "MAD"))
percent_outliered <- round((nrow(dt_merged[is_outlier_new == 1]) / nrow(dt_merged))*100, digits = 1)
print(paste("outliered", percent_outliered, "% of data"))
dropped_locs <- setdiff(unique(original_data4$country), unique(dt_merged[is.na(is_outlier_new)]$country))
print(paste("Dropped ", length(dropped_locs), " countries from model:", paste(dropped_locs, collapse = " ")))

## SEND TO REVIEW FOLDER
outlier_dt <- copy(dt_merged)
# Situations where the outlier status would change
# UN-OUTLIER
outlier_dt[is_outlier == 1 & is.na(is_outlier_new) & grepl("MAD", note_modeler),
           `:=` (is_outlier = 0, note_modeler = paste("unoutlier based on new MAD calculation", date))]
# NEWLY OUTLIER
outlier_dt[is_outlier == 0 & is_outlier_new == 1, is_outlier := 1] # outlier these
# append the prevalence data and the emr data together
all_data <- rbind(outlier_dt, dt_emr, fill = TRUE)
all_data <- rbind(all_data, dt_other_measures, fill = T)
all_data <- rbind(all_data, dt_outliers, fill = T)

all_data[!(is.na(crosswalk_parent_seq)), seq := ""]

# upload <- all_data[, c("seq", "is_outlier")]
#
# source("/ihme/cc_resources/libraries/current/r/save_bulk_outlier.R")
#
# output_filepath <- paste0(j_root, "WORK/12_bundle/", cause, "/", b_id, "/03_review/02_upload/MAD2_outliering_step4_", date, ".xlsx")
# write.xlsx(upload, output_filepath, sheetName = "extraction", col.names=TRUE, showNA = FALSE, row.names= FALSE)
# description <-  paste("outliered data 2 MAD above/below median (", percent_outliered, "%),", paste("dropped", length(dropped_locs), "countries from model:", paste(dropped_locs, collapse = " ")))
#
# result <- save_bulk_outlier(crosswalk_version_id = step4_cv_id, decomp_step = "step4", filepath = output_filepath, description = description) # neck pain: 9803; low back pain: 10838

output_filepath <- paste0(j_root, "WORK/12_bundle/", cause, "/", b_id, "/03_review/02_upload/MAD2_outliering_step4_", date, ".xlsx")
write.xlsx(all_data, output_filepath, sheetName = "extraction", col.names=TRUE, showNA = FALSE, row.names= FALSE)
description <- "outliered data 2 MAD above/below median"
xw_result <- save_crosswalk_version(
  bundle_version_id = bv_id,
  data_filepath=output_filepath,
  description=description) # rheumatoid @ 2: 11099


