############################################################################################################
# HMS 520 
# Bundle Converter
# Authors: Lauryn Stafford and Jessica Bishai
############################################################################################################
source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")

# pull in sample DisMod bundle
sample_DisMod <- get_bundle_data(bundle_id = 435, 
                                 decomp_step = "iterative",
                                 gbd_round_id = 7)

sample_STGPR <- get_crosswalk_version(crosswalk_version_id = 31250)

sample_STGPR_cp <- copy(sample_STGPR)
sample_DISMOD_cp <- copy(sample_DisMod)

## DISMOD to STGPR
if (!("variance" %in% colnames(sample_DISMOD_cp))){
  warning("Adding NA column 'variance'")
  sample_DISMOD_cp <- sample_DISMOD_cp %>% mutate(variance = rep(NA, nrow(sample_DISMOD_cp)))
}
if (!("year_id" %in% colnames(sample_DISMOD_cp))){
  warning("Adding column year_id (defined as the central year between year_start and year_end)")
  sample_DISMOD_cp <- sample_DISMOD_cp %>% mutate(year_id = as.integer(round((year_start + year_end)/2, 2)))
}
if (!("val" %in% colnames(sample_DISMOD_cp))){
  sample_DISMOD_cp <- sample_DISMOD_cp %>% rename(val = mean)
}
#if (!("uncertainty_type_id" %in% colnames(sample_DISMOD_cp))){
#  map_uncertainty <- function(
#    if(uncertainty_type)
#  )
#}
if (!("source_type_id" %in% colnames(sample_DISMOD_cp))){
  sample_DISMOD_cp <- sample_DISMOD_cp %>% mutate(source_type_id = sapply(source_type, source_type_converter))
}
if (!("sampling_type_id" %in% colnames(sample_DISMOD_cp))){
  sample_DISMOD_cp <- sample_DISMOD_cp %>% mutate(sampling_type_id = sapply(sampling_tye, sampling_type_converter))
}
# NOTE: might need to map unit type
# NOTE: need to map uncertainty


source_type_converter <- function(val){
  # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=26099974
  source_type_short = c("DSP before 1990", "VR", "SRS", "DSP", "DSS", "Census", "Standard DHS", "Other DHS", "RHS", 
                        "PAPFAM", "PAPCHILD", "MICS", "WFS", "LSMS", "MIS", "AIS", "Other", "UNICEF", "VR pre-2009", 
                        "VR post-2009", "DSP<1996", "DSP 96-03", "DSP>2003", "DSP 96-00", "VR pre-1978", "VR post-1978",
                        "other")
  source_type_long = c("DSP before 1990", "Vital Registration", "Sample Registrations System", "Disease surveillance points",
                       "Demographic surveillance sites", "Census", "Standard demographic and health survey", 
                       "Other demographic and health survey", "Reproductive Health Survey", "Pan Arab Project for Family Health", 
                       "Pan Arab Project for Child Development", "Multiple Indicator Cluster Survey", "World Fertility Survey",
                       "Living Standards Measurement Study", "Malaria Indicator Survey", "AIDS Indicator Survey", "Other survey",
                       "UNICEF", "Vital Registration (1978-2008)", "Vital Registration (2009-2013)", "Disease Surveillance Point (1991-1995)",
                       "Disease Surveillance Point (1996-2003)", "Disease Surveillance Point (2004 and after)", 
                       "Disease Surveillance Point (1996-2000)", "Vital Registration before 1978", "Vital Registration after 1978",
                       "other")
  source_type_map = data.frame(id = 0:26, type_short = as.character(source_type_short), type_long = as.character(source_type_long))
  if (val %in% source_type_map$type_long){
    return(source_type_map$id[source_type_map$type_long == val])
  } else if (val %in% source_type_map$type_short){
    return(source_type_map$id[source_type_map$type_short == val])
  } else {
    return(26) # other
  }
}

sampling_type_converter <- function(val){
  # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575829
  sampling_type <- c("Cluster", "Multistage", "Nonprobability", "Probability", "Simple random", "Other")
  sampling_type_map <- data.frame(id = 1:6, type = as.character(sampling_type))
  return(sampling_type_map$id[val == sampling_type_map$type])
}

representative_type_converter <- function(val){
  # https://hub.ihme.washington.edu/display/SCDS/shared.representative
  representative_type <- c("Not Set" ,"Unknown" ,"Nationally representative only",
                           "Representative for subnational location only", "Not representative", 
                           "Nationally and subnationally representative",
                           "Nationally and urban/rural representative",
                           "Nationally, subnationally and urban/rural representative",
                           "Representative for subnational location and below",
                           "Representative for subnational location and urban/rural",
                           "Representative for subnational location, urban/rural and below",
                           "Representative of urban areas only", "Representative of rural areas only",
                           "Other")
  representive_type_map <- data.frame(id = -1:12)
}
}