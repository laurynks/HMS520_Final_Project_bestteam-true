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
input_dismod_bundle <- sample_DISMOD_cp

## DISMOD to STGPR
dismod_to_stgpr <- function(input_dismod_bundle){
  columns <- colnames(input_dismod_bundle)
  
  input_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575819
    input_type <- c("Not set", "extracted", "adjusted", "split", "collapsed")
    input_map <- data.frame(id = c(-1, 1:4), type = as.character(input_type))
    if (val %in% input_type){
      return(input_map$id[val == input_map$type])
    }
    else{
      return(-1)
    }
  }
  
  recall_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575826
    recall_type <- c("Not Set", "Point", "Lifetime", "Period: years", "Period: months", "Period: weeks",
                     "Period: days")
    recall_map <- data.frame(id = c(-1, 1:6), type = as.character(recall_type))
    if(val %in% recall_type){
      return(recall_map$id[val == recall_map$type])
    }
    else{
      return(-1)
    }
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
                             "Representative of urban areas only", "Representative of rural areas only")
    representative_type_map <- data.frame(id = -1:11, type = as.character(representative_type))
    if(val %in% representative_type){
      return(representative_type_map$id[val == representative_type_map$type])
    }
    else{
      return(-1)
    }
  }
  
  
  sampling_type_converter <- function(val){
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575829
    sampling_type <- c("Not set", "Cluster", "Multistage", "Nonprobability", "Probability", "Simple random")
    sampling_type_map <- data.frame(id = c(-1, 1:5), type = as.character(sampling_type))
    if (val %in% sampling_type) {
      return(sampling_type_map$id[val == sampling_type_map$type])
    }
    else(
      return(-1)
    )
  }
  
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
  
  uncertainty_type_converter <- function(val){
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575836
    uncertainty_type <- c("Not set", "Standard error", "Effective Sample Size", 
                          "Confidence Interval", "Sample Size")
    uncertainty_map <- data.frame(id = c(-1, 1:4), type = as.character(uncertainty_type))
    if (val %in% uncertainty_type){
      return(uncertainty_map$id[uncertainty_map$type == val])
    }
    else{
      return(-1)
    }
  }
  
  map_type_id <- function(var_name) {
    if (var_name = "representative") {
      if (!("representative_id" %in% columns)) {
        if("representative_name" %in% columns) {
          message("creating column 'representative_id' from 'representative_name'")
          input_dismod_bundle <- input_dismod_bundle %>% 
            mutate(representative_id = sapply(representative_name, representative_type_converter))
        } 
        else {
          stop("input bundle must either have column 'representative_name' or 'representative_id'")
        }
      }
    }
    else {
      if (!(paste0(var_name, "_type_id") %in% columns)) {
        message(paste0("creating column '", var_name, "_type_id' from '", 
                       var_name, "_type'"))
        input_dismod_bundle <- input_dismod_bundle %>%
          mutate(paste0(var_name, "_type_id") = 
                   sapply(get(paste0(var_name, "_type")), get(paste0(var_name, 
                                                                     "_type_converter"))))
      }
      else {
        stop(paste0("input bundle must either have column '", var_name, 
                    "_type' or '", var_name, "_type_id'"))
      }
    }
    return(input_dismod_bundle)
  }
  
  if (!("variance" %in% columns)){
    message("Adding NA column 'variance'")
    input_dismod_bundle <- input_dismod_bundle %>% mutate(variance = rep(NA, nrow(input_dismod_bundle)))
  }
  if (!("year_id" %in% columns)){
    message("Adding column year_id (defined as the central year between year_start and year_end)")
    input_dismod_bundle <- input_dismod_bundle %>% mutate(year_id = as.integer(round((year_start + year_end)/2, 2)))
  }
  if (!("val" %in% columns)){
    message("Renaming col 'mean' to 'val'")
    input_dismod_bundle <- input_dismod_bundle %>% rename(val = mean)
  }
  var_names <- c("input", "recall", "representative", "sampling", "source", "uncertainty")
  # Doing this in a for loop because each iteration of the dataset builds on the next one
  for (var_name in var_names){
    map_type_id(var_name)
  }
  # NOTE: might need to map unit type
  return(input_dismod_bundle)
}

dismod_to_stgpr(sample_DISMOD_cp)