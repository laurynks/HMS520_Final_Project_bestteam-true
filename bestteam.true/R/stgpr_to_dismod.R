################################################################################
# HMS 520
# Bundle Converter: STGPR to DisMod
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

#' ST-GPR to DisMod Bundle Converter
#'
#' @param input_stgpr_bundle A crosswalk version of a ST-GPR shape bundle.
#'
#' @return The inputted bundle data, converted to DisMod shape.
#' @export
#'
#' @examples stgpr_to_dismod(diabetes_stgpr_bundle)
stgpr_to_dismod <- function(input_stgpr_bundle) {
  input_stgpr_bundle <- as.data.frame(input_stgpr_bundle)
  columns <- colnames(input_stgpr_bundle)

  # create mapping functions ---------------------------------------------------
  input_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575819
    input_type <- c("Not set", "extracted", "adjusted", "split", "collapsed")
    input_map <- data.frame(id = c(-1, 1:4), type = as.character(input_type))
    if (val %in% input_map$id){
      return(input_map$type[input_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  recall_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575826
    recall_type <- c("Not Set", "Point", "Lifetime", "Period: years",
                     "Period: months", "Period: weeks",
                     "Period: days")
    recall_map <- data.frame(id = c(-1, 1:6), type = as.character(recall_type))
    if(val %in% recall_map$id) {
      return(recall_map$type[recall_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  representative_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/display/SCDS/shared.representative
    representative_type <- c("Not Set" ,"Unknown" ,
                             "Nationally representative only",
                             "Representative for subnational location only",
                             "Not representative",
                             "Nationally and subnationally representative",
                             "Nationally and urban/rural representative",
                             "Nationally, subnationally and urban/rural representative",
                             "Representative for subnational location and below",
                             "Representative for subnational location and urban/rural",
                             "Representative for subnational location, urban/rural and below",
                             "Representative of urban areas only",
                             "Representative of rural areas only")
    representative_map <- data.frame(id = -1:11,
                                     type = as.character(representative_type))
    if(val %in% representative_map$id) {
      return(representative_map$type[representative_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  sampling_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575829
    sampling_type <- c("Not set", "Cluster", "Multistage", "Nonprobability",
                       "Probability", "Simple random")
    sampling_map <- data.frame(id = c(-1, 1:5), type = as.character(sampling_type))
    if (val %in% sampling_map$id) {
      return(sampling_map$type[sampling_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  source_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=26099974
    source_type_short = c("DSP before 1990", "VR", "SRS", "DSP", "DSS",
                          "Census", "Standard DHS", "Other DHS", "RHS",
                          "PAPFAM", "PAPCHILD", "MICS", "WFS", "LSMS", "MIS",
                          "AIS", "Other", "UNICEF", "VR pre-2009",
                          "VR post-2009", "DSP<1996", "DSP 96-03", "DSP>2003",
                          "DSP 96-00", "VR pre-1978", "VR post-1978", "other")
    source_type_long = c("DSP before 1990", "Vital Registration",
                         "Sample Registrations System",
                         "Disease surveillance points",
                         "Demographic surveillance sites", "Census",
                         "Standard demographic and health survey",
                         "Other demographic and health survey",
                         "Reproductive Health Survey",
                         "Pan Arab Project for Family Health",
                         "Pan Arab Project for Child Development",
                         "Multiple Indicator Cluster Survey",
                         "World Fertility Survey",
                         "Living Standards Measurement Study",
                         "Malaria Indicator Survey", "AIDS Indicator Survey",
                         "Other survey", "UNICEF",
                         "Vital Registration (1978-2008)",
                         "Vital Registration (2009-2013)",
                         "Disease Surveillance Point (1991-1995)",
                         "Disease Surveillance Point (1996-2003)",
                         "Disease Surveillance Point (2004 and after)",
                         "Disease Surveillance Point (1996-2000)",
                         "Vital Registration before 1978",
                         "Vital Registration after 1978", "other")
    source_map = data.frame(id = 0:26,
                            type_short = as.character(source_type_short),
                            type_long = as.character(source_type_long))
    if (val %in% source_map$id) {
      # use long names
      return(source_map$type_long[source_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  uncertainty_type_converter <- function(val) {
    # https://hub.ihme.washington.edu/pages/viewpage.action?pageId=18575836
    uncertainty_type <- c("Not set", "Standard error", "Effective Sample Size",
                          "Confidence Interval", "Sample Size")
    uncertainty_map <- data.frame(id = c(-1, 1:4),
                                  type = as.character(uncertainty_type))
    if (val %in% uncertainty_type) {
      return(uncertainty_map$type[uncertainty_map$id == val])
    }
    else {
      return("Not set")
    }
  }

  # Create a function to apply the maps ----------------------------------------
  map_type_id <- function(var_name, data) {
    # special case for representative because the variables are named differently
    if (var_name == "representative") {
      if (!("representative_name" %in% columns)) {
        if("representative_id" %in% columns) {
          message("creating column 'representative_name' from 'representative_id'")
          input_stgpr_bundle <- input_stgpr_bundle %>%
            mutate(representative_name =
                     sapply(representative_id, representative_type_converter))
        }
        else {
          # neither variable exists in the data
          stop("input bundle must either have column 'representative_name' or 'representative_id'")
        }
      }
    }
    # for all the other variables
    else {
      if (!(paste0(var_name, "_type") %in% columns)) {
        message(paste0("creating column '", var_name, "_type' from '",
                       var_name, "_type_id'"))
        # create a vector to store the mapped variable types
        store <- sapply(input_stgpr_bundle[,(paste0(var_name, "_type_id"))],
                        get(paste0(var_name, "_type_converter")))
        # put the newly created column in the data
        input_stgpr_bundle <- input_stgpr_bundle %>%
          mutate(!!paste0(var_name, "_type") := store)
      }
      else {
        # neither variable type or variable type_id exist inthe data.
        stop(paste0("input bundle must either have column '", var_name,
                    "_type' or '", var_name, "_type_id'"))
      }
    }
    return(input_stgpr_bundle)
  }

  # Rename the column "val" to "mean"
  if (!("mean" %in% columns)){
    message("Renaming col 'val' to 'mean'")
    input_stgpr_bundle <- input_stgpr_bundle %>% rename(mean = val)
  }

  # Map the necessary variables
  var_names <- c("input", "recall", "representative", "sampling", "source", "uncertainty")
  # Doing this in a for loop because each iteration of the dataset builds on the next one
  for (var_name in var_names) {
    input_stgpr_bundle <- map_type_id(var_name, input_stgpr_bunde)
  }

  # NOTE: might need to map unit type
  return(input_stgpr_bundle)
}
