################################################################################
# HMS 520 
# Bundle Converter
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################
source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")

# pull in sample DisMod bundle
sample_DisMod <- get_bundle_data(bundle_id = 435, 
                                 decomp_step = "iterative",
                                 gbd_round_id = 7)

sample_STGPR <- get_crosswalk_version(crosswalk_version_id = 31250)
