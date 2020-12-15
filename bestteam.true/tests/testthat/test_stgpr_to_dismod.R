################################################################################
# HMS 520
# Bundle Converter: STGPR to DisMod Test
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

test_that("stgpr_to_dismod", {
  source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")
  expect_error(stgpr_to_dismod("a"))
  sample_STGPR <- get_crosswalk_version(crosswalk_version_id = 31250)
  sample_run <- stgpr_to_dismod(sample_STGPR)
  expect_error(stgpr_to_dismod(sample_DisMod, "Iterative"))
  expect_true('input_type' %in% colnames(sample_run))
  expect_true('recall_type' %in% colnames(sample_run))
  expect_true('representative_name' %in% colnames(sample_run))
  expect_true('sampling_type' %in% colnames(sample_run))
  expect_true('source_type' %in% colnames(sample_run))
  expect_true('uncertainty_type' %in% colnames(sample_run))
  expect_false('val' %in% colnames(sample_run))
  expect_true('mean' %in% colnames(sample_run))
})
