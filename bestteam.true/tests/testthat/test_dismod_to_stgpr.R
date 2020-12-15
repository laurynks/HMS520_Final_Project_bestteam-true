################################################################################
# HMS 520
# Bundle Converter: DisMod to ST-GPR Test
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

test_that("dismod_to_stgpr", {
  source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
  expect_error(dismod_to_stpgr("a"))
  rm(sample_DisMod)
  sample_DisMod <- get_bundle_data(bundle_id = 435,
                                   decomp_step = "iterative",
                                   gbd_round_id = 7)
  expect_warning(dismod_to_stgpr(sample_DisMod))
  sample_dm_run <- dismod_to_stgpr(sample_DisMod)
  expect_error(dismod_to_stgpr(sample_DisMod, "Iterative"))
  expect_true("input_type_id" %in% colnames(sample_dm_run))
  expect_true("recall_type_id" %in% colnames(sample_dm_run))
  expect_true("representative_id" %in% colnames(sample_dm_run))
  expect_true("sampling_type_id" %in% colnames(sample_dm_run))
  expect_true("source_type_id" %in% colnames(sample_dm_run))
  expect_true("uncertainty_type_id" %in% colnames(sample_dm_run))
  expect_true("val" %in% colnames(sample_dm_run))
  expect_false("mean" %in% colnames(sample_dm_run))
})
