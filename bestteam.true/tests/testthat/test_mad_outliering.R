################################################################################
# HMS 520
# MAD Outliering Function Test
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

test_that("mad_outliering", {
  source("/ihme/cc_resources/libraries/current/r/get_bundle_version.R")
  expect_error(mad_outliering("a"))
  test_df <- get_bundle_version(bundle_version_id = 22562, fetch = "all")
  expect_error(mad_outliering(test_df))
  expect_error(mad_outliering(test_df, 19, 7, c("location_id", "sex", "year_start", "year_end", "nid")))
  t1 <- mad_outliering(test_df, 19, 7, c("location_id", "sex", "year_start", "year_end", "nid"), 2)
  expect_false("as_mean" %in% colnames(t1))
  expect_true("age_start" %in% colnames(t1))
  expect_equal(nrow(test_df), nrow(t1))
  expect_equal(ncol(test_df), ncol(t1))
})
