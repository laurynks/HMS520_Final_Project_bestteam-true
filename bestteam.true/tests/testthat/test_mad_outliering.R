################################################################################
# HMS 520
# MAD Outliering Function Test
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

source("/ihme/cc_resources/libraries/current/r/get_bundle_version.R")

test_that("mad_outliering",{
  expect_error(mad_outliering())
})

