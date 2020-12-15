################################################################################
# HMS 520
# Weighted Mean Function Test
# Authors: Lauryn Stafford and Jessica Bishai
################################################################################

test_that("weighted_means",{
  source("/ihme/cc_resources/libraries/current/r/get_model_results.R")
  expect_error(weighted_mean("a"))
  test_df <- get_model_results(
    gbd_team = 'epi',
    gbd_id = 25217,
    age_group_id = c(7, 8, 9, 10, 11, 12, 13, 14, 15),
    year_id = 2020,
    sex_id = 2,
    decomp_step = 'iterative'
  )
  expect_error(weighted_mean(test_df))
  expect_error(weighted_mean(test_df, c("foo")))
  t1 <- weighted_mean(test_df, c("sex_id", "location_id"))
  t2 <- weighted_mean(test_df, c("sex_id", "age_group_id"), is_model_results = TRUE)
  test_df_grouped <- test_df %>% group_by(sex_id, age_group_id) %>% summarise(sum = sum(mean))
  expect_gt(nrow(t1), nrow(t2))
  expect_true("location_name" %in% colnames(t1))
  expect_true("sex_id" %in% colnames(t2))
  expect_true("age_group_id" %in% colnames(t2))
  expect_true("age_group_id" %in% colnames(t2))
  expect_equal(nrow(test_df_grouped), nrow(t2))
})
