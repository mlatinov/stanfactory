test_that("names derive deterministically from the identity tuple", {
  expect_equal(bayes_target_name("fit", "m3", dataset_id = "d1"), "fit_m3_d1")
  expect_equal(bayes_target_name("summ", "m3", dataset_id = "d1", estimand_id = "ate"),
               "summ_m3_d1_ate")
  expect_equal(bayes_target_name("recov", "m3", design_id = "g07"), "recov_m3_g07")
  expect_equal(bayes_target_name("stress", "m3", design_id = "g1", suffix = "agg"),
               "stress_m3_g1_agg")
})

test_that("tokens are sanitised and collisions avoided", {
  expect_equal(bayes_target_name("fit", "my model!"), "fit_my_model")
  expect_equal(bayes_target_name("fit", "a---b"), "fit_a_b")
  expect_match(bayes_target_name("3x", "9"), "^t_")
})

test_that("invalid input errors early", {
  expect_error(bayes_target_name(model_id = "m3"))
  expect_error(bayes_target_name("fit", 5))
  expect_error(bayes_target_name("fit", "!!!"))
})
