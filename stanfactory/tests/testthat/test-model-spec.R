test_that("model_spec constructs with sane defaults", {
  s <- model_spec("m3", "y",
                  estimands = c(ate = "est_ate", cate = "est_cate"),
                  arms = c(treated = "mu_treated", control = "mu_control"))
  expect_s3_class(s, "model_spec")
  expect_equal(s$y_rep, "y_rep")
  expect_equal(s$log_lik, "log_lik")
  expect_equal(unname(s$estimands), c("est_ate", "est_cate"))
})

test_that("optional fields accept NULL", {
  expect_null(model_spec("m", "y", log_lik = NULL)$log_lik)
})

test_that("malformed declarations error", {
  expect_error(model_spec("m"))
  expect_error(model_spec("m", "y", estimands = c("est_ate")))
  expect_error(model_spec("m", "y", arms = c("mu_treated")))
  expect_error(model_spec("m", "y", groups = list(effects = "a")))
})

test_that("well-formed groups pass", {
  s <- model_spec("m", "y", groups = list(effects = "a", grand = "mu", sd = "tau"))
  expect_false(is.null(s$groups))
})
