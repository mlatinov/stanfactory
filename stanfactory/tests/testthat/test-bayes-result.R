test_that("bayes_result constructs and validates", {
  r <- bayes_result("m3", "check",
                    tables = list(diag = data.frame(rhat = 1.01)),
                    plots = list(ppc = 1, rhat = 2),
                    verdict = tibble::tibble(model_id = "m3", kind = "check",
                                             status = "green", max_rhat = 1.01))
  expect_s3_class(r, "bayes_result")
  expect_equal(r$verdict$status, "green")
})

test_that("default verdict is info when none supplied", {
  expect_equal(bayes_result("m", "prior")$verdict$status, "info")
})

test_that("malformed results error", {
  expect_error(bayes_result("m", "nonsense"))
  expect_error(bayes_result("m", "check", verdict = data.frame(status = c("a", "b"))))
  expect_error(bayes_result("m", "check", verdict = data.frame(x = 1)))
})

test_that("verdict() row-binds and fills missing columns", {
  r1 <- bayes_result("m3", "check",
                     verdict = tibble::tibble(model_id = "m3", kind = "check",
                                              status = "green", max_rhat = 1.01))
  r2 <- bayes_result("m4", "stress",
                     verdict = tibble::tibble(model_id = "m4", kind = "stress",
                                              status = "amber"))
  v <- verdict(r1, r2)
  expect_equal(nrow(v), 2)
  expect_true(is.na(v$max_rhat[2]))
  expect_equal(nrow(verdict(list(r1, r2))), 2)
  expect_error(verdict(1, 2))
})
