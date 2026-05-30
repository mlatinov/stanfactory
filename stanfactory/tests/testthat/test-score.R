test_that("clean diagnostics score green", {
  v <- score_convergence("m", max_rhat = 1.001, min_ess_bulk = 2000,
                          min_ess_tail = 1500, n_divergences = 0)
  expect_equal(v$status, "green")
  expect_true(is.na(v$reasons))
})

test_that("R-hat bands map to amber and red", {
  expect_equal(score_convergence("m", 1.02, 2000, 1500, 0)$status, "amber")
  expect_equal(score_convergence("m", 1.08, 2000, 1500, 0)$status, "red")
})

test_that("any divergence is red and reasons are worded correctly", {
  d3 <- score_convergence("m", 1.001, 2000, 1500, 3)
  expect_equal(d3$status, "red")
  expect_match(d3$reasons, "3 divergent transitions")
  d1 <- score_convergence("m", 1.001, 2000, 1500, 1)
  expect_match(d1$reasons, "1 divergent transition")
})

test_that("low ESS is flagged", {
  expect_equal(score_convergence("m", 1.001, 50, 1500, 0)$status, "red")
  expect_equal(score_convergence("m", 1.001, 300, 1500, 0)$status, "amber")
})

test_that("worst-of-all severity wins and reasons concatenate", {
  mix <- score_convergence("m", 1.02, 2000, 1500, 2)
  expect_equal(mix$status, "red")
  expect_match(mix$reasons, ";")
})

test_that("missing diagnostics do not falsely flag", {
  na <- score_convergence("m", NA_real_, NA_real_, NA_real_, NA_integer_)
  expect_equal(na$status, "green")
})

test_that("score_convergence_from_table extracts worst-case across column spellings", {
  fakefit <- structure(list(), class = "not_cmdstan")
  d1 <- data.frame(variable = c("a", "b"), rhat = c(1.003, 1.20),
                   ess_bulk = c(1200, 90), ess_tail = c(1100, 800))
  v <- score_convergence_from_table("m7", d1, fakefit)
  expect_equal(v$status, "red")
  expect_equal(v$max_rhat, 1.20)
  expect_equal(v$min_ess_bulk, 90)

  d2 <- data.frame(variable = "a", Rhat = 1.004, ess = 1500, ess_tail = 1500)
  expect_equal(score_convergence_from_table("m8", d2, fakefit)$status, "green")
})
