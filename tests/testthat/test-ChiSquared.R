test_that("ChiSquared reporting", {
  set.seed(8008)
  expect_equal(ChiSquared(as.character(rbinom(100, 2, 0.3)),
                          sample(c('true', 'false'), 100, replace = TRUE))$Chi.stats$p.value, 0.63267663)
  expect_error(ChiSquared((rbinom(100, 2, 0.3)), sample(c('true', 'false'), 100, replace = TRUE)))
})
