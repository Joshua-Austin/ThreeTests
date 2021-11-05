test_that("test LinearModel works", {
  set.seed(314159)
  expect_equal(LinearModel(seq(100, 200, 5), runif(21, 100, 200))$decision,
               glue::glue("There is insufficient evidence to reject the null hypothesis as the
p-value, 0.299545509787916, is above the 5% significant level."))
  expect_error(LinearModel(1:5, 1:10))
})
