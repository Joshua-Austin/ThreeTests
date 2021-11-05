test_that("test ttest_report works", {
  set.seed(314159)
  expect_equal(ttest_report(rpois(20, 10), rpois(20, 11), cata = "teeth")$test.stats$p.value, 0.12740663)
  expect_equal(ttest_report()$decision,
               glue::glue("Reject the null hypothesis in favour of the alternative as the reported p-value, 2.77267926623825e-209, is less than the 5% significance level."))
  expect_error(ttest_report(letters[1:20]))

})
