#' Test 2 sample means
#'
#' @description When given 2 numerical vectors the function will test if there is a statistically significant difference between each of their means.
#'
#' @param x First variable data, default is ProjectMale$height
#' @param y Second variable data, default is ProjectFemale$height
#' @param cata The category for each variable to be tested, default is height
#' @param xname The name of the first variable, default Male
#' @param yname The name of the second variable, default Female
#' @param sig.fig Significance level to be tested at, default 0.05 (5%)
#'
#' @return Output list containing the hypothesis, assumptions, test statistics, decision and a conclusion of the test.
#' @export
#'
#' @examples
#' ttest_report(rpois(20, 10), rpois(20, 11), cata = "teeth")
ttest_report <- function(x = ProjectMale$height, y = ProjectFemale$height, cata = "height", xname = "Male", yname = "Female", sig.fig = 0.05) {

  hyp <- glue::glue("Testing if the mean {cata} for {xname} and {yname} are the same the following hypothesis test will be tested using a t-test.
                  Null hypothesis H0: mean {cata} of {xname} and {yname} are equal.
                  Alternative hypothesis H1: The {cata}'s are NOT equal.")

  assumptions <-
    glue::glue("The assumptions for a t-test are that the samples are that both groups are independent, random selected, have equal variances and follow a normally distribution. Random selection and equal variances are going to be assumed, but to confirm both samples are normally distributed they need to have a fairly normal bell looks when plotted.")


  #QQ plots to test the normalcy of the data
  qq1 <-  ggplot2::ggplot()+
    ggplot2::stat_qq(ggplot2::aes(sample = x))+
    ggplot2::labs(title = glue::glue("Normal Q-Q plot for {xname}"))+
    ggplot2::ylab(glue::glue("{xname} {cata}"))

  qq2 <-  ggplot2::ggplot()+
    ggplot2::stat_qq(ggplot2::aes(sample = y))+
    ggplot2::labs(title = glue::glue("Normal Q-Q plot for {yname}"))+
    ggplot2::ylab(glue::glue("{yname} {cata}"))

  qqplots <- gridExtra::grid.arrange(qq1, qq2)


  # use t.test to find the relevant test statistics
  stats <- t.test(x, y, conf.level = 1-sig.fig)
  pval <- stats$p.value

  # Decision and Conclusion

  decision <-
    if(pval < sig.fig){
      glue::glue("Reject the null hypothesis in favour of the alternative as the reported p-value, {pval}, is less than the {100*sig.fig}% significance level.")
    }else{
      glue::glue("There is insufficient evidence to reject the null hypothesis as the
        p-value, {pval}, is above the {100*sig.fig}% significant level.")
    }

  conclusion <-
    if(pval < sig.fig){
      glue::glue("The t-test null hypothesis has been refected there must be a difference between the mean {cata} for {xname} and {yname}.
      Further testing will be needed to conclude if one is significantly lesser or greater than the other.")
    }else{
      glue::glue("The t-test null hypothesis has not been rejected there is no statistically significant difference in the mean {cata} for {xname} and {yname}.")
    }


  return(list("hypothesis" = hyp,"plots" = qqplots, "assumptions" = assumptions,
              "test.stats" = stats, "decision" = decision, "conclusion" = conclusion))

}

