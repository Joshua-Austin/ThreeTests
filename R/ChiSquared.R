#' Compare 2 Categorical Variables with Chi-Squared
#'
#' @description When passed 2 vectors for different categories the function will test if there is a difference in distribution because of the different categories.
#'
#' @param x First variable data, default is ProjectData$gender
#' @param y Second variable data, default is ProjectData$phys
#' @param xname The name of the first variable, default Gender
#' @param yname The name of the second variable, default Physical Activity
#' @param sig.fig Significance level to be tested at, default 0.05 (5%)
#'
#' @return Output list containing the hypothesis, assumptions, test statistics, decision and a conclusion of the test.
#' @export
#'
#' @examples
#' ChiSquared(x = sample(c('male', 'female'), 100, replace=TRUE),
#' y = sample(c('bald', 'short', 'long'), 100, replace=TRUE))
ChiSquared <- function(x = ProjectData$gender, y = ProjectData$phys, xname = "Gender", yname = "Physical Activity", sig.fig = 0.05) {

  hyp <- glue::glue("Testing if there is a relationship between the categorical variables {xname} and {yname}.
                  The null hypothesis H0: {xname} and {yname} are independent.
                  Alternative H1: There is a relationship between {xname} and {yname}.")

  assumptions <-
    glue::glue("The assumptions of a chi-squared test are that the variables are categorical, all observations are independent and mutually exclusive of each other, and the expected value in at least 80% of cells is 5 or more (or all are over 5 if there are one 4 cells).
               The function used to generate this chi-squared report checks if the input variables are categorical, it will produce an error if this assumption isn't met. Similarly a warming will be generated if too many cells are below 5. The other assumptions can't be tested directly as they rely on the data source, can only ask or trust that the information was gathered correctly.")

  if(!is.character(x)||!is.character(y)){
    stop("Error: All input need to be of type categorical.")
  }

  # use chisq.test to find the relevant test statistics
  stats <- chisq.test(x, y)
  pval <- stats$p.value



  # Decision and Conclusion

  decision <-
    if(pval < sig.fig){
      glue::glue("Reject the null hypothesis in favour of the alternative as the reported p-value, {pval}, is less than the {100*sig.fig}% significance level.")
    }else{
      glue::glue("There is insufficient evidence to reject the null hypothesis as the p-value, {pval}, is above the {100*sig.fig}% significant level.")
    }

  conclusion <-
    if(pval < sig.fig){
      glue::glue("The Chi-Squared null hypothesis has been refected there must be a relationship between catagorical variables {xname} and {yname}.
      Further testing will be needed to find exactly what the relationship is.")
    }else{
      glue::glue("The Chi-Squared null hypothesis has not been rejected there is no statistically significant relationship between {xname} and {yname}.")
    }


  return(list("hypothesis" = hyp, "assumptions" = assumptions,
              "Chi.stats" = stats, "decision" = decision, "conclusion" = conclusion))

}

