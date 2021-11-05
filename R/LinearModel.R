#' Test Linear Relationship
#'
#' @description When given 2 numerical vectors the function will test if there is a linear relationship between them. The resulting output will include a hypothesis, assumptions, test statistics, decision and a conclusion.
#'
#' @param X First variable data
#' @param Y Second variable data
#' @param xName The name of the first variable
#' @param yName The name of the second variable
#' @param sig.fig Significance level to be tested at
#'
#' @return output list containing the hypothesis, assumptions, test statistics, decision and a conclusion.
#' @export
#'
#' @examples
#' LinearModel(seq(100, 200, 2.5), runif(41, 100, 200))
LinearModel <- function(X = ProjectData$height, Y = ProjectData$weight, xName = "height", yName = "weight", sig.fig = 0.05) {

  dta <- data.frame(X, Y)
  colnames(dta) <- c(xName, yName)

  LMod <- lm(Y~X)

  hyp <- glue::glue("Testing if there is a linear relationship between {xName} and {yName} of the form {yName} = alpha + beta * {xName} + epsilon.
  The Null Hypothesis, H0: B = 0, theres no linear relationship.
  The Alternate H1: B isn't 0, there is a linear relationship.")

  assumptions <- glue::glue("The underlying assumptions for a simple linear regression test are that there exists a linear relationship between the variables, the residuals are independent and normally distributed. These assumptions are checked through the below plots.
                            Ensure that the assumptions of the test are met before considering the latter results. ")

  scatterplot <- ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x = X, y = Y))+
    ggplot2::ggtitle(glue::glue("Scatterplot for {yName} vs {xName}"))+
    ggplot2::ylab(yName)+
    ggplot2::xlab(xName)

  fitvsres <- ggplot2::ggplot(LMod)+
    ggplot2::geom_point(ggplot2::aes(x = LMod$residuals, y = LMod$fitted))+
    ggplot2::ggtitle("residuals vs fitted")+
    ggplot2::ylab(yName)+
    ggplot2::xlab(xName)



  reshist <- ggplot2::ggplot(data = LMod)+
    ggplot2::geom_histogram(ggplot2::aes(x = LMod$residuals), binwidth = 0.5)+
    ggplot2::labs(title = "histogram of residuals")+
    ggplot2::xlab("residuals")



  plots <- c(scatterplot, fitvsres, reshist)


  # Finding relevant test statistics
  beta <- LMod$coefficients[2]
  alpha <- LMod$coefficients[1]
  con_int <- confint(LMod, level = 1-sig.fig)   #function gives confidence interval
  degf <- LMod$df

  sum_lm <- summary(LMod) #use a summary of lm for test statistics
  tval <- sum_lm$coefficients[2,3]
  pval <- sum_lm$coefficients[2,4]

  stat_results <- glue::glue("The slope B is {beta}, and corresponding model estimate is; {yName} = {alpha} + {beta} * {xName} + epsilon
             The t-value is {tval}, with {degf} degrees of freedom, with a corresponding to a p-value of {pval}.")


  # Decision and Conclusion

   decision <-
     if(pval < sig.fig){
          glue::glue("Reject the null hypothesis in favour of the alternative as the reported
                   p-value, {pval}, is less than the {100*sig.fig}% significance level.")
        }else{
          glue::glue("There is insufficient evidence to reject the null hypothesis as the
        p-value, {pval}, is above the {100*sig.fig}% significant level.")
        }


  conclusion <-
    if(pval < sig.fig){
      glue::glue("The Linear Regression null hypothesis has been refected there must be a linear relationship
               between {yName} and {xName}. For every unit change in {xName} we expect {yName} to shift by {beta}.
               {yName} = {alpha} + {beta} * {xName} + epsilon" )
    }else{
      glue::glue("The Linear Regression null hypothesis has not been rejected, there is no statistically significant linear relationship between {yName} and {xName}.")
    }





  return(list("hypothesis" = hyp,"plot1" = scatterplot, "plot2" = fitvsres, "plot3" = reshist, "assumptions" = assumptions,
              "lm_data" = list("coefficients" = c(beta, alpha), "pvalue" = pval, "teststat" = tval), "Report" = stat_results,
              "decision" = decision, "conclusion" = conclusion))

}
