#' 
#' @title Performs one and two sample t-tests
#' @param xvect a numeric vector of data values
#' @param yvect a numeric vector of data values
#' @param alternative an integer specifying the alternative hypothesis,
#' 1 for "two.sided" (default), 2 for "greater" or 3 for "less".  
#' @param mu a number indicating the true value of the mean (or difference
#' in means if you are performing a two sample test).
#' @param paired a logical indicating paired non-paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal.  If 'TRUE' then the pooled variance
#' is used to estimate the variance otherwise the Welch (or satterthwaite) 
#' approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @return a list of class \code{htest}, see the R function \code{t.test} in 
#' package \code{stats} for details of the elements in the returmed object.
#' @author Burton P.; Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' library(dsbaseclient)
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # run a t.test
#' datashield.aggregate(opals, quote(t.test.ds(D$LAB_TSC, D$LAB_HDL)))
#' }
#'
ag.t.test.ds <- function (xvect, yvect = NULL, alternative = 1, mu = 0, paired = FALSE, var.equal = FALSE,
                    conf.level = 0.95) {
  if(alternative == 1){alternative <- "two.sided"}
  if(alternative == 2){alternative <- "less"}
  if(alternative == 3){alternative <- "greater"}
  output <- t.test(xvect, yvect, alternative, mu, paired, var.equal, conf.level)
  return (output)
}