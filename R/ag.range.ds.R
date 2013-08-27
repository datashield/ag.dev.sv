#' 
#' @title returns the minimum and maximum of a numeric vector
#' @description this function is similar to R function \code{range} but instead to not return 
#' the real minimum and maximum, the computed values are multiplied by a very small random number. 
#' @param xvect a numerical 
#' @return  a numeric vector which contains the minimum and the maximum
#' @author Gaye, A.
#' @export
#' @examples 
#'  \dontrun{
#' # load the login data
#' library(dsbaseclient)
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # check if the variable 'LAB_HDL' is empty
#' datashield.aggregate(opals, quote(range.ds(D$LAB_HDL)))
#' }
#'
ag.range.ds <- function(xvect){
  
  # print an error message if the input vector is not a numeric
  if(!(is.numeric(xvect))){
    stop("The input vector is not a numeric!")
  }else{
    rr <- c(min(xvect, na.rm=TRUE), max(xvect, na.rm=TRUE))
    output <- rr * runif(2, 1, 1.1)
  }
  return (output)
}