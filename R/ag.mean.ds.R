#' Computes the mean statistic for vectors with more than 4 entries
#'
#' @title Calculates a statistical mean
#' @param xvect a numerical vector
#' @return a numeric 
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(ag.dev.cl)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the statistical mean
#' stat.mean <- datashield.aggregate(opals, quote(ag.mean.ds(D$LAB_TSC)))
#' }
#' 

ag.mean.ds <- function (xvect) {
  if(length(xvect) >= 5){
    mean(xvect,na.rm=TRUE)
  }else{
    cat("Operation not allowed!\n")
  }
}
