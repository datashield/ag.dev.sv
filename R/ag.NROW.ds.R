#'
#' @title Returns the number of rows or columns present in a 1-column matrix vector
#' @details The number of rows is returned only if the vector contain more than 4 entries.
#' @param xvect a numerical vector
#' @return a numeric 
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' library(dsbaseclient)
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # run the function
#' stat.mean <- datashield.aggregate(opals, quote(NROW(D$LAB_TSC)))
#' }
#' 

ag.NROW.ds <- function (xvect) {
  if(length(xvect) > 0 & length(xvect) < 5){
    NROW(xvect)
  }else{
    cat("Operation not allowed!\n")
  }
}