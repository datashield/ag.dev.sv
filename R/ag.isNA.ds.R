#' 
#' @title Checks if a vector is empty 
#' @description this function is similar to R function \code{is.na} but instead of a vector 
#' of booleans it returns just one boolean to tell if all the element are missing values.
#' @param xvect a numerical or character vector
#' @return the integer '1' if the vector contains on NAs and '0'  otherwise
#' @author Gaye, A.
#' @export
#' @examples 
#' \dontrun{
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # check if the variable 'LAB_HDL' is empty
#' datashield.aggregate(opals, quote(ag.isNA.ds(D$LAB_HDL)))
#' }
#'
ag.isNA.ds <- function(xvect){
  
  out <- is.na(xvect)
  mm <- mean(out, na.rm=TRUE)
  if(mm==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}