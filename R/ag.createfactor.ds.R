#' 
#' @title Generates a factor variable 
#' @param xvect a numerical or character vector
#' @param levels optional vector of values that 'xvect' might have taken
#' @param labels labels for the different levels
#' @param exclude values to be excluded
#' @param ordered tells if the variable 'x' is ordered
#' @return a 'valid' factor variable
#' @author Gaye, A.; Burton, P.
#' @export
#' @examples 
#'  \dontrun{
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("GENDER")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a factor vector using the variable 'GENDER'
#' datashield.assign(opals, "gender", quote(ag.createfactor.ds(D$GENDER)))
#'  }
#'
ag.createfactor.ds <- function (xvect, levels=NULL, labels=levels, exclude=NA, ordered=is.ordered(xvect)){
  
  if(is.null(levels)){
    tt <- table(xvect)
    levels <- names(tt)
    labels <- levels
  }
  # use the R function to turn the input vector into a factor
  # if it is not already a factor
  if(!(is.factor(xvect))){
    xvector <- factor(xvect, levels, labels, exclude, ordered)
  }else{
    xvector <- xvect
  }
  
  # call the function that checks of the vector is a valid factor
  out <- ag.checkfactor.ds(xvector) 
  status <- out$status
  
  # if the factor is valid (i.e. no categories with between 0 and 5 counts)
  # return the created factor otherwise print a failure message
  if(status == 0){
    return(xvector) 
  }else{
    return("This is not a valid factor; some categories have  a  count > 0 and < 5!")
  }
  
}