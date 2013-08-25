#' 
#' @title Provides quantiles and mean information without maximum and minimum
#' @param xvect a numerical vector
#' @return a numeric vector that represents the sample quantiles 
#' @export
#' @author Burton, P.; Gaye, A.
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
#' # compute the sample quantiles that correspond to the probabilities specified internally
#' quantiles <- datashield.aggregate(opals, quote(ag.quantilemean.ds(D$LAB_TSC)))
#' }
#' 
ag.quantilemean.ds<- function (xvect) {
  qq <- quantile(xvect,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)
  mm <- mean(xvect,na.rm=TRUE)
  quantile.obj <- c(qq, mm)
  
  names(quantile.obj) <- c("5%","10","25%","50%","75%","90%","95%","Mean")
  
  return(quantile.obj)
}
