#' 
#' @title Computes a histogram of the given data values without plotting.
#' @description this functions produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with a
#' count of less than 5. If a bin has a count < 5 it is collapsed with 
#' the nearing bin; this process iterates until all bins have count >=5.
#' @param a numeric vector for which the histogram is desired.
#' @return an object of class \code{histogram}
#' @export
#' @author Gaye, A.
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
#' # use the function to genrate the histogram object
#' datashield.aggregate(opals, quote(ag.histogram.ds(D$LB_TSC)))
#' }
#' 
ag.histogram.ds <- function (a) {
  
  # get the histogram object
  histout <- hist(a,plot=FALSE)
  
  # check if any of the 'bins' contains a count < 5
  ch <- length(which(histout$count < 5))
  
  # if any 'bin' contains a count < 5 use larger bins (i.e. less bins)
  # this process continues until all counts > 5
  if(ch > 0){
    # get the vector of break points and its length
    brkpts <- histout$breaks
    l.brkpts <- length(brkpts)
    while(ch > 0){
      # combine the break points at the tails of the histogram
      new.brkpts <- brkpts[-c(2,(l.brkpts-1))]
      
      # use the new vector of break points
      histout <- hist(a,plot=FALSE, breaks=new.brkpts)
      
      # check the counts in the bins
      ch <- length(which(histout$count < 5))
    }
  }
  return(histout)
}
