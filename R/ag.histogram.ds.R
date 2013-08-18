#' 
#' @title Computes a histogram of the given data values without plotting.
#' @description this functions produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with a
#' count of less than 5. If a bin has a count < 5 it is collapsed with 
#' the nearing bin; this process iterates until all bins have count >=5.
#' @param xvect numeric vector for which the histogram is desired.
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
#' # use the function to generate the histogram object
#' his.object <- datashield.aggregate(opals, quote(ag.histogram.ds(D$LAB_TSC)))
#' }
#' 
ag.histogram.ds <- function (xvect) {
  
  # get the histogram object
  histout <- hist(xvect,plot=FALSE)
  
  # check if any of the 'bins' contains a count < 5
  ch <- length(which(histout$count < 5))
  
  # if any 'bin' contains a count < 5 use larger bins (i.e. less bins)
  # this process continues until all counts > 5
  if(ch > 0){
    while(ch > 0){
      # get the vector of break points and its length
      brkpts <- histout$breaks
      l.brkpts <- length(brkpts)
      
      # indices of the bins with counts < 5
      indx <- which(histout$count < 5)
      
      # combine the break points where count < 5 with the nearest break point
      # if the small count is on the left tail of the histogram the nearest is 
      # on the right and on the left if the small count is on the right tail
      # by 'combine' I mean just removing the nearest break hence merging two bins.
      midpoint <- l.brkpts/2
      if(indx[1] < midpoint){
        new.brkpts <- brkpts[-(indx[1]+1)]
      }
      if(indx[1] > midpoint){
        new.brkpts <- brkpts[-(indx[1])]
      }      
      
      # use the new vector of break points
      histout <- hist(xvect,plot=FALSE, breaks=new.brkpts)
      
      # check the counts in the bins
      ch <- length(which(histout$count < 5))
    }
  }

  return(histout)
}
