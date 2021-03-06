#' 
#' @title Computes a histogram of the given data values without plotting.
#' @description this functions produces the information required to plot
#' a histogram. This is done without allowing for bins (cells) with a
#' count of less than 5. If a bin has a count < 5 it is collapsed with 
#' the nearing bin; this process iterates until all bins have count >=5.
#' @param xvect the numeric vector for which the histogram is desired.
#' @param min a numerical used as the lower lim of the histogram breaks
#' @param max a numerical used as the upper lim of the histogram breaks
#' @return a list with an object of class \code{histogram} and a vector of x-positions
#' @export
#' @author Gaye, A.
#' @examples 
#' \dontrun{
#' # load the file that contains the login details
#' data(logindata)
#' 
#  # login and assign a numeric variable to R
#  myvar <- list("LAB_TSC")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a histogram object without plotting
#' hist.object <- datashield.aggregate(opals, quote(ag.histogram.ds(D$LAB_TSC)))
#' }
#' 
ag.histogram.ds <- function (xvect, min, max) {
  
  # generate breaks points using the provided range
  brks <- seq(min, max, 0.3)
  
  # get the histogram object
  histout <- hist(xvect, breaks=brks, plot=FALSE)
  
  # check if any of the 'bins' contains a count < 5
  indx <- which(histout$counts > 0 & histout$counts < 5)
  l.small.counts <- length(indx)
  
  if(l.small.counts > 0){

    # replace the corresponding, counts, densities and intensities by zeros
    histout$counts[indx] <- 0
    histout$density[indx] <- 0   
    histout$intensities[indx] <- 0   
    
    # get the midpoints corresponding to the above indices
    # these midpoint will be used to put '*' in the final plot
    axterixpos <- histout$mids[indx]
  }else{
    axterixpos <- NULL
  }
  
  # return a list with the histogram object and the vector 'axterispos'
  return(list("histobject"=histout, "aterix2plot"=axterixpos))
  
}
