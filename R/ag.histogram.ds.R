#' Computes a histogram of the given data values without plotting.
#'
#' @param a a vector of values for which the histogram is desired.
#' @export
#' @author Gaye, A.
#' 
ag.histogram.ds <- function (a) {
  hist(a,plot=FALSE)
}
