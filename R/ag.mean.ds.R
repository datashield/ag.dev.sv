#' Computes the mean statistic for vectors with more than 4 entries
#'
#' @title Calculates a statistical mean
#' @param a a numerical vector
#' @return a numeric 
#' @author Gaye A.
#' @export
#' 

ag.mean.ds <- function (a) {
  if(length(a) >= 5){
    mean(a,na.rm=TRUE)
  }else{
    cat("Operation not allowed!\n")
  }
}
