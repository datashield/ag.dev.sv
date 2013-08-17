#' 
#' @title Generates a matrix with a minimum and maximum value, for two numerical vector
#' @description This function used by another function to plot a heatmap graph.
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @return a matrix
#' @author Isaeva, J.
#' @export
#' 
ag.MinMax.ds <- function(xvect, yvect) {
  
  MinMaxMatrix <- data.frame(cbind(c(0,0),c(0,0)))
  colnames(MinMaxMatrix) <- c("xvect", "yvect")
  rownames(MinMaxMatrix) <- c("min", "max")
  
  MinMaxMatrix[1, 1] <- min(xvect, na.rm=T)
  MinMaxMatrix[2, 1] <- max(xvect, na.rm=T)
  MinMaxMatrix[1, 2] <- min(yvect, na.rm=T)
  MinMaxMatrix[2, 2] <- max(yvect, na.rm=T)
  
  MinMaxMatrix[1, 1] <- runif(1, 0.9, 1) * MinMaxMatrix[1, 1]
  MinMaxMatrix[2, 1] <- runif(1, 1, 1.1) * MinMaxMatrix[2, 1] 
  MinMaxMatrix[1, 2] <- runif(1, 0.9, 1) * MinMaxMatrix[1, 2]
  MinMaxMatrix[2, 2] <- runif(1, 1, 1.1) * MinMaxMatrix[2, 2]
  
  return(MinMaxMatrix)  
}
