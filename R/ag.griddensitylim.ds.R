#' 
#' @title Generates a density grid with a priori defined limits
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @param x.min a minimum value for the x axis of the grid density object
#' @param x.max a maximum value for the x axis of the grid density object
#' @param y.min a minimum value for the y axis of the grid density object
#' @param y.max a maximum value for the y axis of the grid density object
#' @param numints a number of intervals for the grid density object, by default is 20
#' @return a list which a grid density matrix and the number of invalid cells
#' @author Isaeva, J. and Gaye, A.
#' @export
#' 
ag.griddensitylim.ds  <-  function(xvect,yvect, x.min, x.max, y.min, y.max, numints=20){
  
  xvect.save <- xvect
  yvect.save <- yvect
  
  xvect.not.missing <- 1-(is.na(xvect)*1)
  yvect.not.missing <- 1-(is.na(yvect)*1)
  
  x.and.y.present <- xvect.not.missing*yvect.not.missing
  
  xvect <- xvect.save[x.and.y.present==1]
  yvect <- yvect.save[x.and.y.present==1]
  
  
  y.range <- y.max-y.min
  x.range <- x.max-x.min
  
  y.interval <- y.range/numints
  x.interval <- x.range/numints
  
  y.cuts <- seq(from=y.min,to=y.max,by=y.interval)
  y.mids <- seq(from=(y.min+y.interval/2),to=(y.max-y.interval/2),by=y.interval)
  y.cuts[numints+1] <- y.cuts[numints+1]*1.001
  
  x.cuts <- seq(from=x.min,to=x.max,by=x.interval)
  x.mids <- seq(from=(x.min+x.interval/2),to=(x.max-x.interval/2),by=x.interval)
  x.cuts[numints+1] <- x.cuts[numints+1]*1.001
  
  
  grid.density <- matrix(0,nrow=numints,ncol=numints)
  cell.count <-  0
  
  for(j in 1:numints)
  {
    for(k in 1:numints)
    {
      grid.density[j,k] <- sum(1*(yvect>=y.cuts[j] & yvect<y.cuts[j+1] & xvect >=x.cuts[k] & xvect<x.cuts[k+1]), na.rm=TRUE)
      
      if ( (grid.density[j,k]>0) & (grid.density[j,k]<=4) ) {
        grid.density[j,k]  <-  0
        cell.count  <-  cell.count+1
      }
      
    }
  }
  
  print(cell.count)
  grid.density.obj <- cbind(grid.density,x.mids,y.mids)
  
  #title.text = paste('Number of invalid cells (cells with counts >0 and <5) is ',cell.count, sep='')
  #cat(title.text,"\n")
  
  #names(dimnames(grid.density.obj))[2] = title.text
  #names(dimnames(grid.density.obj))[1] = ''
  
  return(list(grid.density.obj,cell.count))
  
}