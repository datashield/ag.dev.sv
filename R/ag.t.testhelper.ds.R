#' 
#' @title Produces the input required by the function 'ds.t.test'
#' @description This function is not used alone it is called by 'ds.t.test'
#' @param x a numeric vector of data values
#' @param y a numeric vector of data values
#' @param alternative an integer specifying the alternative hypothesis,
#' 1 for "two.sided" (default), 2 for "greater" or 3 for "less".  
#' @param mu a number indicating the true value of the mean (or difference
#' in means if you are performing a two sample test).
#' @param paired a logical indicating paired non-paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal.  If 'TRUE' then the pooled variance
#' is used to estimate the variance otherwise the Welch (or satterthwaite) 
#' approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @return a list that contains the length, mean and variance of the variable x
#' @author Gaye, A., Isaeva, J.
#' @export
#'
ag.t.testhelper.ds <- function (x, y = NULL, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95){  
  
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))){
    stop("'mu' must be a single number")
  }
  
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1)){
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  if (!is.null(y)){
    if (paired){
      pair.compl.obs <- complete.cases(x, y)
      xok <- subset(x, pair.compl.obs)
      yok <- subset(x, pair.compl.obs)
    }else{
      not.na.x <- complete.cases(x)
      xok <- subset(x, not.na.x)
      not.na.y <- complete.cases(y)    
      yok <- subset(x, not.na.y)
    }
  }else{
    if (paired){
      stop(paste(y, " is missing for paired test", sep=""))
    }
    not.na.x <- complete.cases(x)
    xok <- subset(x, not.na.x)    
  }
  
  if (paired){
    minus_y <- yok * (-1)
    xok <- sum(xok, minus_y)
    yok <- as.null(yok)
  }
  
  length.local.x <- NROW(xok)
  mean.local.x <- mean(xok, na.rm=TRUE)
  var.local.x <- var(xok, na.rm=TRUE)  
  
  return(list("length.local.x"=length.local.x, "mean.local.x"=mean.local.x, "var.local.x"=var.local.x))
  
}