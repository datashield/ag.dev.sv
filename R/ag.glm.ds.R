#' Generalized linear modellling
#'
#' @title Fits a generalized linear model
#' @description THIS FUNCTION IS NOT USED ALONE, IT IS CALLED BY THE FUNCTION \code{ds.glm} FROM THE PACKAGE \code{dsmodellingclient}.
#' @param formula an object of class \code{formula}
#' @param family a description of the error distribution and link function to
#' used in the model
#' @param beta.vect starting values for the parameters in the linear predictor
#' @return a list which contains: the fitted \code{family}, a score vector and an information matrix
#' @author Burton, P.; Laflamme, P.; Gaye, A.
#' @export
#' @examples {
#' # Please see the examples in the documentation of the function 'ds.glm' from the package 'dsmodellingclient'
#' }
#'
ag.glm.ds <- function (formula, family, beta.vect=NULL) {

  mod.glm.ds <- glm(formula, family=family, x=TRUE, control=glm.control(maxit=1), constrast=NULL)

  X.mat <- as.matrix(mod.glm.ds$x)

  if(is.null(beta.vect)) {
    beta.vect <- rep(0,dim(X.mat)[2])
  }

  numsubs<-dim(X.mat)[1]

  y.vect<-as.vector(mod.glm.ds$y)

  lp.vect<-X.mat%*%beta.vect

  f<-mod.glm.ds$family

  mu.vect<-f$linkinv(lp.vect)
  mu.eta.val<-f$mu.eta(lp.vect)
  var.vect<-f$variance(mu.vect)
  dev<-sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))

  W.vect<-as.vector(mu.eta.val^2/var.vect)
  WX.mat<-W.vect*X.mat
  info.matrix<-t(X.mat)%*%WX.mat

  u.vect<-(y.vect-mu.vect)*1/var.vect
  W.u.mat<-matrix(W.vect*u.vect)
  score.vect<-t(X.mat)%*%W.u.mat

  list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev)
}
