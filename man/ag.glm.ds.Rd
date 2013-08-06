\name{ag.glm.ds}
\alias{ag.glm.ds}
\title{Fits a generalized linear model}
\usage{
  ag.glm.ds(formula, family, beta.vect = NULL)
}
\arguments{
  \item{formula}{an object of class \code{formula}}

  \item{family}{a description of the error distribution and
  link function to used in the model}

  \item{beta.vect}{starting values for the parameters in
  the linear predictor}
}
\value{
  a list which contains: the fitted \code{family}, a score
  vector and an information matrix
}
\description{
  Generalized linear modellling
}
\author{
  Burton, P.; Laflamme, P.; Gaye, A.
}

