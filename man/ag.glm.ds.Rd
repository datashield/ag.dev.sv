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
\examples{
\dontrun{
# load the file that contains the login details
library(ag.dev.cl)
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# specify the relevant arguments
formula <- D$DIS_DIAB~D$PM_BMI_CONTINUOUS+D$LAB_HDL
family <- quote(binomial)
betas <- c(0,0,0)

# run the function
 mod <- datashield.aggregate(opals, quote(ag.glm.ds(formula, fanily, betas)))
}
}
\author{
  Burton, P.; Laflamme, P.; Gaye, A.
}

