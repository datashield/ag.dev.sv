\name{ag.createfactor.ds}
\alias{ag.createfactor.ds}
\title{Generates a factor variable}
\usage{
  ag.createfactor.ds(xvect, levels = NULL, labels = levels,
    exclude = NA, ordered = is.ordered(xvect))
}
\arguments{
  \item{xvect}{a numerical or character vector}

  \item{levels}{optional vector of values that 'xvect'
  might have taken}

  \item{labels}{labels for the different levels}

  \item{exclude}{values to be excluded}

  \item{ordered}{tells if the variable 'x' is ordered}
}
\value{
  a 'valid' factor variable
}
\description{
  Generates a factor variable
}
\author{
  Gaye, A.; Burton, P.
}

