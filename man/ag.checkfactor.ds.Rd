\name{ag.checkfactor.ds}
\alias{ag.checkfactor.ds}
\title{Verifies that a factor vector is valid}
\usage{
  ag.checkfactor.ds(xvect)
}
\arguments{
  \item{xvect}{a numeric of character vector of type
  \code{factor}}
}
\value{
  a list which contains two elements: \code{summary table},
  the results of the tabulation of the input vector
  \code{status}, tells if the factor is valid, 1 if one or
  more levels have counts of between 0 and 5 and 0
  otherwise.
}
\description{
  This function checks if a numeric or character factor
  vector contains levels with counts > 0 and < 5.
}
\author{
  Gaye, A.; Newby, C.
}

