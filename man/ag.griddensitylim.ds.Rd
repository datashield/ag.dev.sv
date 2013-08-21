\name{ag.griddensitylim.ds}
\alias{ag.griddensitylim.ds}
\title{Generates a density grid with a priori defined limits}
\usage{
  ag.griddensitylim.ds(xvect, yvect, x.min, x.max, y.min,
    y.max, numints = 20)
}
\arguments{
  \item{xvect}{a numerical vector}

  \item{yvect}{a numerical vector}

  \item{x.min}{a minimum value for the x axis of the grid
  density object}

  \item{x.max}{a maximum value for the x axis of the grid
  density object}

  \item{y.min}{a minimum value for the y axis of the grid
  density object}

  \item{y.max}{a maximum value for the y axis of the grid
  density object}

  \item{numints}{a number of intervals for the grid density
  object, by default is 20}
}
\value{
  a list which a grid density matrix and the number of
  invalid cells
}
\description{
  Generates a density grid with a priori defined limits
}
\author{
  Isaeva, J. and Gaye, A.
}

