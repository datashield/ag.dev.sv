\name{ag.t.testhelper.ds}
\alias{ag.t.testhelper.ds}
\title{Produces the input required by the function 'ds.t.test'}
\usage{
  ag.t.testhelper.ds(x, y = NULL,
    alternative = "two.sided", mu = 0, paired = FALSE,
    var.equal = FALSE, conf.level = 0.95)
}
\arguments{
  \item{x}{a numeric vector of data values}

  \item{y}{a numeric vector of data values}

  \item{alternative}{an integer specifying the alternative
  hypothesis, 1 for "two.sided" (default), 2 for "greater"
  or 3 for "less".}

  \item{mu}{a number indicating the true value of the mean
  (or difference in means if you are performing a two
  sample test).}

  \item{paired}{a logical indicating paired non-paired
  t-test.}

  \item{var.equal}{a logical variable indicating whether to
  treat the two variances as being equal.  If 'TRUE' then
  the pooled variance is used to estimate the variance
  otherwise the Welch (or satterthwaite) approximation to
  the degrees of freedom is used.}

  \item{conf.level}{confidence level of the interval.}
}
\value{
  a list that contains the length, mean and variance of the
  variable x
}
\description{
  This function is not used alone it is called by
  'ds.t.test'
}
\author{
  Gaye, A., Isaeva, J.
}

