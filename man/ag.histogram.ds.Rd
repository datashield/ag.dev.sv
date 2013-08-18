\name{ag.histogram.ds}
\alias{ag.histogram.ds}
\title{Computes a histogram of the given data values without plotting.}
\usage{
  ag.histogram.ds(xvect)
}
\arguments{
  \item{xvect}{numeric vector for which the histogram is
  desired.}
}
\value{
  an object of class \code{histogram}
}
\description{
  this functions produces the information required to plot
  a histogram. This is done without allowing for bins
  (cells) with a count of less than 5. If a bin has a count
  < 5 it is collapsed with the nearing bin; this process
  iterates until all bins have count >=5.
}
\examples{
\dontrun{
# load the file that contains the login details
library(ag.dev.cl)
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# use the function to generate the histogram object
his.object <- datashield.aggregate(opals, quote(ag.histogram.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

