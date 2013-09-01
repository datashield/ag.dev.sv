\name{ag.table1d.ds}
\alias{ag.table1d.ds}
\title{Creates 1 dimensional contingency tables - potentially disclosive data are suppressed}
\usage{
  ag.table1d.ds(vect1)
}
\arguments{
  \item{vect1}{A single vector (variable) with a limited
  series of discrete values (numeric or alphanumeric) -
  typically a factor}
}
\value{
  A list object with one component from each separate study
  which contains the following items where out.obj is the
  assigned output list obj: \code{is.table.valid} For
  example, out.obj[[2]]$is.table.valid is a logical
  (Boolean) indicator of whether the data in study 2 is
  entirely valid (TRUE) for the stated variable or at least
  one category is invalid (FALSE) \code{safe.table} For
  example, out.obj[[1]]$safe.table contains the safe
  (non-disclosive) one dimensional table that can be
  released from study 1 - unsafe data are concealed by
  converting invalid counts to "-1" and their corresponding
  catregory identifier to "-9"
}
\description{
  The function pbs.table.1d is a server-side subfunction of
  datashield.aggregate(). It generates 1-dimensional table
  for all data sources. Valid (non-disclosive) data are
  defined as data from studies where no table cells have
  counts between 1 and 4 (the upper value [4] can in
  principle be changed but only by rewriting the underlying
  function - it cannot be changed by a standard DataSHIELD
  user). If the count in any cell in the table produced by
  a given data source IS invalid, that cell count is
  changed to "-1" and the name of the category
  corresponding to the count is changed to "-9"
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("DIS_DIAB")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate the two-dimensional table
out.obj <- datashield.aggregate(opals, quote(ag.table1d.ds(D$DIS_DIAB)))
}
}
\author{
  Burton, P.
}

