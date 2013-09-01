\name{ag.table2d.ds}
\alias{ag.table2d.ds}
\title{Creates 2-dimensional contingency tables - potentially disclosive data are suppressed}
\usage{
  ag.table2d.ds(obj1)
}
\arguments{
  \item{obj1}{One parameter but it combines two vectors
  (variables) each with a limited series of discrete values
  (numeric or alphanumeric) - typically factors. Note they
  must be specified as a single object with the two vectors
  linked using cbind().}
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
  The function pbs.table.2d is a server-side subfunction of
  datashield.aggregate(). It generates 2-dimensional tables
  for all data sources. Valid (non-disclosive) data are
  defined as data from studies where no table cells have
  counts between 1 and 4 (the upper value [4] can in
  principle be changed but only by rewriting the underlying
  function - it cannot be changed by a standard DataSHIELD
  user). If the count in any cell in the table produced by
  a given data source IS invalid, that cell count is
  changed to "-1" and the name of the two categories that
  correspond to it are both changed to "-9"
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("DIS_DIAB","GENDER")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate the two-dimensional table
out.obj <- datashield.aggregate(opals, obj1=quote(ag.table2d.ds(cbind(D$DIS_DIAB,D$GENDER))))
}
}
\author{
  Burton, P.
}

