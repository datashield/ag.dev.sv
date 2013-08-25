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
\examples{
\dontrun{
# load the login data
library(dsbaseclient)
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a factor vector using the variable 'GENDER'
datashield.assign(opals, "gender", quote(ag.createfactor.ds(D$GENDER)))
 }
}
\author{
  Gaye, A.; Burton, P.
}

