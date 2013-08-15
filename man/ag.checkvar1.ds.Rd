\name{ag.checkvar1.ds}
\alias{ag.checkvar1.ds}
\title{Checks that all studies have the variables included in the glm \code{formula}}
\usage{
  ag.checkvar1.ds(data, servername, formula)
}
\arguments{
  \item{data}{a \code{dataframe} that hold the data
  assigned from the opal datasource to R}

  \item{servername}{name of the login object the data were
  assigned to}

  \item{formula}{an object of class \code{formula} which
  describes the model to be fitted}
}
\value{
  an object of class \code{opal}, the input object (if no
  study has been excluded) or an updated one if some
  study(ies) has been excluded due to missing variables.
}
\description{
  The combined glm fit fails if the outcome variable or one
  of the covariates is missing from any of the studies.
  This function is called by the function \code{ds.glm} to
  ensure that only studies that contains all the required
  variables are included in the analysis. If a study is
  excluded an alert is displayed to inform the user. This
  function is internal (not available to users) as it is
  only required by \code{ds.glm}.
}
\author{
  Gaye, A.
}

