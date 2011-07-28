\docType{package}
\name{Rd2roxygen-package}
\alias{Rd2roxygen-package}
\title{Convert Rd to roxygen documentation and utilities to enhance
documentation and building packages}
\description{
  This package contains functions to convert Rd to roxygen
  documentation. It can parse an Rd file to a list
  (\code{\link{parse_file}}), create the roxygen
  documentation (\code{\link{create_roxygen}}) and update
  the original R script (e.g. the one containing the
  definition of the function) accordingly
  (\code{\link{Rd2roxygen}}). This package also provides
  utilities which can help developers build packages using
  roxygen more easily (\code{\link{rab}}).
}
\examples{
## see the package vignette: vignette('Rd2roxygen')
}
\author{
  Hadley Wickham and Yihui Xie
}
