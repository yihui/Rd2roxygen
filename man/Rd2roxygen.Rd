\name{Rd2roxygen}
\alias{Rd2roxygen}
\title{Convert all the Rd files of a package to roxygen documentations}
\usage{
Rd2roxygen(pkg, nomatch, usage = FALSE)
}
\arguments{
  \item{pkg}{the root directory of the package}

  \item{nomatch}{the file name (base name only) to use when
  an object in the Rd file is not found in any R source
  files (typically this happens to the data documentation);
  if not specified, the default will be `pkg'-package.R}

  \item{usage}{logical: whether to include the usage
  section in the output}
}
\value{
  NULL (but the process of conversion will be printed on
  screen)
}
\description{
  This function takes a package root directory, parses all
  its Rd files under the man directory and update the
  corresponding R source code by inserting roxygen
  documentation in to the R scripts.
}
\note{
  ESS users may use \code{options(roxygen.comment = "##'
  ")} to ensure the generated roxygen comments begin with
  \code{"##' "}, which is the default setting in Emacs/ESS.
}
\examples{
\dontrun{
Rd2roxygen("path/to/your/source/package")
}
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
