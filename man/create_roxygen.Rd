\name{create_roxygen}
\alias{create_roxygen}
\title{Create the roxygen documentation}
\usage{
create_roxygen(info, usage = FALSE)
}
\arguments{
  \item{info}{the named list of the parsed documentation}

  \item{usage}{logical: whether to include the usage
  section in the output (this can be useful when there are
  multiple functions in a single usage section, but
  generally it is not necessary because roxygen can
  generate the usage section automatically)}
}
\value{
  a character vector
}
\description{
  The parsed information is converted to a vector of
  roxygen tags.
}
\examples{
rd.file = system.file("examples", "parse_and_save.Rd", package = "Rd2roxygen")
options(roxygen.comment = "##' ")
create_roxygen(parse_file(rd.file))
}
\author{
  Hadley Wickham; modified by Yihui Xie
  <\url{http://yihui.name}>
}
