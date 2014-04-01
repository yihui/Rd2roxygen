\name{parse_file}
\alias{parse_file}
\title{Parse the input Rd file to a list}
\usage{
parse_file(path)
}
\arguments{
\item{path}{the path of the Rd file}
}
\value{
a named list containing the documentation sections as strings
}
\description{
This function uses the function \code{parse_Rd} in the \pkg{tools} package to
parse the Rd file.
}
\examples{
rd.file = system.file("examples", "parse_and_save.Rd", package = "Rd2roxygen")
parse_file(rd.file)
}
\author{
Hadley Wickham; modified by Yihui Xie <\url{http://yihui.name}>
}
