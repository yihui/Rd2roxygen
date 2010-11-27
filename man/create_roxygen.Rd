\name{create_roxygen}
\alias{create_roxygen}
\title{Create the roxygen documentation...}
\usage{create_roxygen(info, usage=FALSE)}
\description{Create the roxygen documentation}
\details{The parsed information is converted to a vector of roxygen tags.}
\value{a character vector}
\author{Hadley Wickham; modified by Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{info}{the named list of the parsed documentation}
\item{usage}{logical: whether to include the usage section in the output (this
can be useful when there are multiple functions in a single usage section, but
generally it is not necessary because roxygen can generate the usage section
automatically)}}
