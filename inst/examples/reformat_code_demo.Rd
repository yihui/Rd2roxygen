\name{parse_and_save}
\alias{parse_and_save}
\title{Parse the input Rd file and save the roxygen documentation into a file.}
\usage{parse_and_save(path, file, usage=FALSE)}
\description{Parse the input Rd file and save the roxygen documentation into a file.}
\value{a character vector if \code{file} is not specified, or write the vector
into a file}
\author{Hadley Wickham; modified by Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{path}{the path of the Rd file}
\item{file}{the path to save the roxygen documentation}
\item{usage}{logical: whether to include the usage section in the output}}
\examples{
## the examples here are only for demo purpose

print("hello world")

## a loooong expression
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

## the dontrun command
\dontrun{
    parse_and_save("parse_and_save.Rd")
}

## not well-formatted code
for(i in 1:10){message('number ',i,
'?')}
}
