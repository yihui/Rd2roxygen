\name{rm_undocumented}
\alias{rm_undocumented}
\title{Remove Rd files for the undocumented functions.}
\usage{rm_undocumented(pkg, len=5)}
\description{Remove Rd files for the undocumented functions.
Usually roxygen will generate Rd files even for the undocumented functions, and
the length of such files will be less or equal to 5. This function removes all the
Rd files which are shorter than 5 lines under the 'man' directory.}
\value{\code{NULL} (if such Rd files exist, there will be messages printed in the
console showing which files are deleted)}
\author{Yihui Xie <\url{http://yihui.name}>}
\arguments{\item{pkg}{the directory of the source package}
\item{len}{the maximum number of lines of Rd files which are not
actually needed}}
