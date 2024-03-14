\name{testy}
\alias{testy}
\alias{testy.default}
\alias{testy.factor}
\alias{testy.numeric}
\title{Test All The Things!}
\usage{
  testy(x, \dots)
  \method{testy}{default}(x, \dots, notused)
  \S3method{testy}{numeric}(x, \dots, notused)
  \S3method{testy}{factor}(x, \dots, notused)
}
\arguments{
  \item{notused}{A parameter that isn't used at all!}
  \item{x}{A parameter that does something, but I don't remember.}
  \item{\dots}{
      Dots all the things!
  }
}
\description{
  This document tests the features of \pkg{Rd2roxygen}.
  Eeyore is my spirit animal.
}
\section{Zero Argument Macros}{
  There is a \\cr in the \cr middle of this sentence!
  I use \\dots (\dots) in code and \\ldots (\ldots) in text. But I always love \R!
}
\section{Multi Argument Macros}{
    This is a \\href\{\}\{\} \href{https://4the.lulz}{link} to a site that doesn't exist.
    This is the \\url\{\} \url{https://4the.lulz} to that same site.

    \subsection{The \\tabular command}{
        The \\tabular\{\}\{\} command takes two arguments. I can \\describe\{\} the arguments

        \describe{
          \item{one}{the column alignment}
          \item{two}{an arbitrary number of lines separated by \\cr, and with fields separated by \\tab}
        }

        Or I can \\enumerate\{\} them.
        \enumerate{
          \item one: the column alignment
          \item two: an arbitrary number of lines separated by \\cr, and with fields separated by \\tab
        }

        Here's a table.

        \tabular{rlll}{
          [,1] \tab Ozone   \tab numeric \tab Ozone (ppb)\cr
          [,2] \tab Solar.R \tab numeric \tab Solar R (lang)\cr
          [,3] \tab Wind    \tab numeric \tab Wind (mph)\cr
          [,4] \tab Temp    \tab numeric \tab Temperature (degrees F)\cr
          [,5] \tab Month   \tab numeric \tab Month (1--12)\cr
          [,6] \tab Day     \tab numeric \tab Day of month (1--31)
        }
    }
}
\section{Not llamas}{
    This section is not about llamas. It is boring.
}
\examples{
  # from tools:::as.character.Rd
  # ZEROARG = c("\\cr", "\\dots", "\\ldots", "\\R", "\\tab")
  # MULTIARG = c("\\section", "\\subsection", "\\item", "\\enc",
  #     "\\method", "\\S3method", "\\S4method", "\\tabular",
  #     "\\if", "\\href", "\\ifelse")
  # USERMACROS = c("USERMACRO", "\\newcommand", "\\renewcommand")
  # EQN = c("\\deqn", "\\eqn", "\\figure")
}
