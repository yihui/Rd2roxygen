#!/usr/bin/env Rscript

## automatic bib generation
library(tweakr)
write_citation(c('Rd2roxygen', 'roxygen2', 'formatR', 'knitr'), file = 'Rd2roxygen.bib')

## deal with LyX filename mangling
if (file.exists('Rd2roxygen.tex')) {
    x = readLines('Rd2roxygen.tex')
    idx = grep('\\\\documentclass', x)
    if (idx > 1) x = x[-(1:(idx-1))]
    idx = grep('\\\\bibliography|\\\\includegraphics', x)
    x[idx] = sub('\\{.*Rd2roxygen_vignettes_', '{', x[idx])
    writeLines(x, 'Rd2roxygen.tex')
    file.rename('Rd2roxygen.tex', 'Rd2roxygen.Rnw')
}
unlink(sprintf('Rd2roxygen.%s', c('aux', 'log', 'out', 'pdf')))
## now we can cheat Sweave :-)
