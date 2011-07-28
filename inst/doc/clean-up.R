#!/usr/bin/env Rscript

## deal with LyX filename mangling
x = readLines('Rd2roxygen.tex')
idx = grep('\\\\documentclass', x)
if (idx > 1) x = x[-(1:(idx-1))]
idx = grep('\\\\bibliography\\{', x)
x[idx] = sub('\\{.*Rd2roxygen_inst_doc_', '{', x[idx])
x = gsub('%\\\\selectlanguage\\{english\\}[%]*', '', x)
writeLines(x, 'Rd2roxygen.tex')
file.rename('Rd2roxygen.tex', 'Rd2roxygen.Rnw')
unlink(sprintf('Rd2roxygen.%s', c('aux', 'log', 'map', 'out', 'pdf')))
## now we can cheat Sweave :-)
