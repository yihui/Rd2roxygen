#!/usr/bin/env Rscript

## deal with LyX filename mangling
x = readLines('Rd2roxygen.tex')
idx = grep('\\\\documentclass', x)
if (idx > 1) x = x[-(1:(idx-1))]
idx = grep('\\\\bibliography\\{', x)
x[idx] = sub('\\{.*Rd2roxygen_inst_doc_', '{', x[idx])
writeLines(x, 'Rd2roxygen.tex')
file.rename('Rd2roxygen.tex', 'Rd2roxygen.Rnw')
## now we can cheat Sweave :-)
