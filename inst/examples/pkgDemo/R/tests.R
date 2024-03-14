testy <- function(x, ...) {
  UseMethod("testy")
}

testy.default <- function(x, ..., notused) {
  cat("hello world!\n")
}

testy.factor <- function(x, ..., notused) {
  cat("I like factors. Factors are my favorite!\n")
}

testy.numeric <- function(x, ..., notused) {
  cat("I like a number of things!\n")
}
