##' extract tags
tag <- function(x) attr(x, "Rd_tag")

##' replace tags
untag <- function(x) {
  if (is.null(x)) return(NULL)
  attr(x, "Rd_tag") <- "TEXT"
  x
}

##' construct strings from rd
reconstruct <- function(rd) {
  if (is.null(rd)) return()

  if (is.list(rd)) {
    special <- tag(rd) == toupper(tag(rd))
    prefix <- ifelse(special, "", paste(tag(rd), "{", sep = ""))
    suffix <- ifelse(special, "", "}")

    paste(prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix,
     sep = "")
  } else {
    rd
  }
}

##' wrap strings with comment prefix
comment_line <- function(x, exdent = 0) {
  if (missing(x)) return(comment_prefix())

  strwrap(x, width = 80, exdent = exdent, prefix = comment_prefix())
}

##' add comments
comment_tag <- function(tag, value) {
  if (is.null(value) || value == "" || length(value) == 0) return()

  comment_line(paste(tag, value), exdent = 2)
}

##' access the comment prefix
comment_prefix <- function() {
	if (is.null(getOption("roxygen.comment")))
				"#' " else getOption("roxygen.comment")
}
