## extract tags
tag <- function(x) attr(x, "Rd_tag")

##' replace tags
untag <- function(x) {
  if (is.null(x)) return(NULL)
  attr(x, "Rd_tag") <- "TEXT"
  x
}

## construct strings from rd
reconstruct <- function(rd) {
  if (is.null(rd)) return()

  if (is.list(rd)) {
	if (length(tag(rd)) && tag(rd) %in% c('\\item', '\\tabular', '\\eqn', '\\deqn', '\\link')){
		if (tag(rd) == '\\link')
			return(paste('\\link', sprintf('[%s]', attr(rd, 'Rd_option')), '{', rd, '}', sep = ""))
		if (length(rd) == 2)
			return(paste(tag(rd), '{', rd[[1]], '}{',
				paste(sapply(rd[[2]], reconstruct), collapse = ""),
				'}', sep = "", collapse = "")) else if (length(rd) == 0) return(tag(rd))
	}
	special <- tag(rd) == toupper(tag(rd))
	singles <- tag(rd) %in% c('\\tab', '\\cr')
	prefix <- ifelse(special, "",
		paste(tag(rd), ifelse(singles, "", "{"), sep = ""))
	suffix <- ifelse(special, "", ifelse(singles, "", "}"))
	paste(prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix,
		 sep = "")
  } else {
    rd
  }
}

## wrap strings with comment prefix
comment_line <- function(x, exdent = 0) {
  if (missing(x)) return(comment_prefix())

  strwrap(x, width = 80, exdent = exdent, prefix = comment_prefix())
}

## add comments
comment_tag <- function(tag, value) {
  if (is.null(value) || value == "" || length(value) == 0) return()

  comment_line(paste(tag, value), exdent = 2)
}

## access the comment prefix
comment_prefix <- function() {
	if (is.null(getOption("roxygen.comment")))
				"#' " else getOption("roxygen.comment")
}


##' Remove Rd files for the undocumented functions.
##' Usually roxygen will generate Rd files even for the undocumented functions, and
##' the length of such files will be less or equal to 5. This function removes all the
##' Rd files which are shorter than 5 lines under the 'man' directory.
##'
##' @param pkg the directory of the source package
##' @param len the maximum number of lines of Rd files which are not
##' actually needed
##' @return \code{NULL} (if such Rd files exist, there will be messages printed in the
##' console showing which files are deleted)
##' @export
##' @author Yihui Xie <\url{http://yihui.name}>
rm_undocumented = function(pkg, len = 5) {
    for (f in list.files(file.path(pkg, "man"), ".*\\.Rd$", all.files = TRUE,
        full.names = TRUE)) {
        if (length(readLines(f)) <= len) {
            unlink(f)
            message("deleted: ", f)
            flush.console()
        }
    }
}

##' Roxygenize a package, clean up and build/check the package.
##' After the source package is roxygenized, this function first removes the
##' unnecessary Rd files, replaces `\%' with `\\\%', and build the package. Optionally
##' it also installs or checks the package.
##'
##' @param pkg the root directory of the source package
##' @param roxygen.dir the directory for the roxygenized package
##' @param install whether to install the package
##' @param check whether to check the package
##' @param check.opts options to check the package (e.g. \code{"--no-examples"})
##' @param escape whether to escape \code{"\%"}
##' @param ... other arguments passed to \code{\link[roxygen]{roxygenize}}
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples \dontrun{
##' roxygen_and_build("./Rd2roxygen", install = TRUE)
##' }
roxygen_and_build = function(pkg, roxygen.dir = NULL, install = FALSE,
    check = FALSE, check.opts = "", escape = TRUE, ...) {
    if (is.null(roxygen.dir)) roxygen.dir = paste(basename(pkg), '.roxygen', sep = '')
    roxygenize(pkg, roxygen.dir, ...)
    unlink(sprintf("%s/.git", roxygen.dir), recursive = TRUE)
    if (!length(list.files((inst.dir <- file.path(roxygen.dir, 'inst')), recursive = TRUE)))
        unlink(inst.dir, recursive = TRUE)
    rm_undocumented(roxygen.dir)
    if (escape) {
        for (f in list.files(file.path(roxygen.dir, "man"), ".*\\.Rd$", all.files = TRUE, full.names = TRUE)) {
            x = readLines(f)
            if (length(grep("(^|[^\\])%", x))) {
                x = gsub("(^|[^\\])%", "\\1\\\\%", x)
                writeLines(x, con = f)
                message("updated % --> \\%: ", f)
                flush.console()
            }
        }
    }
    system(sprintf("R CMD build %s.roxygen", pkg))
    if (install)
        system(sprintf("R CMD INSTALL %s.roxygen", pkg))
    if (check)
        system(sprintf("R CMD check %s.roxygen %s", pkg, check.opts))
    invisible(NULL)
}
