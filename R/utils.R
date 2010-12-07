## extract tags
tag <- function(x) attr(x, "Rd_tag")

## replace tags
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
##' the \command{name} and \command{title} tags of such files will be the same.
##' This function removes all such Rd files under the 'man' directory.
##'
##' @param pkg the directory of the source package
##' @return \code{NULL} (if such Rd files exist, there will be messages printed in the
##' console showing which files are deleted)
##' @export
##' @author Yihui Xie <\url{http://yihui.name}>
rm_undocumented = function(pkg) {
    for (f in list.files(file.path(pkg, "man"), ".*\\.Rd$", all.files = TRUE,
                         full.names = TRUE)) {
        x = readLines(f)
        cond = identical(
            gsub('\\\\name\\{(.*)\\}', '\\1', grep('^\\\\name', x, value=TRUE)),
            gsub('\\\\title\\{(.*)\\}', '\\1', grep('^\\\\title', x, value=TRUE))
        )
        if (cond) {
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
##' @aliases roxygen_and_build rab
##' @param pkg the root directory of the source package
##' @param roxygen.dir the directory for the roxygenized package
##' @param install whether to install the package
##' @param check whether to check the package
##' @param check.opts options to check the package
##' (e.g. \code{"--no-examples"})
##' @param escape whether to escape \code{"\%"}
##' @param remove.check whether to remove the directory generated by
##' \command{R CMD check}
##' @param ... other arguments passed to
##' \code{\link[roxygen]{roxygenize}}
##' @note This function also tries to remove directories \file{pkg/inst/doc} and
##' \file{pkg/inst} if they are empty; this is due to the fact that roxygen
##' will generate these directories no matter if they are needed.
##'
##' This function also has a short name \code{rab} to avoid typing efforts.
##' @return NULL
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples \dontrun{
##' roxygen_and_build("./Rd2roxygen", install = TRUE)
##' ## or simply
##' rab('./Rd2roxygen', install = TRUE)
##' }
roxygen_and_build = function(pkg, roxygen.dir = NULL, install = FALSE,
    check = FALSE, check.opts = "", escape = TRUE, remove.check = TRUE, ...) {
    if (is.null(roxygen.dir)) roxygen.dir = file.path(dirname(pkg), paste(basename(pkg), '.roxygen', sep = ''))
    if (file.exists(roxygen.dir) && normalizePath(pkg) != normalizePath(roxygen.dir)) unlink(roxygen.dir, recursive = TRUE)
    roxygenize(pkg, roxygen.dir, ...)
    unlink(sprintf("%s/.git", roxygen.dir), recursive = TRUE)
    if (!length(list.files((inst.dir <- file.path(roxygen.dir, 'inst', 'doc')), recursive = TRUE)))
        unlink(inst.dir, recursive = TRUE)
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
    system(sprintf("R CMD build %s ", roxygen.dir))
    if (install)
        system(sprintf("R CMD INSTALL %s ", roxygen.dir))
    if (check) {
        system(sprintf("R CMD check %s %s", roxygen.dir, check.opts))
        if (remove.check) unlink(sprintf('%s.Rcheck', roxygen.dir), TRUE)
    }
    invisible(NULL)
}

##' @export
rab = roxygen_and_build


##' Format the code in the usage and examples sections.
##'
##' This function can polish the Rd files generated by roxygen mainly in two sections: usage and examples. By default, roxygen will omit the spaces and indent in the code, which makes the code (especially in the examples section) hard to read.
##' @param path the path of the Rd file
##' @param section the sections in the Rd file to (re)format
##' @param ... other arguments passed to \code{tidy.source}
##' @return NULL (the original Rd file gets updated)
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' rd.file = system.file('examples', 'reformat_code_demo.Rd', package = 'Rd2roxygen')
##' file.copy(rd.file, tempdir())
##' fmt.file = file.path(tempdir(), 'parse_and_save.Rd')
##' if (interactive()) file.show(fmt.file)
##' reformat_code(fmt.file)
##' if (interactive()) file.show(fmt.file)
reformat_code = function(path, section = c('examples', 'usage'), ...) {
    if (require('formatR')) {
        rd = tools::parse_Rd(path)
        flag = FALSE
        for (sec in section) {
            idx = which(sapply(rd, tag) == paste('\\', sec, sep = ''))
            if (length(idx)) {
                txt = rd[idx]
                class(txt) = 'Rd'
                txt = as.character(txt)
                txt = txt[-c(1, 2, length(txt))]
                txt = gsub('^[[:space:]]+', '', txt)
                txt = sub('^\\\\dontrun', 'tag_name_dontrun = function() ', txt)
                txt[txt == ''] = '\n'
                con = textConnection(paste(txt, collapse = ''))
                txt = tidy.source(con, output = FALSE, keep.blank.line = TRUE)
                txt = txt$text.tidy
                close(con)
                txt = paste(txt, rep(c('\n', ''), c(length(txt) - 1, 1)), sep = '', collapse = '')
                txt = gsub('tag_name_dontrun = function() {', '\\dontrun{', txt, fixed = TRUE)
                rd[idx] = paste('\\', sec, '{', txt, '}', sep = '')
                flag = TRUE
            } else message('section ', sec, ' not found in ', path)
        }
        if (flag) {
            class(rd) = 'Rd'
            cat(as.character(rd), file = path, sep = '')
        }
    } else {
        message('unable to tidy the code: the package "formatR" was not installed')
    }
}