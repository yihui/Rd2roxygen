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
    if (length(tag(rd)) && tag(rd) %in% c('\\item', '\\tabular', '\\eqn', '\\deqn', '\\link')) {
      if (tag(rd) == '\\link')
        return(paste('\\link', sprintf('[%s]', attr(rd, 'Rd_option')), '{', rd, '}', sep = ""))
      if (length(rd) == 2) {
        return(paste(tag(rd), '{', rd[[1]], '}{',
                     paste(sapply(rd[[2]], reconstruct), collapse = ""),
                     '}', sep = "", collapse = ""))
      } else if (length(rd) == 0) return(tag(rd))
    }
    special <- tag(rd) == toupper(tag(rd))
    singles <- tag(rd) %in% c('\\tab', '\\cr')
    prefix <- ifelse(special, "",
                     paste(tag(rd), ifelse(singles, "", "{"), sep = ""))
    suffix <- ifelse(special, "", ifelse(singles, "", "}"))
    paste(prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix,
          sep = "")
  } else {
    if (tag(rd) == 'TEXT') gsub('%', '\\%', rd, fixed = TRUE) else rd
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

  comment_line(paste(tag, value), exdent = 0)
}

## access the comment prefix
comment_prefix <- function() {
  getOption("roxygen.comment", "#'")
}
