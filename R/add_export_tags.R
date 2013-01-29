#' Add @export to exported functions as discovered from NAMESPACE
#' 
#' Calling this function will add an \code{@export} tag to any functions
#' found as exports in \code{NAMESPACE}, to the functions defined in the
#' \code{R} source directory.
#' @author Kevin Ushey
add_export_tags <- function() {
  
  NAMESPACE <- tryCatch( readLines("NAMESPACE"),
                         error=function(e) {
                           stop("could not read NAMESPACE")
                         })
  
  exported_fns <- grep( "^export\\(", NAMESPACE, value=TRUE )
  exported_fn_names <- gsub( "export\\((.*)\\)", "\\1", exported_fns )
  fn_match_re <- paste("^", exported_fn_names, " ?<- ?", sep="")
  
  R_files <- grep( "\\.[rR]$", list.files("./R", full.names=TRUE), value=TRUE )
  
  for( file in R_files ) {
    doc <- readLines( file )
    for( i in seq_along(doc) ) {
      if( any( sapply( fn_match_re, function(x) {
        length( grep( x, doc[i] ) ) > 0
      } ) ) ) {
        doc[i] <- paste( comment_tag("@export"), doc[i], sep="\n" )
      }
    }
    writeLines( doc, file )
  }
  
}
