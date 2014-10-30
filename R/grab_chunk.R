#' Grabs a chunk based upon one or two goalpoasts in the text
#' 
#' This is a convienence function that takes a bunch of character strings and returns a subset of them
#'  corresponding to a \code{start_text} and \code{stop_text} term supplied by the user.
#' @param full_text A character array of text to search through.
#' @param start_text A word to mark the beginning of the chunk to be returned.  If this is \code{NULL} then 
#'  the returned text will start at the beginning of \code{full_text}.
#' @param stop_text A word to mark the end of the chunk to be returned.  If this is \code{NULL} then 
#'  the returned text will go to the end.
#' @param verbose Prints indices of start and stop for debugging (default is FALSE )
#' @param ... Ignored
#' @return Zero or more lines of text between \code{start_text} and \code{stop_text}.
#' @export
grab_chunk <- function( full_text, start_text=NULL, stop_text=NULL, verbose=FALSE, ... ){

  start_idx <- 1
  stop_idx <- length( full_text )
  s <- seq(1,length(full_text) )
  
  if( !is.null(start_text) ) 
    start_idx <- min( s[unlist(lapply( full_text, function(x) grepl(start_text,x)) )])
  
  if( !is.null(stop_text) )
    stop_idx <- max( s[unlist(lapply( full_text, function(x) grepl(stop_text,x)) )])
  
  if( verbose )
    cat(start_idx, "-", stop_idx,"\n")
  
  ret <- full_text[(start_idx+1):(stop_idx-1)]
  return( ret )

}


