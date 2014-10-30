#' Makes a term document matrix out of a folder full of PDF's
#' 
#' This is more of a convienence function but allows you to quickly
#' make a term document matrix out of a folder of PDF's
#' @param path The path to the folder.  The PDF's must be labelled *.pdf.
#' @param ... Other parameters to be passed to \code{grab_text()} and \code{grab_chunk()} 
#'  functions.  See their descriptions to what they allow to be passed or what is 
#'  used as defaults.
#' @return A term document matrix.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
make_conceptual_tdm <- function( path, ... ){
  if(missing(path))
    stop("you need to supply a path for the make_tdm() function.")
  
  files <- list.files(path, pattern=".pdf")
  contents <- list()
  
  for( file in files ){
    
    raw_text <- grab_text( paste(path,file,sep="") )
    intro <- grab_chunk( raw_text,stop_text="Methods")
    
    contents[[file]] <- intro
    
  }
  
}


