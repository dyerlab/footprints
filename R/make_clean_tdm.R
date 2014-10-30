#' Creates a clean term document matrix
#' 
#' This function seeks to create a cleaned up
#' term document matrix from raw text.
#' @param text A vector of character values for raw text.
#' @return A cleaned up Term Document Matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
make_clean_tdm <- function( text ){
  
  removeMyWords <- function(x) {
    ret <- removeWords(x, stopwords("english"))
    return(ret)
  }
  
  txt <- ifelse( length(text)>1, paste(text,collapse=" "), text )
  
  # remove split words
  txt <- gsub("- ", "", txt, fixed=T)
  txt <- gsub(" ion ", "ion ", txt, fixed=T)
  
  corpus <- Corpus( VectorSource( txt ) )
  corpus <- tm_map( corpus, removePunctuation )
  corpus <- tm_map( corpus, tolower )
  corpus <- tm_map( corpus, removeNumbers )
  corpus <- tm_map( corpus, stripWhitespace )
  corpus <- tm_map( corpus, function(x) { removeWords(x, stopwords("english")) } )
  tdm <- TermDocumentMatrix( corpus )
  
  return( tdm )
}

