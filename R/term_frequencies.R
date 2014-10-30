#' Get word frequency from chunk of text
#' 
#' This takes a set of text items and returns a \code{data.frame} from
#'  it with important words in reverse importance of frequency.
#' @param text The chunk of text to use
#' @param show_cloud An optional term to plot as a wordcloud if necessary
#' @return A \code{data.frame} of word frequencies in the chunk
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
term_frequency <- function( text, show_cloud=FALSE ) {
  tdm <- make_clean_tdm( text )
  m <- as.matrix( tdm )
  p <- sort(rowSums(m), decreasing=T)
  df <- data.frame( words=names(p), freq=p)
  
  if( show_cloud ) {
    pal <- brewer.pal(8, "Dark2")
    wordcloud(df$word, df$freq, colors = pal, scale = c(6, 0.2), rot.per = 0.4, random.order = TRUE)
  }
  
  
  return( df )
}
