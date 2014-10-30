#' Find location of particular citation in text
#' 
#' This is a handy function to find out where a citation is
#'  located in a manuscript.
#' @param full_text This is an array of text that is usually grabbed 
#'  from \code{grab_text()} function.
#' @param key The sequence you are searching for in the \code{full_text}
#' @param breaks The sequence of section headers to partition the paper into.  By default, 
#'  the text is broken into "Introduction", "Methods", "Results", and "Discussion"
#' @return A list with the number of times the \code{key} was found in each section (defined
#'  by \code{breaks}) in \code{full_text}.
#' @note This function will allow you to specify the partitions of the manuscript in which 
#'  to look for a particular citation (or phrase).  It should be noted that you need to 
#'  try alternate citation types (e.g., Smith (1997), (Smith 1997), Smith .... (1997)), this
#'  function isn't that cleaver...
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
citation_location <- function( full_text, key, breaks=c("Introduction","Methods","Results","Discussion") ) {
  if( missing(full_text) || missing(key) )
    stop("You need both the 'full_text' and the 'key' to use the function 'citation_location()'.")
  
  ret <- as.list(rep(0,length(breaks)))
  names(ret) <- breaks
  
  
  return(ret)
  
}

