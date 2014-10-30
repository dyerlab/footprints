#' Grab raw text from pdf
#' 
#' This function returns the entire text from a pdf.
#' @param pdf The path to the pdf file
#' @param pdf2txt The shell command to run the python script pdf2txt.py (default)
#' @param remove_nulls A flag indicating that empty lines should be ignored (default = TRUE)
#' @param remove_singles A flag indicating that lines with only one character on them should be
#'  ignored (default=TRUE)
#' @param ... Ignored
#' @return The raw text from the file.
#' @note This function is the main function through which pdf text is to be filtered.  This funciton
#'  requires pdf2txt.py to be installed on the machine.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
grab_text <- function( pdf, pdf2txt="pdf2txt.py", remove_nulls=TRUE, remove_singles=TRUE, ... ) {
  if( missing(pdf))
    stop("Cannot run function 'grab_text' without a file to point towards...")
  
  cmd <- paste( pdf2txt, pdf, sep=" ")
  ret <- system( cmd, intern=T )
  
  if( remove_nulls )
    ret <- ret[ nchar(ret)>0 ]
  if( remove_singles )
    ret <- ret[ nchar(ret) != 1 ]
  
  
  return( ret )
}


