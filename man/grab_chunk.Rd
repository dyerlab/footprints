% Generated by roxygen2 (4.0.1): do not edit by hand
\name{grab_chunk}
\alias{grab_chunk}
\title{Grabs a chunk based upon one or two goalpoasts in the text}
\usage{
grab_chunk(full_text, start_text = NULL, stop_text = NULL,
  verbose = FALSE, ...)
}
\arguments{
\item{full_text}{A character array of text to search through.}

\item{start_text}{A word to mark the beginning of the chunk to be returned.  If this is \code{NULL} then
the returned text will start at the beginning of \code{full_text}.}

\item{stop_text}{A word to mark the end of the chunk to be returned.  If this is \code{NULL} then
the returned text will go to the end.}

\item{verbose}{Prints indices of start and stop for debugging (default is FALSE )}

\item{...}{Ignored}
}
\value{
Zero or more lines of text between \code{start_text} and \code{stop_text}.
}
\description{
This is a convienence function that takes a bunch of character strings and returns a subset of them
 corresponding to a \code{start_text} and \code{stop_text} term supplied by the user.
}

