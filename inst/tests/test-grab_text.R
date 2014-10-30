context("grab_text.R")

test_that( "params", {
  pdffile <- system.file("extdata", "DyerNason2004.pdf", package="footprint")
  
  expect_that( nchar(pdffile)>1,  is_true() )
  ret <- grab_text(pdffile)
  expect_that( length(ret) > 0 , is_true() )
})

