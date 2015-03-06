## ------------------------------------------------------------------------
#' @title zeropad, Pad a Number
#' @description Formats a character representation of a number and pads it with leading zeros to a specified number of characters. Copied from USGSwsBase package (which has since been replaced/renamed)

#' @param x a vector of numbers, to be formatted. Missing value are permitted and result in corresponding missing values in the output.
#' @param LEN the minimum number of characters in the output. If LEN is less than or equal to 1, then no padding is done. The default is 0.
#' @return An optionally padded character vector representing the data in x.
#' @export

zeropad <- function(x, LEN=0) {
  ## Coding history:
  ##    2002Dec04 DLLorenz Initial coding.
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.character(x)
  if(LEN <= 1) return(x)
  pattern <- paste(rep(0,LEN), collapse="")
  sapply(as.list(x), FUN=function(x, p, LEN) {
    nx <- nchar(x) - 1
    if(nx >= LEN) # Protect against x longer than LEN
      return(x)
    substring(p, LEN - nx, LEN) <- x
    return(p)}, p = pattern, LEN=LEN)
}


