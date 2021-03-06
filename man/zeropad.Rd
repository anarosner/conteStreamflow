% Generated by roxygen2: do not edit by hand
% Please edit documentation in r/from_other_packages.R
\name{zeropad}
\alias{zeropad}
\title{zeropad, Pad a Number}
\usage{
zeropad(x, LEN = 0)
}
\arguments{
\item{x}{a vector of numbers, to be formatted. Missing value are permitted and result in corresponding missing values in the output.}

\item{LEN}{the minimum number of characters in the output. If LEN is less than or equal to 1, then no padding is done. The default is 0.}
}
\value{
An optionally padded character vector representing the data in x.
}
\description{
Formats a character representation of a number and pads it with leading zeros to a specified number of characters. Copied from USGSwsBase package (which has since been replaced/renamed)
}
