#' @importFrom magrittr %>%
NULL


#' @useDynLib striprtf
#' @importFrom Rcpp sourceCpp
NULL


#' Quick test whether a file is RTF
#'
#' Conducts a quick test whether a file looks RTF.
#'
#' @param con A connection object or string of file name
#' @param n Integer that specifies the length of contents to be tested.
#'          If smaller than 10, forced to 10.
#' @return Logical.
#' @export
looks_rtf <- function(con, n=1000) {
  if (n < 10) n <- 10

  x <- readBin(con, integer(), n=n, size=1)

  # RTF should contain only ascii characters (v <= 125)
  # If the file contains non-ascii data (i.e. > 125), then
  # return FALSE (not likely an RTF file)
  if (!all(x < 126 & x >= 0)) return(FALSE)

  # We require that the file should starts with "{\rtfN"
  # possibly with leading spaces
  check <- grep(" *\\{\\\\rtf", intToUtf8(x))
  if (length(check) != 1 || check != 1) return(FALSE)

  # Passed test we have prepared.
  # return TRUE (looks like an RTF)
  return(TRUE)
}
