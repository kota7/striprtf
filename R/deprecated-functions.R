
#' Renamed Functions
#' @description From ver 0.3.1, the functions are renamed as follows:
#' \itemize{
#' \item{\code{striprtf} --> \code{\link{read_rtf}}}
#' \item{\code{rtf2text} --> \code{\link{strip_rtf}}}
#' }
#' @name striprtf-deprecated
NULL


#' @rdname striprtf-deprecated
#' @export
#' @param file  Path to an RTF file. Must be character of length 1.
#' @param verbose Logical. If TRUE, progress report is printed on console.
#' While it can be informative when parsing a large file,
#' this option itself makes the process slow.
#' @param ... Addional arguments passed to \code{\link{readLines}}
#' @return Character vector of extracted text
striprtf <- function(file, verbose = FALSE, ...)
{
  .Deprecated("read_rtf", "striprtf")
  read_rtf(file, verbose, ...)
}


#' @rdname striprtf-deprecated
#' @export
#' @param text  Character of length 1.  Expected to be contents of an RTF file.
rtf2text <- function(text, verbose = FALSE)
{
  .Deprecated("strip_rtf", "striprtf")
  strip_rtf(text, verbose)
}
