
#' Renamed Functions
#' @description From ver 0.3.1, the functions are renamed as follows:
#' \itemize{
#' \item{\code{striprtf} --> \code{\link{read_rtf}}}
#' \item{\code{rtf2text} --> \code{\link{strip_rtf}}}
#' }
#' @rdname striprtf-deprecated
#' @export
striprtf <- function(file, verbose = FALSE, ...)
{
  .Deprecated("read_rtf", "striprtf")
  read_rtf(file, verbose, ...)
}


#' @rdname striprtf-deprecated
#' @export
rtf2text <- function(text, verbose = FALSE)
{
  .Deprecated("strip_rtf", "striprtf")
  strip_rtf(text, verbose)
}
