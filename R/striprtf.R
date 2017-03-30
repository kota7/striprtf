
#' Extract Text from RTF (Rich Text Format) File
#' @description Parses an RTF file and extracts plain text as character vector.
#' @param file  Path to an RTF file. Must be character of length 1.
#' @param verbose Logical. If TRUE, progress report is printed on console.
#' While it can be informative when parsing a large file,
#' this option itself makes the process slow.
#' @param ... Addional arguments passed to \code{\link{readLines}}
#' @return Character vector of extracted text
#' @export
#' @examples read_rtf(system.file("extdata/king.rtf", package = "striprtf"))
#' @references
#' \itemize{
#'  \item{Python 3 implementation by Gilson Filho: \url{https://gist.github.com/gilsondev/7c1d2d753ddb522e7bc22511cfb08676}}
#'  \item{Original discussion thread: \url{http://stackoverflow.com/a/188877}}
#'  \item{Code page table: \url{http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/}}
#' }
#' @details Rich text format (RTF) files are written as a text file
#' consisting of ASCII characters.  The specification has been developed by
#' Microsoft.
#' This function interprets the character strings and extracts plain texts of
#' the file.
#' Major part of the algorithm this function employs has been discussed in a
#' stack overflow thread (\url{http://stackoverflow.com/a/188877}) and later
#' refactored and implemented by Gilson Filho for python 3
#' (\url{https://gist.github.com/gilsondev/7c1d2d753ddb522e7bc22511cfb08676}).
#' The function is a translation of the above codes to R language,
#' associated with C++ codes for enhancement.
#'
#' An advance from the preceding implementation is that the function
#' accomodates with various ANSI code pages.  For example, RTF files created
#' by Japanese version of Microsoft Word marks \code{\\ansicpg932}, which indicates
#' the code page 932 is used for letter-code conversion.
#' The function detect the code page indication and
#' convert them to UTF-8 as possible.  Conversion table is retrieved from
#' here (\url{http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/}).
#'
read_rtf <- function(file, verbose = FALSE, ...)
{
  stopifnot(is.character(file))
  stopifnot(length(file) == 1L)

  readLines(file, warn = FALSE, ...) %>%
    paste0(collapse = "\n") %>%
    strip_rtf(verbose)
}










#' @rdname read_rtf
#' @param text  Character of length 1.  Expected to be contents of an RTF file.
#' @export
strip_rtf <- function(text, verbose = FALSE)
{
  stopifnot(is.character(text))
  stopifnot(is.logical(verbose))
  stopifnot(length(verbose) > 0)
  if (length(verbose) > 1) {
    verbose <- verbose[1]
    warning("only first element of verbose is used")
  }

  # if text has more than 1 length, collapse
  if (length(text) > 1) text <- paste0(text, collapse = "character")


  # obtain code page
  cp <- stringr::str_match(text, "\\\\ansicpg([0-9]+)")[,2]
  if (is.na(cp)) {
    cpname <- NA_character_
  } else {
    cpname <- paste("CP", cp, sep = "")
  }

  pattern <- stringr::regex(
    "\\\\([a-z]{1,32})(-?\\d{1,10})?[ ]?|\\\\'([0-9a-f]{2})|\\\\([^a-z])|([{}])|[\r\n]+|(.)",
    ignore_case = TRUE)

  match_mat <- stringr::str_match_all(text, pattern)[[1]]
  if (nrow(match_mat) == 0) return(character(0))
  # str_match_all now returns NAs for unmatched optional groups
  # I replace NAs with "" so that the c++ function works as before
  match_mat[is.na(match_mat)] <- ""

  # use c++ helper function to parse
  parsed <- strip_helper(match_mat,
                         dest_names = .destinations,
                         special_keys = .specialchars$keys,
                         special_hex  = .specialchars$hexstr,
                         verbose = verbose)
  #print(parsed)

  out <- strsplit(parsed$strcode, "x") %>%
    lapply(as.hexmode) %>%
    lapply(intToUtf8)
  #print(out)

  # code page translation
  if (!is.na(cpname)) {
    if (cpname %in% names(.cptable)) {
      out[parsed$toconv] <- lapply(out[parsed$toconv], function(a) {
        chartr(.cptable[[cpname]]$before, .cptable[[cpname]]$after, a)
      })
    } else {
      warning("conversion table for ", cpname, " is missing")
    }
  }

  # return by splitting by line breaks
  paste0(out, collapse = "") %>% strsplit("\n") %>% unlist()
}



