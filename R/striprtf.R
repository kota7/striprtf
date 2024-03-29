
#' Extract Text from RTF (Rich Text Format) File
#' @description Parses an RTF file and extracts plain text as character vector.
#' @param file  Path to an RTF file. Must be character of length 1.
#' @param verbose Logical. If TRUE, progress report is printed on console.
#' While it can be informative when parsing a large file,
#' this option itself makes the process slow.
#' @param row_start,row_end strings to be added at the beginning and end of table rows
#' @param cell_end string to be put at the end of table cells
#' @param ignore_tables if \code{TRUE}, no special treatment for tables
#' @param check_file if \code{TRUE}, conducts a quick check on the file if it is an RTF file.
#' If the file fails to pass the check, returns NULL without parsing the file.
#' @param ... Addional arguments passed to \code{\link{readLines}}
#' @return Character vector of extracted text
#' @export
#' @examples read_rtf(system.file("extdata/king.rtf", package = "striprtf"))
#' @references
#' \itemize{
#  \item{Python 3 implementation by Gilson Filho: \url{https://gist.github.com/gilsondev/7c1d2d753ddb522e7bc22511cfb08676}}
#'  \item{Original discussion thread: \url{https://stackoverflow.com/a/188877}}
#'  \item{Code page table: \url{https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/}}
#' }
#' @details Rich text format (RTF) files are written as a text file
#' consisting of ASCII characters.  The specification has been developed by
#' Microsoft.
#' This function interprets the character strings and extracts plain texts of
#' the file.
#' Major part of the algorithm of this function comes from a
#' stack overflow thread (\url{https://stackoverflow.com/a/188877}) and
#' the references therein.
#' This function is a translation of the above to R language, associated with C++ codes for enhancement.
#'
#' An advance from the preceding implementation is that the function
#' accomodates with various ANSI code pages.  For example, RTF files created
#' by Japanese version of Microsoft Word marks \code{\\ansicpg932}, which indicates
#' the code page 932 is used for letter-code conversion.
#' The function detects the code page indication and
#' convert the characters to UTF-8 where possible.  Conversion tables are retrieved from
#' here: (\url{https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/}).
#'
read_rtf <- function(file, verbose = FALSE,
                     row_start = "*| ", row_end = "", cell_end = " | ", ignore_tables=FALSE,
                     check_file=TRUE,
                     ...)
{
  stopifnot(is.character(file))
  stopifnot(length(file) == 1L)

  if (check_file && !looks_rtf(file)) {
    message(sprintf("File '%s' does not seem to be an RTF file.", file))
    return(NULL)
  }

  readLines(file, warn = FALSE, ...) %>%
    paste0(collapse = "\n") %>%
    strip_rtf(verbose, row_start, row_end, cell_end, ignore_tables)
}










#' @rdname read_rtf
#' @param text  Character of length 1.  Expected to be contents of an RTF file.
#' @export
strip_rtf <- function(text, verbose = FALSE,
                      row_start = "*| ", row_end = "", cell_end = " | ", ignore_tables=FALSE)
{
  stopifnot(is.character(text))
  stopifnot(is.logical(verbose))
  stopifnot(length(verbose) > 0)
  if (length(verbose) > 1) {
    verbose <- verbose[1]
    warning("only first element of verbose is used")
  }

  # if text has more than 1 length, collapse
  if (length(text) > 1) text <- paste0(text, collapse = "\n")


  # obtain code page
  cp <- stringr::str_match(text, "\\\\ansicpg([0-9]+)")[,2]
  if (is.na(cp)) {
    cpname <- NA_character_
    code_before <- integer(0)
    code_after <- integer(0)
  } else {
    cpname <- paste("CP", cp, sep = "")
    code_before <- .cptable[[cpname]]$before
    code_after <- .cptable[[cpname]]$after
  }

  pattern <- stringr::regex(
    "\\\\([a-z]{1,32})(-?\\d{1,10})?[ ]?|\\\\'([0-9a-f]{2})|\\\\([^a-z])|([{}])|[\r\n]+|(.)",
    ignore_case = TRUE)

  match_mat <- stringr::str_match_all(text, pattern)[[1]]
  if (nrow(match_mat) == 0) return(character(0))
  # str_match_all now returns NAs for unmatched optional groups
  # I replace NAs with "" so that the c++ function works as before
  match_mat[is.na(match_mat)] <- ""


  tmp_rep <- unused_letters(c(text, row_start, row_end, cell_end),
                            2, as_number=TRUE)
  tmp_rep_str <- intToUtf8(tmp_rep) %>% strsplit("") %>% unlist()
  keys   <- .specialchars$keys
  hexstr <- .specialchars$hexstr
  hexstr[keys=="row"]   <- sprintf("x%04d", tmp_rep[1])
  hexstr[keys=="cell"]  <- sprintf("x%04d", tmp_rep[2])



  # use c++ helper function to parse
  parsed <- strip_helper(match_mat,
                         dest_names = .destinations,
                         special_keys = keys,
                         special_hex  = hexstr,
                         code_before = code_before,
                         code_after = code_after,
                         verbose = verbose)
  #print(parsed)

  # convert integer codes to utf8
  out <- lapply(parsed$intcode, intToUtf8) %>% unlist()
  #print(out)
  # code page translation ... will be done in the strip_helper function
  # if (!is.na(cpname)) {
  #   if (cpname %in% names(.cptable)) {
  #     out[parsed$toconv] <- chartr(.cptable[[cpname]]$before,
  #                                  .cptable[[cpname]]$after,
  #                                  out[parsed$toconv])
  #     #out[parsed$toconv] <- lapply(out[parsed$toconv], function(a) {
  #     #  chartr(.cptable[[cpname]]$before, .cptable[[cpname]]$after, a)
  #     #})
  #   } else {
  #     warning("conversion table for ", cpname, " is missing")
  #   }
  # }

  # if there is no table or ignore table option is specified,
  # remove tmp_rep characters,
  # split by line breaks,
  # and return
  if (!any(parsed$table) || ignore_tables) {
    out <- out %>% paste0(collapse = "") %>%
      strsplit("\n") %>%
      unlist()
    regx <- sprintf("[%s]", paste0(tmp_rep_str, collapse = ""))
    out <- gsub(regx, "", out)
    return(out)
  }
  #print(out)

  # non-table sections are split by line breaks
  # table sections are split by \row
  # so we replace line breaks
  out[!parsed$table] <- stringr::str_replace_all(out[!parsed$table], "\n", tmp_rep_str[1])
  out <- strsplit(out, tmp_rep_str[1])
  # unlist, with table flag kept tracked
  len <- lapply(out, length) %>% unlist()
  out <- unlist(out)
  table_flg <- Map(rep, parsed$table, len) %>% unlist()
  #print(out)
  #print(table_flg)

  # remove empty table sections
  emp_tbl <- (nchar(out) == 0) & table_flg
  out <- out[!emp_tbl]
  table_flg <- table_flg[!emp_tbl]

  # cell separators are replaced
  out[table_flg] <- stringr::str_replace_all(out[table_flg], tmp_rep_str[2], cell_end)
  # row start and end indicators added
  out[table_flg] <- paste(row_start, out[table_flg], row_end, sep = "")


  out
}



