#' Find letters not used in strings
#'
#' Returns letters not used in strings
#'
#' @param s character vector
#' @param n number of letters to return
#' @param avoid_strifrtf_internal If \code{TRUE}, letters used in the package's internal process are also regarded as "used".
#' @param as_number if \code{TRUE}, return unicode numbers instead of letters itself
#' @param as_vector if \code{FALSE} (and \code{as_number} is \code{FALSE}), return
#' a single concatenated character, otherwise returns a character vector
#' @details This function can be useful when some special characters must be
#' temporarily converted to another letter without being confused with the same letters used elsewhere.
#'
#' Letters are first searched from \code{\\u0001} upto \code{\\uffff}.
#' Do not specify too large \code{n}; An error is raised if a sufficient number of unsed letters are not found.
#'
#' @return unsed characters, format depends on \code{as_number} and \code{as_vector} arguments
#' @export
unused_letters <- function(s, n = 1, avoid_strifrtf_internal = TRUE,
                           as_number = FALSE, as_vector = FALSE)
{
  stopifnot(is.character(s))
  stopifnot(is.numeric(n))
  stopifnot(length(n) >= 1)
  n <- as.integer(n)
  if (length(n) > 1) {
    warning('n must be a scalar, only the first element is used')
    n <- n[1]
  }
  stopifnot(n >= 1)

  if (avoid_strifrtf_internal) s <- c(s, .specialchars$str)
  s <- paste0(s, collapse = "")

  used <- utf8ToInt(s) %>% unique()
  out <- integer(0)
  for (k in 1:4096)
  {
    candidates <- 16^(k-1):(16^k-1)

    tmp <- setdiff(candidates, used)
    if (length(tmp) >= n) {
      out <- c(out, utils::head(tmp, n))
      break
    } else {
      out <- c(out, tmp)
      n <- n - length(tmp)
    }
  }

  if (length(out) < n) stop("could not find ", n, " unused letters")

  if (as_number) return(out)
  if (!as_vector) return(intToUtf8(out))
  intToUtf8(out) %>% strsplit('') %>% unlist()
}
