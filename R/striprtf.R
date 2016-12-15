#' Extract Text from RTF (Rich Text Format) File
#' @param file  Path to an RTF file. Must be character of length 1.
#' @param verbose Logical. If TRUE, progress report is shown.
#' @param ... Addional arguments passed to \code{\link{readLines}}
#' @return Character vector of extracted text
#' @export
striprtf <- function(file, verbose = FALSE, ...)
{
  stopifnot(is.character(file))
  stopifnot(length(file) == 1L)

  readLines(file, warn = FALSE, ...) %>%
    paste0(collapse = "\n") %>%
    rtf2text(verbose)
}







#
# rtf2text_R <- function(text, quiet = TRUE, verbose = FALSE)
# {
#   ### old version ###
#
#   # obtain code page
#   cp <- stringr::str_match(text, "\\\\ansicpg([0-9]+)")[,2]
#   if (is.na(cp)) {
#     cpname <- NA_character_
#   } else {
#     cpname <- paste("CP", cp, sep = "")
#   }
#
#   pattern <- stringr::regex("\\\\([a-z]{1,32})(-?\\d{1,10})?[ ]?|\\\\'([0-9a-f]{2})|\\\\([^a-z])|([{}])|[\r\n]+|(.)",
#                    ignore_case = TRUE)
#   destinations <- c(
#     'aftncn', 'aftnsep', 'aftnsepc', 'annotation',
#     'atnauthor', 'atndate', 'atnicn', 'atnid',
#     'atnparent', 'atnref', 'atntime', 'atrfend',
#     'atrfstart', 'author', 'background',
#     'bkmkend', 'bkmkstart', 'blipuid', 'buptim',
#     'category', 'colorschememapping',
#     'colortbl', 'comment', 'company', 'creatim',
#     'datafield', 'datastore', 'defchp', 'defpap',
#     'do', 'doccomm', 'docvar', 'dptxbxtext', 'ebcend',
#     'ebcstart', 'factoidname', 'falt',
#     'fchars', 'ffdeftext', 'ffentrymcr', 'ffexitmcr',
#     'ffformat', 'ffhelptext', 'ffl',
#     'ffname', 'ffstattext', 'field', 'file', 'filetbl',
#     'fldinst', 'fldrslt', 'fldtype',
#     'fname', 'fontemb', 'fontfile', 'fonttbl',
#     'footer', 'footerf', 'footerl', 'footerr',
#     'footnote', 'formfield', 'ftncn', 'ftnsep',
#     'ftnsepc', 'g', 'generator', 'gridtbl',
#     'header', 'headerf', 'headerl', 'headerr', 'hl',
#     'hlfr', 'hlinkbase', 'hlloc', 'hlsrc',
#     'hsv', 'htmltag', 'info', 'keycode', 'keywords',
#     'latentstyles', 'lchars', 'levelnumbers',
#     'leveltext', 'lfolevel', 'linkval', 'list', 'listlevel',
#     'listname', 'listoverride',
#     'listoverridetable', 'listpicture', 'liststylename',
#     'listtable', 'listtext',
#     'lsdlockedexcept', 'macc', 'maccPr', 'mailmerge', 'maln',
#     'malnScr', 'manager', 'margPr',
#     'mbar', 'mbarPr', 'mbaseJc', 'mbegChr', 'mborderBox',
#     'mborderBoxPr', 'mbox', 'mboxPr',
#     'mchr', 'mcount', 'mctrlPr', 'md', 'mdeg', 'mdegHide',
#     'mden', 'mdiff', 'mdPr', 'me',
#     'mendChr', 'meqArr', 'meqArrPr', 'mf', 'mfName', 'mfPr',
#     'mfunc', 'mfuncPr', 'mgroupChr',
#     'mgroupChrPr', 'mgrow', 'mhideBot', 'mhideLeft', 'mhideRight',
#     'mhideTop', 'mhtmltag',
#     'mlim', 'mlimloc', 'mlimlow', 'mlimlowPr', 'mlimupp',
#     'mlimuppPr', 'mm', 'mmaddfieldname',
#     'mmath', 'mmathPict', 'mmathPr', 'mmaxdist', 'mmc', 'mmcJc',
#     'mmconnectstr', 'mmconnectstrdata', 'mmcPr', 'mmcs',
#     'mmdatasource', 'mmheadersource', 'mmmailsubject',
#     'mmodso', 'mmodsofilter', 'mmodsofldmpdata', 'mmodsomappedname',
#     'mmodsoname', 'mmodsorecipdata', 'mmodsosort',
#     'mmodsosrc', 'mmodsotable', 'mmodsoudl',
#     'mmodsoudldata', 'mmodsouniquetag', 'mmPr',
#     'mmquery', 'mmr', 'mnary', 'mnaryPr',
#     'mnoBreak', 'mnum', 'mobjDist', 'moMath', 'moMathPara',
#     'moMathParaPr', 'mopEmu',
#     'mphant', 'mphantPr', 'mplcHide', 'mpos', 'mr', 'mrad',
#     'mradPr', 'mrPr', 'msepChr',
#     'mshow', 'mshp', 'msPre', 'msPrePr', 'msSub', 'msSubPr',
#     'msSubSup', 'msSubSupPr', 'msSup',
#     'msSupPr', 'mstrikeBLTR', 'mstrikeH', 'mstrikeTLBR', 'mstrikeV',
#     'msub', 'msubHide', 'msup', 'msupHide', 'mtransp', 'mtype',
#     'mvertJc', 'mvfmf', 'mvfml', 'mvtof', 'mvtol',
#     'mzeroAsc', 'mzeroDesc', 'mzeroWid', 'nesttableprops',
#     'nextfile', 'nonesttables',
#     'objalias', 'objclass', 'objdata', 'object', 'objname',
#     'objsect', 'objtime', 'oldcprops',
#     'oldpprops', 'oldsprops', 'oldtprops', 'oleclsid', 'operator',
#     'panose', 'password',
#     'passwordhash', 'pgp', 'pgptbl', 'picprop', 'pict', 'pn', 'pnseclvl',
#     'pntext', 'pntxta',
#     'pntxtb', 'printim', 'private', 'propname', 'protend', 'protstart',
#     'protusertbl', 'pxe',
#     'result', 'revtbl', 'revtim', 'rsidtbl', 'rxe', 'shp', 'shpgrp', 'shpinst',
#     'shppict', 'shprslt', 'shptxt', 'sn',
#     'sp', 'staticval', 'stylesheet', 'subject', 'sv',
#     'svb', 'tc', 'template', 'themedata', 'title', 'txe', 'ud',
#     'upr', 'userprops',
#     'wgrffmtfilter', 'windowcaption', 'writereservation', 'writereservhash',
#     'xe', 'xform',
#     'xmlattrname', 'xmlattrvalue', 'xmlclose', 'xmlname', 'xmlnstbl',
#     'xmlopen'
#   )
#
#   specialchars <- c(
#     'par' = '\n',
#     'sect' = '\n\n',
#     'page' = '\n\n',
#     'line' = '\n',
#     'tab' = '\t',
#     'emdash' = '\u2014',
#     'endash' = '\u2013',
#     'emspace' = '\u2003',
#     'enspace' = '\u2002',
#     'qmspace' = '\u2005',
#     'bullet' = '\u2022',
#     'lquote' = '\u2018',
#     'rquote' = '\u2019',
#     'ldblquote' = '\201C',
#     'rdblquote' = '\u201D'
#   )
#
#
#
#   stack     <- list() # keep previous state (used when entering new brace)
#   ignorable <- FALSE  # Whether this group (and all inside it) are "ignorable".
#   ucskip    <- 1L     # N of ASCII characters to skip after a unicode character.
#   curskip   <- 0L     # N of ASCII characters left to skip
#   curhex    <- ""     # current consecutive hex expression
#
#   out <- character(0)  # Output buffer.
#   match_list <- stringr::str_match_all(text, pattern)[[1]]
#   if (nrow(match_list) == 0) return(out)
#
#   for (i in 1:nrow(match_list))
#   {
#     if (!quiet) {
#       cat(sprintf('\r[%-20s%3.0f%%]',
#                   paste(rep('=', i/nrow(match_list)*20), collapse=''),
#                   i/nrow(match_list)*100))
#       flush.console()
#     }
#
#     m <- match_list[i,]
#     word  <- m[2]
#     arg   <- m[3]
#     hex   <- m[4]
#     char  <- m[5]
#     brace <- m[6]
#     tchar <- m[7]
#
#     # if consecutive hex is done, add the string to out
#     if (nchar(curhex) > 0 && (hex == "" || nchar(curhex) == 4L)) {
#       n <- as.hexmode(curhex) %>% as.integer()
#       #cat(curhex, " -> ", n, "or", sprintf("%x", n), "\n")
#       out <- c(out, intToUtf8(n))
#       curhex <- ""
#     }
#
#     if (brace != "") {
#       curskip = 0L
#       if (brace == '{') {
#         # Push state
#         stack <- c(stack, list(list(ucskip, ignorable)))
#       } else if (brace == '}') {
#         # Pop state
#         tmp <- stack[[length(stack)]]
#         ucskip <- tmp[[1]]
#         ignorable <- tmp[[2]]
#         stack <- stack[-length(stack)]
#       }
#     } else if (char != "") {
#       curskip <- 0L
#       if (char == "~") {
#         if (!ignorable) out <- c(out, "\ua0")
#       } else if (char %in% c("{", "}", "\\")) {
#         if (!ignorable) out <- c(out, char)
#       } else if (char == "*") {
#         ignorable <- TRUE
#       }
#     } else if (word != "") {
#       curskip <- 0L
#       if (word %in% destinations) {
#         ignorable <- TRUE
#       } else if (ignorable) {
#         next
#       } else if (word %in% names(specialchars)) {
#         out <- c(out, specialchars[word])
#       } else if (word == "uc") {
#         ucskip <- as.integer(arg)
#       } else if (word == "u") {
#         n <- as.integer(arg)
#         #cat("u", n, "\n")
#         if (n < 0) n <- n + 0x10000
#         if (n > 127) {
#           out <- c(out, intToUtf8(n))
#         } else {
#           out <- c(out, intToUtf8(n))
#         }
#         curskip <- ucskip
#       }
#     } else if (hex != "") {
#       if (curskip > 0) {
#         curskip <- curskip - 1
#       } else if (!ignorable) {
#         curhex <- paste(curhex, hex, sep = "")
#         # n <- as.hexmode(hex) %>% as.integer()
#         # cat("hex", n, "or", sprintf("%x", n), "\n")
#         #hexvec <- c(hexvec, n)
#         # if (n > 127) {
#         #   out <- c(out, intToUtf8(n))
#         # } else {
#         #   out <- c(out, intToUtf8(n))
#         # }
#       }
#     } else if (tchar != "") {
#       if (curskip > 0) {
#         curskip <- curskip - 1
#       } else if (!ignorable) {
#         #cat("tchar", tchar, "\n")
#         out <- c(out, tchar)
#       }
#     }
#   }
#   if (!quiet) cat("\n")
#   out <- paste0(out, collapse = "")
#
#   # code page translation
#   if (!is.na(cpname)) {
#     if (cpname %in% names(.cptable)) {
#       out <- chartr(.cptable[[cpname]]$before, .cptable[[cpname]]$after, out)
#     } else {
#       warning("conversion table for ", cpname, " is missing")
#     }
#   }
#
#
#   # return by splitting by line breaks
#   strsplit(out, "\n") %>% unlist()
#
# }




rtf2text_C <- function(text, verbose = FALSE)
{
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

  # use c++ helper function to parse
  parsed <- strip_helper(match_mat,
                         dest_names = .destinations,
                         special_keys = .specialchars$keys,
                         special_hex  = .specialchars$hexstr)
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



#' @rdname striprtf
#' @param text  Character of length 1.  Expected to be contents of an RTF file.
rtf2text <- rtf2text_C

