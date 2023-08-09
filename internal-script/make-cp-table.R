# prepare for code point convert table
# save them as internal data
library(magrittr)
library(stringr)
undefined <- 0x3f   # undefined characters are converted to '?'



file_list <- dir("data-raw", "\\.[Tt][Xx][Tt]$", full.names = TRUE)
get_cp <- function(fp)
{
  # args:
  #   fp : file path
  #
  # returns: string like "CPXXX"
  if (grepl("^CP[0-9]+", basename(fp))) {
    sub("\\..+$", "", basename(file))
  } else {
    tmp <- scan(fp, what="character", sep="\n", fileEncoding="UTF-8")
    cp <- str_extract(tmp, "[cP][pP][0-9]+")
    cp <- cp[!is.na(cp)]
    stopifnot(length(cp) >= 1)
    cp <- cp[1]
    toupper(cp)
  }
}

robust_read <- function(fp)
{
  # read table file,
  # while avoiding warning
  # "number of items read is not a multiple of the number of columns"
  #
  # args:
  #   fp : file path
  #
  # returns: data frame with two columns

  tmp <- scan(fp, what="character", sep="\n",
              fileEncoding="UTF-8", comment.char="#")
  # number of tabs
  # keep only rows with 2 tabs
  ntabs <- nchar(gsub("[^\t]+", "", tmp))
  included <- (ntabs == 2L)
  cat(sum(!included), "rows are ignored\n")
  text <- tmp[included]

  # save to tmp file
  tmppath <- "tmpfile"
  write(text, tmppath)
  out <- read.table(tmppath, sep="\t")
  file.remove(tmppath)
  out[1:2]
}


out <- list()
for (file in file_list)
{
  # obtain table name, i.e., CPXXX
  table_name <- get_cp(file)
  cat(table_name, "...\n")
  x <- robust_read(file)
  names(x) <- c("before", "after")

  # impute `undefined` for missing cases
  x$after[is.na(x$after)] <- undefined

  # assert that there is no missings
  stopifnot(all(!is.na(x)))

  # we only need the cases where codes are different
  x <- x[x$before != x$after,]

  # create before and after integer values
  #bef <- intToUtf8(x$before) %>% paste0(collapse = "")
  #aft <- intToUtf8(x$after) %>% paste0(collapse = "")
  #stopifnot(nchar(bef) == nchar(aft))

  #table_list <- c(table_list, table_name)
  out <- c(out, list(x) %>% setNames(table_name))
}



.cptable <- out
usethis::use_data(.cptable, internal = TRUE, overwrite = TRUE)


#cat(paste0(table_list, collapse = ", "))
#devtools::use_data(
#  CP1250, CP1251, CP1252, CP1253, CP1254,
#  CP1255, CP1256, CP1257, CP1258, CP874,
#  CP932, CP936, CP949, CP950,
#  internal = TRUE, overwrite = TRUE
#)
