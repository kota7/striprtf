# prepare for code point convert table
# save them as internal data
library(magrittr)

undefined <- 0x3f

file_list <- dir("data-raw", "\\.[Tt][Xx][Tt]$", full.names = TRUE)
out <- list()
for (file in file_list)
{
  table_name <- sub("\\..+$", "", basename(file))
  cat(table_name, "...\n")
  x <- read.table(file, sep = "\t")[1:2]
  names(x) <- c("before", "after")

  # impute `undefined` for missing cases
  x$after[is.na(x$after)] <- undefined

  # assert that there is no missings
  stopifnot(all(!is.na(x)))

  # we only need the cases where codes are different
  x <- x[x$before != x$after,]

  #table_list <- c(table_list, table_name)
  out <- c(out, setNames(list(x), table_name))
}

.cptable <- out
devtools::use_data(.cptable, internal = TRUE, overwrite = TRUE)
#cat(paste0(table_list, collapse = ", "))
#devtools::use_data(
#  CP1250, CP1251, CP1252, CP1253, CP1254,
#  CP1255, CP1256, CP1257, CP1258, CP874,
#  CP932, CP936, CP949, CP950,
#  internal = TRUE, overwrite = TRUE
#)
