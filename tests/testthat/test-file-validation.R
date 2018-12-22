library(testthat)
library(striprtf)

context("VALIDATION")

# for debugging
#setwd("tests/testthat/")


test_that("RTF test files pass the test", {
  files <- dir(pattern="\\.rtf$", full.names=TRUE)
  #print(files)
  for (f in files) {
    expect_true(looks_rtf(f), info=sprintf("'%s' does not look RTF", f))
  }
})


test_that("Non-RTF files do not pass the test", {
  files <- dir(full.names=TRUE)
  files <- files[!grepl(pattern="\\.rtf$", files)]
  #print(files)
  for (f in files) {
    expect_false(looks_rtf(f), info=sprintf("'%s' looks RTF", f))
  }
})


test_that("read_rtf returns NULL for non-RTF file", {
  files <- dir(full.names=TRUE)
  files <- files[!grepl(pattern="\\.rtf$", files)]
  #print(files)
  for (f in files) {
    expect_null(read_rtf(f, check_file=TRUE),
                info=sprintf("read_rtf('%s') should return NULL", f))
  }
})


