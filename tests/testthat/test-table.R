library(testthat)
library(striprtf)

context("TABLE IN RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic table", {
  x <- read_rtf("table.rtf")
  #check <- "This is a test file." %in% x
  #expect_true(check)
})


