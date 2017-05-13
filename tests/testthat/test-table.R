library(testthat)
library(striprtf)

context("TABLE IN RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic table", {
  x <- read_rtf("table.rtf", row_start = "**", row_end = "**", cell_end = "|")
  i1 <- grep("**A|B|C|**", x, fixed=TRUE)
  i2 <- grep("**1.01|2.02|3.03|**", x, fixed=TRUE)

  # both must appear exatly once
  expect_equal(length(i1), 1)
  expect_equal(length(i2), 1)
  # and they must be side by side
  expect_equal(i2-i1, 1)
})


test_that("table with special chars", {
  x <- read_rtf("table-specialchars.rtf", row_start = "<tr>", row_end = "</tr>", cell_end = ",")

  i1 <- grep("<tr>a\tb\tc,d\ne\nf,h,i,j,</tr>", x, fixed=TRUE)
  i2 <- grep("<tr>k l m ,“foo”,‘bar’,</tr>" , x, fixed=TRUE)

  # both must appear exatly once
  expect_equal(length(i1), 1)
  expect_equal(length(i2), 1)
  # and they must be side by side
  expect_equal(i2-i1, 1)
})

