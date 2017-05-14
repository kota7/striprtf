library(testthat)
library(striprtf)

context("TABLES")

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

  x <- read_rtf("table.rtf", ignore_tables=TRUE)
  expect_true(grepl("ABC1.012.023.03", x))
})


test_that("table with special chars", {
  x <- read_rtf("table-specialchars.rtf", row_start = "<tr>", row_end = "</tr>", cell_end = ",")

  i1 <- grep("<tr>a\tb\tc,d\ne\nf,h,i,j,</tr>", x, fixed=TRUE)
  i2 <- grep("<tr>k l m ,\u201cfoo\u201d,‘bar’,</tr>", x, fixed=TRUE)

  # both must appear exatly once
  expect_equal(length(i1), 1)
  expect_equal(length(i2), 1)
  # and they must be side by side
  expect_equal(i2-i1, 1)
})




test_that("multiple tables", {
  x <- read_rtf("table-multi.rtf", row_start="*|")

  i <- grep("*|", x, fixed=TRUE)
  expect_equal(length(i), 7)

  # there are two tables, one with 3 rows, the other with 4 rows
  # to check this, use diff(i) and count the number of diff=1 cases
  expect_equal(sum(diff(i)==1), 5)

  x <- read_rtf("table-multi.rtf", ignore_tables=TRUE)
  expect_true("123456" %in% x)
  expect_true("abcdefghijkl" %in% x)
})




test_that("big file with many tables", {
  x <- read_rtf("big-with-table.rtf", row_start="***")

  i <- grep("***", x, fixed=TRUE)
  expect_equal(length(i), 8)

  # there are two tables, one with 3 rows, the other with 4 rows
  # to check this, use diff(i) and count the number of diff=1 cases
  expect_equal(sum(diff(i)==1), 4)

  x <- read_rtf("big-with-table.rtf", ignore_tables=TRUE)
  i <- grep("abcdefghij", x)
  expect_equal(length(i), 4)
})

