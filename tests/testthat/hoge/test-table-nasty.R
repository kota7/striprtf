library(striprtf)
library(testthat)

context("TABLE EDGE CASES with VARIOUS SOFTWARE")

# for debugging
#setwd("tests/testthat/")

test_that("suggested example", {
  x <- read_rtf("table-cell-outside.rtf", row_start = "** ", cell_end="\t")
  row_flg <- grepl("** ", x, fixed=TRUE)
  row_ct <- sum(row_flg)

  expect_equal(row_ct, 1L)
  expect_true(grepl("AAA \tBBB \tCCC \t", x[row_flg]))
})

test_that("various software", {
  # libre's RTF tends to add extra \cell, but allow this deviation
  y <- list(
    gdoc  = read_rtf("table-gdoc.rtf", row_start = "** ", cell_end="\t"),
    libre = read_rtf("table-libre.rtf", row_start = "** ", cell_end="\t"),
    word  = read_rtf("table-word.rtf", row_start = "** ", cell_end="\t")
  )
  for (i in seq_along(y))
  {
    x <- y[[i]]
    name <- names(y)[i]
    row_flg <- grepl("** ", x, fixed=TRUE)
    row_ct <- sum(row_flg)
    expect_equal(row_ct, 6L, info=name)
    expect_true(sum(grepl("A\tB\tC\t", x[row_flg]))==1L, info=name)
    expect_true(sum(grepl("D\tE\tF\t", x[row_flg]))==1L, info=name)
    expect_true(sum(grepl("G\tH\tI\t", x[row_flg]))==1L, info=name)
    expect_true(sum(grepl("J\tK\tL\t", x[row_flg]))==1L, info=name)

    expect_true(sum(grepl("Text", x[!row_flg]))==1L, info=name)
    expect_true(sum(grepl("Blah", x[!row_flg]))==3L, info=name)
    expect_true(sum(grepl("foo", x[!row_flg]))==1L, info=name)
    expect_true(sum(grepl("bar", x[!row_flg]))==1L, info=name)
  }
})


test_that("with line breaks", {
  # libre's RTF tends to add extra \cell, but allow this deviation
  y <- list(
    gdoc  = read_rtf("table-gdoc-lb.rtf", row_start = "** ", cell_end="\t"),
    libre = read_rtf("table-libre-lb.rtf", row_start = "** ", cell_end="\t"),
    word  = read_rtf("table-word-lb.rtf", row_start = "** ", cell_end="\t")
  )
  for (i in seq_along(y))
  {
    x <- y[[i]]
    name <- names(y)[i]
    row_flg <- grepl("** ", x, fixed=TRUE)
    row_ct <- sum(row_flg)
    expect_equal(row_ct, 4L, info=name)
    expect_true(sum(grepl("A\nB\nC\tD\nE\nF\tG\nH\nI\t", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("J\nK\nL\tM\nN\nO\tP\nQ\nR\t", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("S\nT\tU\nV", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("W\nX\tY\nZ", x[row_flg]))==1L,
                info=name)

    expect_true(sum(grepl("Blah", x[!row_flg]))==3L, info=name)
    expect_true(sum(grepl("Foo", x[!row_flg]))==1L, info=name)
    expect_true(sum(grepl("Bar", x[!row_flg]))==1L, info=name)
  }
})



test_that("with line breaks with no outside text", {
  # libre's RTF tends to add extra \cell, but allow this deviation
  y <- list(
    gdoc  = read_rtf("table-gdoc-lb2.rtf", row_start = "** ", cell_end="\t"),
    libre = read_rtf("table-libre-lb2.rtf", row_start = "** ", cell_end="\t"),
    word  = read_rtf("table-word-lb2.rtf", row_start = "** ", cell_end="\t")
  )
  for (i in seq_along(y))
  {
    x <- y[[i]]
    name <- names(y)[i]
    row_flg <- grepl("** ", x, fixed=TRUE)
    row_ct <- sum(row_flg)
    expect_equal(row_ct, 4L, info=name)
    expect_true(sum(grepl("A\nB\nC\tD\nE\nF\tG\nH\nI\t", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("J\nK\nL\tM\nN\nO\tP\nQ\nR\t", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("S\nT\tU\nV", x[row_flg]))==1L,
                info=name)
    expect_true(sum(grepl("W\nX\tY\nZ", x[row_flg]))==1L,
                info=name)
  }
})



test_that("with empty cells", {
  # libre's RTF tends to add extra \cell, but allow this deviation
  y <- list(
    gdoc  = read_rtf("table-gdoc-empty.rtf", row_start = "** ", cell_end="\t"),
    libre = read_rtf("table-libre-empty.rtf", row_start = "** ", cell_end="\t"),
    word  = read_rtf("table-word-empty.rtf", row_start = "** ", cell_end="\t")
  )
  for (i in seq_along(y))
  {
    x <- y[[i]]
    name <- names(y)[i]
    row_flg <- grepl("** ", x, fixed=TRUE)
    row_ct <- sum(row_flg)
    expect_equal(row_ct, 4L, info=name)
    expect_true(sum(grepl("A[\n]*\tB\nC\t\t", x[row_flg]))==1L, info=name)
    expect_true(sum(gsub("\t", "", x[row_flg]) == "** ")==2L, info=name)
    expect_true(sum(grepl("\t\t", x[row_flg]))==3L, info=name)
    expect_true(sum(grepl("\tE\nF\t", x[row_flg]))==1L, info=name)

    expect_true(sum(grepl("Blah", x[!row_flg]))==3L, info=name)
  }
})



