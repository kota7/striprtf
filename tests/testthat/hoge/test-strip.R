library(testthat)
library(striprtf)

context("STRIP RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic", {
  x <- readLines("msword.rtf", warn = FALSE) %>% strip_rtf()
  check <- "This is a test file." %in% x
  expect_true(check)
})


test_that("big", {
  x <- readLines("big.rtf", warn = FALSE) %>% strip_rtf()
  check <- sum(x == "THIS IS A MUCH BIGGER FILE")
  expect_true(check == 360L)
})


test_that("libre", {
  x <- readLines("libre.rtf", warn = FALSE) %>% strip_rtf()
  ans <- readLines("libre.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("abiword", {
  x <- readLines("abiword.rtf", warn = FALSE) %>% strip_rtf()
  ans <- readLines("abiword.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Japanese (cp932)", {
  x <- readLines("cp932.rtf", warn = FALSE) %>% strip_rtf()
  ans <- readLines("cp932.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Chinese and Korean", {
  x <- readLines("eastasia.rtf", warn = FALSE) %>% strip_rtf()
  ans <- readLines("eastasia.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Europe", {
  x <- readLines("europe.rtf", warn = FALSE) %>% strip_rtf()
  ans <- readLines("europe.ans", encoding = "UTF-8")
  #TBA...
  #check <- lapply(ans, function(a) which(x == a))
  #len <- lapply(check, length) %>% unlist()
  #expect_true(all(len == 1L))                # inclusion check
  #expect_true(all(diff(unlist(check)) > 0))  # order check
})
