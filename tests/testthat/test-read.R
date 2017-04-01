library(testthat)
library(striprtf)

context("READ RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic", {
  x <- read_rtf("msword.rtf")
  check <- "This is a test file." %in% x
  expect_true(check)
})


test_that("big", {
  x <- read_rtf("big.rtf")
  check <- sum(x == "THIS IS A MUCH BIGGER FILE")
  expect_true(check == 360L)
})


test_that("libre", {
  x <- read_rtf("libre.rtf")
  ans <- readLines("libre.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("abiword", {
  x <- read_rtf("abiword.rtf")
  ans <- readLines("abiword.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Japanese (cp932)", {
  x <- read_rtf("cp932.rtf")
  ans <- readLines("cp932.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Chinese and Korean", {
  x <- read_rtf("eastasia.rtf")
  ans <- readLines("eastasia.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Europe", {
  x <- read_rtf("europe.rtf")
  ans <- readLines("europe.ans", encoding = "UTF-8")
  #TBA...
  #check <- lapply(ans, function(a) which(x == a))
  #len <- lapply(check, length) %>% unlist()
  #expect_true(all(len == 1L))                # inclusion check
  #expect_true(all(diff(unlist(check)) > 0))  # order check
})
