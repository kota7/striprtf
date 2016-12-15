library(testthat)
library(striprtf)

context("Strip RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic", {
  x <- striprtf("msword.rtf")
  check <- "This is a test file." %in% x
  expect_true(check)
})


test_that("big", {
  x <- striprtf("big.rtf")
  check <- sum(x == "THIS IS A MUCH BIGGER FILE")
  expect_true(check == 360L)
})


test_that("libre", {
  x <- striprtf("libre.rtf")
  ans <- readLines("libre.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("abiword", {
  x <- striprtf("abiword.rtf")
  ans <- readLines("abiword.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Japanese (cp932)", {
  x <- striprtf("cp932.rtf")
  ans <- readLines("cp932.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Chinese and Korean", {
  x <- striprtf("eastasia.rtf")
  ans <- readLines("eastasia.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Europe", {
  x <- striprtf("europe.rtf")
  ans <- readLines("europe.ans", encoding = "UTF-8")
  #TBD...
  #check <- lapply(ans, function(a) which(x == a))
  #len <- lapply(check, length) %>% unlist()
  #expect_true(all(len == 1L))                # inclusion check
  #expect_true(all(diff(unlist(check)) > 0))  # order check
})
