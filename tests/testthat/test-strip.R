library(testthat)
library(striprtf)

context("Strip RTF")

# for debugging
#setwd("tests/testthat/")


test_that("basic", {
  x <- striprtf("msword.rtf", quiet = TRUE)
  check <- "This is a test file." %in% x
  expect_true(check)
})


test_that("big", {
  x <- striprtf("big.rtf", quiet = TRUE)
  check <- sum(x == "THIS IS A MUCH BIGGER FILE")
  expect_true(check == 360L)
})


test_that("libre", {
  x <- striprtf("libre.rtf", quiet = TRUE)
  ans <- scan("libre.ans", what = "character", sep = "\n", quiet = TRUE)
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("abiword", {
  x <- striprtf("abiword.rtf", quiet = TRUE)
  ans <- scan("abiword.ans", what = "character", sep = "\n", quiet = TRUE)
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("cp932", {
  x <- striprtf("cp932.rtf", quiet = TRUE)
  ans <- scan("cp932.ans", what = "character", sep = "\n", quiet = TRUE)
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})
