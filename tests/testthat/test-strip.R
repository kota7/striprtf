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
  ans <- readLines("libre.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("abiword", {
  x <- striprtf("abiword.rtf", quiet = TRUE)
  ans <- readLines("abiword.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Japanese (cp932)", {
  x <- striprtf("cp932.rtf", quiet = TRUE)
  ans <- readLines("cp932.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Chinese and Korean", {
  x <- striprtf("eastasia.rtf", quiet = TRUE)
  ans <- readLines("eastasia.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})
