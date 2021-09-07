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


test_that("Special characters", {
  x <- read_rtf("special-chars.rtf")
  #ans <- readLines("special-chars.ans", encoding = "UTF-8")
  expect_equal(grep("\u2022", x), 4L)
  expect_equal(grep("\u2014", x), 5L)
  expect_equal(grep("\u2013", x), 6L)
  expect_equal(grep("\u2018", x), 7L)
  expect_equal(grep("\u2019", x), 8L)
  expect_equal(grep("\u201C", x), 9L)
  expect_equal(grep("\u201D", x), 10L)
})


test_that("Office for Mac", {
  x <- read_rtf("mac.rtf")
  #ans <- readLines("special-chars.ans", encoding = "UTF-8")
  expect_equal(grep("^This file"                           , x), 1L)
  expect_equal(grep("^\u3053\u306e\u30d5\u30a1\u30a4\u30eb", x), 3L)
  expect_equal(grep("^\u6b64\u6587\u4ef6\u662f\u7531"      , x), 5L)
  expect_equal(grep("^\uc774\u20\ud30c\uc77c\uc740"        , x), 7L)
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


test_that("Brace mismatch 1", {
  x <- read_rtf("brace_mismatch_1.rtf")
  ans <- readLines("brace_mismatch_1.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})


test_that("Brace mismatch 2", {
  x <- read_rtf("brace_mismatch_2.rtf")
  ans <- readLines("brace_mismatch_2.ans", encoding = "UTF-8")
  check <- lapply(ans, function(a) which(x == a))
  len <- lapply(check, length) %>% unlist()
  expect_true(all(len == 1L))                # inclusion check
  expect_true(all(diff(unlist(check)) > 0))  # order check
})
