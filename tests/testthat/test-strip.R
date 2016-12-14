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
  check <- c("リブレオフィスで作ったＲＴＦファイルです。",
             "アンダーラインを引いています。",
             "イタリックです。",
             "太字です。") %in% x
  expect_true(all(check))
})


test_that("abiword", {
  x <- striprtf("abiword.rtf", quiet = TRUE)
  check <- c("THIS IS A FILE CREATED BY ABIWORD.",
             "BASED ON CODE PAGE 1252.",
             "このファイルはアビワードを用いて作りました。",
             "コードページ１２５２に基づいています。") %in% x
  expect_true(all(check))
})


test_that("cp932", {
  x <- striprtf("cp932.rtf", quiet = TRUE)
  check <- c("This is a test file.",
             #"japanese here"
             "1234567890") %in% x
  expect_true(all(check))
})
