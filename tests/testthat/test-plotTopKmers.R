library(NanoPlotR)
library(ggplot2)

test_that("Input of non dataframe for modResults param throws error", {
  expect_error(plotTopKmers(modResults = c("test"), numKmers = 10))
  expect_error(plotTopKmers(modResults =  10, numKmers = 10))
  expect_error(plotTopKmers(modResults = "test", numKmers = 10))
})

test_that("Input of non integer for numKmers param throws error", {
  kmer <- c("ATGAA", "TCATC")
  id <- c("ENSP1000", "ENSP1001")
  df <- data.frame(kmer, id)
  expect_error(plotTopKmers(modResults = df, numKmers = df))
  expect_error(plotTopKmers(modResults = df, numKmers = "10"))
})

test_that("Input of dataframe with missing required columns throws error", {
  id <- c("ENSP1000", "ENSP1001")
  modRate <- c(0.8, 0.2)
  df <- data.frame(id, modRate)
  expect_error(plotTopKmers(modResults = df, numKmers = 10))
})

test_that("Valid inputs produce a ggplot", {
  id <- c("ENSP1000", "ENSP1001", "ENSP1002")
  kmer <- c("ATGAA", "TCATC", "ATGAA")
  df <- data.frame(id, kmer)
  expect_true(is.ggplot(plotTopKmers(modResults = df, numKmers = 2)))
})

test_that("Input of numKmers greater than kmers present in modResults throws warning", {
  id <- c("ENSP1000", "ENSP1001")
  kmer <- c("ATGAA", "TCATC")
  df <- data.frame(id, kmer)
  expect_warning(plotTopKmers(modResults = df, numKmers = 10))
})
