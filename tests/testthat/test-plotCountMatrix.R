library(NanoPlotR)
library(ggplot2)

test_that("Input of non dataframe for modResults param throws error", {
  expect_error(plotCountMatrix(modResults = 0, modSites = c("A"), numTopIds = 20))
  expect_error(plotCountMatrix(modResults = "test", modSites = c("A"), numTopIds = 20))
  expect_error(plotCountMatrix(modResults = c(0, 2), modSites = c("A"), numTopIds = 20))
})

test_that("Input of dataframe with missing required columns throws error", {
  id <- c("ENSP1000", "ENSP1001")
  modRate <- c(0.8, 0.2)
  diff_mod_rate_test <- c(0.7, 0.6)
  df <- data.frame(id, modRate, diff_mod_rate_test)
  expect_error(plotCountMatrix(modResults = df, numTopIds = 10))
})

test_that("Input of wrong data type for numTopIds throws error", {
  id <- c("ENSP1000", "ENSP1001")
  kmer <- c("ATGAA", "TCATC")
  diff_mod_rate_test <- c(0.7, 0.6)
  df <- data.frame(id, kmer, diff_mod_rate_test)
  expect_error(plotCountMatrix(modResults = df, numTopIds = "10"))
})

test_that("Input of wrong data type for modSites throws error", {
  id <- c("ENSP1000", "ENSP1001")
  kmer <- c("ATGAA", "TCATC")
  diff_mod_rate_test<- c(0.7, 0.6)
  df <- data.frame(id, kmer, diff_mod_rate_test)
  expect_error(plotCountMatrix(modResults = df, modSites = 10, numTopIds = "10"))
})

test_that("Valid inputs produce a ggplot", {
  id <- c("ENSP1000", "ENSP1001", "ENSP1002")
  kmer <- c("ATGAA", "TCATC", "ATATA")
  diff_mod_rate_test<- c(0.7, 0.6, 0.5)
  df <- data.frame(id, kmer, diff_mod_rate_test)
  expect_true(is.ggplot(plotCountMatrix(modResults = df, numTopIds = 2)))
})
