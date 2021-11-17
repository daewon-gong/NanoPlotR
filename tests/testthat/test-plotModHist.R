library(NanoPlotR)

test_that("Input of non dataframe for modResults param throws error", {
  expect_error(plotModHist(modResults = 0))
  expect_error(plotModHist(modResults = "test"))
})
