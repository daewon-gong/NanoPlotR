library(NanoPlotR)

test_that("Input of non dataframe for modResults param throws error", {
  expect_error(getTopIds(modResults = "test", numTopIds = 10))
})

test_that("Input of non integer for numTopIds param throws error", {
  id <- c("ENSP1000", "ENSP1001", "ENSP1002", "ENSP1003")
  diff_mod_rate_test <- c(0.7, 0.9, 0.5, 0.2)
  df <- data.frame(id, diff_mod_rate_test)
  expect_error(getTopIds(modResults = df, numTopIds = "10"))
})

test_that("Valid inputs produce correct outputs", {
  id <- c("ENSP1000", "ENSP1001", "ENSP1002", "ENSP1003")
  diff_mod_rate_test <- c(0.7, 0.9, 0.5, 0.2)
  df1 <- data.frame(id, diff_mod_rate_test)

  #Try data set with different diff_mod_rate column name as we dynamically look for it.
  id <- c("ENSP1000", "ENSP1001", "ENSP1002", "ENSP1003")
  diff_mod_rate_hello <- c(0.7, 0.3, 0.9, 0.4)
  df2 <- data.frame(id, diff_mod_rate_hello)

  expect_setequal(getTopIds(modResults = df1, numTopIds = 2), c("ENSP1000", "ENSP1001"))
  expect_setequal(getTopIds(modResults = df2, numTopIds = 3), c("ENSP1000", "ENSP1002", "ENSP1003"))
})

test_that("Input of numTopIds greater than unique number of Ids in modResults throws warning", {
  id <- c("ENSP1000", "ENSP1001", "ENSP1002", "ENSP1003")
  diff_mod_rate_test <- c(0.7, 0.9, 0.5, 0.2)
  df <- data.frame(id, diff_mod_rate_test)
  expect_warning(getTopIds(modResults = df, numTopIds = 5))
})
