library(testthat)
library(rowwisenorm)


# tests for sum_normalize

test_that("sum_normalize() works for refFunc being sum", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_no_error(sum_normalize(lowest_level_df))
})

test_that("sum_normalize() works for refFunc being median", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_no_error(sum_normalize(lowest_level_df, refFunc = "median"))
})

test_that("sum_normalize() works when refFunc contains white space or capital letters", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_no_error(sum_normalize(lowest_level_df, refFunc = " SuM"))
})

test_that("sum_normalize() works for norm=F", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_no_error(sum_normalize(lowest_level_df, norm = F))
})

test_that("sum_normalize() works for na.rm=F", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_no_error(sum_normalize(lowest_level_df, na.rm = F))
})

test_that("sum_normalize() returns object of class data.frame", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_s3_class(sum_normalize(lowest_level_df), "data.frame")
})

test_that("sum_normalize() returns data frame with same number of rows as input data", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), nrow(sum_normalize(lowest_level_df)))
})
