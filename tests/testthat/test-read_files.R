library(testthat)
library(rowwisenorm)


# tests for read_files

test_that("read_files() returns a list of length 5", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  expect_equal(length(return_list), 5)
})

test_that("read_files() returns an object of type list including 3 objects of class data.frame, one object of class character and one object of class integer", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  additional_cols <- return_list[["additional_cols"]]
  pca_colors <- return_list[["pca_colors"]]
  pca_symbols <- return_list[["pca_symbols"]]

  expect_type(return_list, "list")
  expect_s3_class(lowest_level_df, "data.frame")
  expect_s3_class(exp_design, "data.frame")
  expect_s3_class(additional_cols, "data.frame")
  expect_true(typeof(pca_colors) == "character")
  expect_true(typeof(pca_symbols) == "integer")
})

test_that("read_files() returns an object of type list including the additional cols as class data.frame even when additional_cols only includes one column", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, F, F, F)
  additional_cols <- return_list[["additional_cols"]]

  expect_type(return_list, "list")
  expect_s3_class(additional_cols, "data.frame")
})

test_that("read_files() works with a missing value inside experimental design", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignTest.txt", package = "rowwisenorm")

  expect_no_error(read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F))
})

test_that("read_files() stops with a condition having more than one row assigned in experimental design", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignTest2.txt", package = "rowwisenorm")

  expect_error(read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F))
})

test_that("read_files() stops when a condition name is missing in the first column in experimental design", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignMissingCond.txt", package = "rowwisenorm")

  expect_error(read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F))
})

test_that("read_files() works with white space around the entries in experimental design", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignWS.txt", package = "rowwisenorm")

  expect_no_error(read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F))
})

test_that("read_files() makes a warning when feature 'reverse' should be filtered but is missing in the file", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  expect_warning(read_files(data_path1, data_path2, rm_reverse = T, rm_only_by_site = F, rm_contaminant = F))
})

test_that("read_files() makes a warning when feature 'only by site' should be filtered but is missing in the file", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  expect_warning(read_files(data_path1, data_path2, rm_reverse = F, rm_only_by_site = T, rm_contaminant = F))
})

test_that("read_files() makes a warning when feature 'contaminant' should be filtered but is missing in the file", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  expect_warning(read_files(data_path1, data_path2, rm_reverse = F, rm_only_by_site = F, rm_contaminant = T))
})

test_that("read_files() stops when entry (column name) occurs more than once in exp_design", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessedDoubledEntry.txt", package = "rowwisenorm")

  expect_error(read_files(data_path1, data_path2, rm_reverse = F, rm_only_by_site = F, rm_contaminant = T))
})


# filtering check specific for this example data
# test_that("read_files() filters out the correct number of rows", {
#   data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
#   data_path2 <- system.file("extdata", "experimentalDesignWS.txt", package = "rowwisenorm")
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=F, rm_contaminant=F)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 8069)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=F, rm_contaminant=F)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7352)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=T, rm_contaminant=F)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7822)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=F, rm_contaminant=T)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7949)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=T, rm_contaminant=F)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7280)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=T, rm_contaminant=T)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7703)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=F, rm_contaminant=T)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7244)
#
#   return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=T, rm_contaminant=T)
#   lowest_level_df <- return_list[["lowest_level_df"]]
#   expect_equal(nrow(lowest_level_df), 7172)
# })
