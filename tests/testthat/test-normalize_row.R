library(testthat)
library(rowwisenorm)


# tests for normalize_row

test_that("normalize_row() gives a warning when there are no possible references", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignNoRefs.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]

  expect_warning(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design))
})

test_that("normalize_row() gives a warning when user enters non-possible condition as ref", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignNotAllRefs.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_warning(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, ref = c("L", "M", "H")))
})

test_that("normalize_row() works when no references are set - automatically takes refs", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design))
})

test_that("normalize_row() works when user enters the same condition more than once as ref", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, ref = c("L", "L", "M", "H")))
})

test_that("normalize_row() works when user enters a condition with white space", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, ref = c("  L", "M", "H")))
})

test_that("normalize_row() works when refFunc includes white space and capital letters", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, refFunc = " MediAn"))
})

test_that("normalize_row() works for refFunc sum", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, refFunc = "sum"))
})

test_that("normalize_row() works for na.rm False", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_no_error(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design, na.rm = F))
})

test_that("normalize_row() returns object of class data.frame", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_s3_class(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design), "data.frame")
})

test_that("normalize_row() returns data frame with same number of rows as input data", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_contaminant = F, rm_only_by_site = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  expect_equal(nrow(lowest_level_df), nrow(normalize_row(lowest_level_df = lowest_level_df, exp_design = exp_design)))
})
