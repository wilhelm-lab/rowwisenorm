library(testthat)
library(rowwisenorm)


# tests for read_files

test_that("read_files() returns a list of length 3", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  expect_equal(length(return_list), 3)
})

test_that("read_files() returns an object of type list including 3 objects of class data.frame", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  additional_cols <- return_list[["additional_cols"]]
  expect_type(return_list, "list")
  expect_s3_class(lowest_level_df, "data.frame")
  expect_s3_class(exp_design, "data.frame")
  expect_s3_class(additional_cols, "data.frame")
})

test_that("read_files() stops with wrong condition names in experimental design", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignCond.txt", package = "rowwisenorm")

  expect_error(read_files(data_path1, data_path2))
})

test_that("read_files() works with a missing value inside experimental design", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignTest.txt", package = "rowwisenorm")

  expect_no_error(read_files(data_path1, data_path2))
})

test_that("read_files() stops with a condition having more than one row assigned in experimental design", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignTest2.txt", package = "rowwisenorm")

  expect_error(read_files(data_path1, data_path2))
})

test_that("read_files() works with withe space around the entries in experimental design", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignWS.txt", package = "rowwisenorm")

  expect_no_error(read_files(data_path1, data_path2))
})

# filtering check specific for this example data
test_that("read_files() filters out the correct number of rows", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignWS.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=F, rm_contaminant=F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 8069)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=F, rm_contaminant=F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7352)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=T, rm_contaminant=F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7822)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=F, rm_contaminant=T)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7949)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=T, rm_contaminant=F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7280)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=F, rm_reverse=T, rm_contaminant=T)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7703)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=F, rm_contaminant=T)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7244)

  return_list <- read_files(data_path1, data_path2, rm_only_by_site=T, rm_reverse=T, rm_contaminant=T)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_equal(nrow(lowest_level_df), 7172)
})


# TODO add more tests


# --> call 'devtools::test()' in console to run these tests
# csv files need to be stored in inst/extdata and not in /data, in /data use the rda format for any files

# --> call 'devtools::check()' to check if package works on various operating systems
# -> this shows what exactly needs to be modified in the code

# --> call 'rhub::check_for_cran()' to check for CRAN specific requirements:
# ----> use this: rhub::check_for_cran(path="C:/Users/User/Documents/rowwisenorm", email="sina.mohrenweis@t-online.de")
# -> needs validated email
# -> validate_email(), get token via this email
# -> list_validated_emails() shows validated emails with their token

# --> call 'devtools::build(path="C:/Users/User/Documents/rowwisenorm")' to build the package
# -> .tar.gz folder gets created

# MAY add '@keywords internal' to the functions that should be internal
