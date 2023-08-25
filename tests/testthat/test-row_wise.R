library(testthat)        # load testthat package
library(rowwisenorm)       # load our package


# tests for read_files

test_that("read_files() returns a list of length 3", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  returned_list <- read_files(data_path1, data_path2)
  expect_equal(length(returned_list), 3)
})

test_that("read_files() returns an object of type list including 3 objects of type data.frame", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  returned_list <- read_files(data_path1, data_path2)
  intensities <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]
  additional_cols <- return_list[["additional_cols"]]
  expect_type(returned_list, "list")
  expect_s3_class(intensities, "data.frame")
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

# maybe check filtering, nrow where + inside and if this is substracted (many cases)
# -> check that final row number is correct then


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
