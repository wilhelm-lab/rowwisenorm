library(devtools)
library(rowwisenorm)


# tests for write_outfile

test_that("write_outfile() creates file for lowest level correctly in default location", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]

  write_outfile(lowest_level_df = lowest_level_df)
  expected_file <- "output_rowwisenorm.csv"
  expect_true(file.exists(expected_file))
})

test_that("write_outfile() creates the complete file correctly in default location", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]
  additional_cols <- return_list[["additional_cols"]]

  write_outfile(lowest_level_df = lowest_level_df, additional_cols = additional_cols)
  expected_file <- "output_rowwisenorm_complete.csv"
  expect_true(file.exists(expected_file))
})

test_that("write_outfile() creates file with specified name in specified directory", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]

  write_outfile(lowest_level_df = lowest_level_df, filename = "myfile", output_dir = "myout")
  expected_file <- "myout/myfile.csv"
  expect_true(file.exists(expected_file))
})

test_that("write_outfile() correctly removes white space (test for specified name and directory)", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]

  write_outfile(lowest_level_df = lowest_level_df, filename = " myfilews ", output_dir = "  myoutws ")
  expected_file <- "myoutws/myfilews.csv"
  expect_true(file.exists(expected_file))
})
