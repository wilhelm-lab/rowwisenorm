library(devtools)
library(rowwisenorm)


# tests for plot_results

test_that("plot_results() creates PDF file correctly in default location", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]

  plot_results(lowest_level_df = lowest_level_df, exp_design = exp_design)
  expected_file <- "result_rowwisenorm.pdf"
  expect_true(file.exists(expected_file))
})

test_that("plot_results() creates PDF file correctly with specified name in specified directory", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2)
  lowest_level_df <- return_list[["lowest_level_df"]]
  exp_design <- return_list[["exp_design"]]

  plot_results(lowest_level_df = lowest_level_df, exp_design = exp_design, main = "my_results", output_dir = "res")
  expected_file <- "res/my_results.pdf"
  expect_true(file.exists(expected_file))
})
