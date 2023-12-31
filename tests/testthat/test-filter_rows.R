library(devtools)
library(rowwisenorm)


# tests for filter_rows

test_that("filter_rows() filters out the correct number of rows", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_contaminant = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  lowest_level_df_filtered <- filter_rows(lowest_level_df)
  expect_equal(nrow(lowest_level_df_filtered), 6315)  # specific for this example data
})

test_that("filter_rows() returns object of class data.frame", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_contaminant = F, rm_reverse = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_s3_class(filter_rows(data = lowest_level_df), "data.frame")
})


