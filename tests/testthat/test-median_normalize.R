library(devtools)
library(rowwisenorm)


# tests for median_normalize


test_that("median_normalize() correctly divides a column by its median", {
  example_df <- data.frame(
    row.number = c(1, 2, 3, 4),
    A = c(2, 5, 1, 3),
    B = c(7, 2, 9, 4),
    C = c(1, 6, 8, 2)
  )
  example_df_median <- median_normalize(example_df)
  expect_equal(example_df_median[,3], example_df[,3]/median(example_df[,3]))
})

test_that("median_normalize() correctly keeps the ID column the same", {
  example_df <- data.frame(
    row.number = c(1, 2, 3, 4),
    A = c(2, 5, 1, 3),
    B = c(7, 2, 9, 4),
    C = c(1, 6, 8, 2)
  )
  example_df_median <- median_normalize(example_df)
  expect_equal(example_df_median[,1], example_df[,1])
})

test_that("median_normalize() returns object of class data.frame", {
  data_path1 <- system.file("extdata", "proteingroups.csv", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesignProcessed.txt", package = "rowwisenorm")

  return_list <- read_files(data_path1, data_path2, rm_only_by_site = F, rm_reverse = F, rm_contaminant = F)
  lowest_level_df <- return_list[["lowest_level_df"]]
  expect_s3_class(median_normalize(lowest_level_df), "data.frame")
})
