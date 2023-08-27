library(devtools)
library(rowwisenorm)


# tests for log2_transform

test_that("log2_transform() also works when negative values are in the data", {
  example_df <- data.frame(
    A = c(-2, 5, -1, -3),
    B = c(-7, 2, -9, -4),
    C = c(-1, -6, 8, -2)
  )
  expect_no_error(log2_transform(example_df))
})

test_that("log2_transform() correctly prints a message when negative values are in the data", {
  example_df <- data.frame(
    A = c(-2, 5, -1, -3),
    B = c(-7, 2, -9, -4),
    C = c(-1, -6, 8, -2)
  )
  expect_message(log2_transform(example_df), "")
})

test_that("log2_transform() correctly performs log2", {
  example_df <- data.frame(
    A = c(-2, 5, -1, -3),
    B = c(-7, 2, -9, -4),
    C = c(-1, -6, 8, -2)
  )
  example_df_log <- log2_transform(example_df)
  expect_equal(example_df_log[3,3], log2(example_df[3,3]))
})

