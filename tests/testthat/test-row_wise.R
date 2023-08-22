context("row-wise")     # Our file is called "test-row_wise.R"
library(testthat)        # load testthat package
library(rowwisenorm)       # load our package

test_that("read_files() returns a list of length 3", {
  returned_list <- read_files("proteinGroups.txt", "experimentalDesign.txt")
  expect_equal(length(returned_list), 3)
})

# TODO add more tests

