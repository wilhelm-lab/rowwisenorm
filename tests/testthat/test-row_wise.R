context("row-wise")     # Our file is called "test-row_wise.R"
library(testthat)        # load testthat package
library(rowwisenorm)       # load our package

test_that("read_files() returns a list of length 3", {
  data_path1 <- system.file("extdata", "proteinGroups.txt", package = "rowwisenorm")
  data_path2 <- system.file("extdata", "experimentalDesign.txt", package = "rowwisenorm")

  returned_list <- read_files(data_path1, data_path2)
  expect_equal(length(returned_list), 3)
})

# TODO add more tests


# --> call 'devtools::test()' in console to run these tests
# csv files need to be stored in inst/extdata and not in /data, in /data use the rda format for any files

# --> call 'devtools::check()' to check if package works on various operating systems
# -> this shows what exactly needs to be modified in the code

# --> call 'rhub::check_for_cran()' to check for CRAN specific requirements

