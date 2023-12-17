#' @title filter_rows
#'
#' @description
#' Filtering out rows that do not contain more real values than the specified ratio.
#' The ratio symbolizes an exclusive lower bound.
#' Exceptions: ratio = 0 means no filtering is done, ratio = 1 means keeping only rows with 100 percent real values.
#'
#' @param data lowest-level data frame
#' @param keep_only_rows_with_x_valid_ratio only keep rows with more than this ratio of non-NA-values
#'
#' @return filtered lowest-level data frame
#'
#' @export
#'

filter_rows <- function(data, keep_only_rows_with_x_valid_ratio=0.5){
  ratio_ok <- TRUE
  if (is.character(keep_only_rows_with_x_valid_ratio)){
    ratio_ok <- FALSE
    warning("The ratio needs to be entered as a numeric. No filtering was done.")
  }
  else if (keep_only_rows_with_x_valid_ratio < 0 | keep_only_rows_with_x_valid_ratio > 1){
    ratio_ok <- FALSE
    warning("The ratio needs to be between 0 and 1. No filtering was done.")
  }
  if (ratio_ok){
    num_na_per_row <- rowSums(is.na(data[, !colnames(data) %in% "row.number"]))
    threshold_na <- (1-keep_only_rows_with_x_valid_ratio) * ncol(data[, !colnames(data) %in% "row.number"])  # e.g. 0.5 * 12 = 6
    # edge cases: for 0 also rows with only NA allowed, for 1 those with 100% non-NA
    if (keep_only_rows_with_x_valid_ratio == 0 | keep_only_rows_with_x_valid_ratio == 1){
      data <- data[num_na_per_row <= threshold_na, ]
    }
    else {  # usual
      data <- data[num_na_per_row < threshold_na, ]  # e.g. only rows with < 6 (5 or less) NA, meaning at least 7 (> 50%) non-NA
    }
  }
  return(data)
}
