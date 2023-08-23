#' @title filter_rows
#'
#' @description
#' Filtering out rows with too many NA values
#'
#' @param data lowest-level data frame
#' @param keep_only_rows_with_x_valid_ratio only keep rows with more than this ratio of non-NA-values
#'
#' @return filtered lowest-level data frame
#'

filter_rows <- function(data, keep_only_rows_with_x_valid_ratio=0.5){
  num_na_per_row <- rowSums(is.na(data[, !colnames(data) %in% "row.number"]))
  threshold_na <- (1-keep_only_rows_with_x_valid_ratio) * ncol(data[, !colnames(data) %in% "row.number"])  # e.g. 0.5 * 12 = 6
  data <- data[num_na_per_row < threshold_na, ]  # e.g. only rows with < 6 (5 or less) NA, meaning at least 7 (> 50%) non-NA
  return(data)
}
