#' @title log2_transform
#'
#' @description
#' Logarithmic transformation. This represents as an optional pre-processing step.
#'
#' @param lowest_level_df lowest-level data frame
#'
#' @return Log2 transformed lowest-level data frame.
#'
#' @export
#'


log2_transform <- function(lowest_level_df){
  # log2 modify without ID column
  lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"] <- log2(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"])
  return(lowest_level_df)
}

