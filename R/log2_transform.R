#' @title log2_transform
#'
#' @description
#' Logarithmic transformation that can optionally be applied to the lowest-level data
#'
#' @param lowest_level_df lowest-level data frame
#'
#' @return log2 transformed lowest-level data frame
#'
#' @export
#'


log2_transform <- function(lowest_level_df){
  # log2 modify without ID column
  lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"] <- log2(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"])
  return(lowest_level_df)
}

