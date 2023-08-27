#' @title log2_transform
#'
#' @param lowest_level_df lowest-level data frame
#'
#' @return log2 transformed lowest-level data frame
#'


log2_transform <- function(lowest_level_df){
  contains_negatives <- any(lowest_level_df < 0)
  if(contains_negatives){
    message("There are negative values in the data which will be NaN values in the result.")
  }
  # log2 modify without ID column
  # TODO do we want to suppress warnings by R when NaNs are produced?
  suppressWarnings(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"] <- log2(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]))
  return(lowest_level_df)
}

