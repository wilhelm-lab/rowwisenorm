#' @title median_normalize
#'
#' @description
#' Median normalization of the lowest-level data frame
#'
#' @param data lowest-level data frame
#'
#' @return median normalized lowest-level data frame
#'
#' @export
#'
#' @importFrom stats median
#'


median_normalize <- function(data){
  # median normalize all columns but the ID column
  data[, !colnames(data) %in% "row.number"] <- as.data.frame(lapply(data[, !colnames(data) %in% "row.number"], function(col) col/median(col, na.rm = T)))
  return(data)
}
