#' @title median_normalize
#'
#' @description
#' Median normalization. This represents as an optional pre-processing step.
#' NA values are ignored inside the calculation of the median.
#'
#' @param data lowest-level data frame
#'
#' @return Median normalized lowest-level data frame.
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
