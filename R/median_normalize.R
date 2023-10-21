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
  data_n <- data[, !colnames(data) %in% "row.number"]  # exclude ID column

  data_n <- as.data.frame(lapply(data_n, function(col) col/median(col, na.rm = T)))
  data_n <- cbind(row.number = data$row.number, data_n)  # add ID column back

  return(data_n)
}
