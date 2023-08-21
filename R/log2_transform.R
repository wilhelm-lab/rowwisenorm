#' @title log2_transform
#'
#' @param intensities lowest-level data frame
#'
#' @return log2 transformed lowest-level data frame
#'


log2_transform <- function(intensities){
  # log2 modify without ID column
  intensities[, !colnames(intensities) %in% "row.number"] <- log2(intensities[, !colnames(intensities) %in% "row.number"])
  return(intensities)
}
