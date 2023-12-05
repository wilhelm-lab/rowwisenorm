#' @title pcaPlot
#'
#' @description
#' Helper function for generating the component plot
#'
#' @param data lowest-level data frame
#'
#' @return creates a component plot with the first and second principal components
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

pcaPlot <- function(data) {
  data <- data[, !colnames(data) %in% "row.number"]
  colnames(data) <- remove_common_prefix(colnames(data))[["strings_without_prefix"]]  # remove common prefix of column names

  data <- data[!apply(data, 1, function(d) any(is.na(d))),]  # remove rows with NA inside
  fit <- princomp(data, cor=TRUE)

  par(mar = rep(5, 4))  # adjust margins
  biplot(fit, main="Biplot displaying variable relationships \n along principal components 1 and 2")
}


