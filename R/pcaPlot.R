#' @title pcaPlot
#'
#' @description
#' Helper function for generating the component plot
#'
#' @param data lowest-level data frame
#'
#' @return creates a component plot
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

pcaPlot <- function(data) {
  data <- data[, !colnames(data) %in% "row.number"]

  data <- data[!apply(data, 1, function(d) any(is.na(d))),]  # remove rows with NA inside
  fit <- princomp(data, cor=TRUE)

  par(mar = rep(5, 4))  # adjust margins
  biplot(fit, main="PCA Biplot: \n Directions of the Variables in relation to Comp. 1 and Comp. 2")
}


