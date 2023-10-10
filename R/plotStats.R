#' @title plotStats
#'
#' @description
#' Generates plots
#'
#' @param data lowest-level data frame
#' @param main title for the plots
#'
#' @return creates correlation plot, heatmap, component plot, and PCA plot
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom graphics lines pairs par points strwidth text
#' @importFrom stats biplot cor median na.omit prcomp princomp
#'

plotStats <- function(data, main="") {
  data <- data[, !colnames(data) %in% "row.number"]

  # calling the four helper functions
  plot_correlations(data=data, main=main)  # correlation plot
  plot_heatmap(data, main=main)  # heatmap
  try(pcaPlot(data, main=main))  # component plot
  try(pcaPlot2(data, main=main))  # PCA plot
}
