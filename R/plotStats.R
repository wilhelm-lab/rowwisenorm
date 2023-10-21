#' @title plotStats
#'
#' @description
#' Generates plots
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param main title for the plots
#' @param show_labels states whether the labels for the data points are shown inside the PCA plot
#' @param pdf_mode used for adjusting the PCA plot for the PDF when generated in plot_results
#'
#' @return creates correlation plot, heatmap, component plot, and PCA plot
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom graphics lines pairs par points strwidth text
#' @importFrom stats biplot cor median na.omit prcomp princomp
#'

plotStats <- function(data, exp_design, main="", show_labels=T, pdf_mode=F) {
  data <- data[, !colnames(data) %in% "row.number"]

  # calling the four helper functions
  plot_correlations(data=data, main=main)  # correlation plot
  plot_heatmap(data, main=main)  # heatmap
  try(pcaPlot(data, main=main))  # component plot
  try(pcaPlot2(data, exp_design, main=main, show_labels=show_labels, pdf_mode=pdf_mode))  # PCA plot

}
