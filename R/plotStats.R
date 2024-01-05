#' @title plotStats
#'
#' @description
#' Generation of all four plots at once. The plots are a correlation plot, a heatmap plot, a component plot, and a PCA plot.
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param show_labels Boolean that states whether the labels for the data points are shown inside the PCA plot
#' @param pdf_mode Boolean used for adjusting the PCA plot margins for the PDF
#' @param set_colors optionally set colors for the batches inside the PCA plot and inside the heatmap as a vector of Strings
#' @param set_symbols optionally set symbols for the conditions inside the PCA plot as a vector of numerics
#'
#' @return Displays the correlation plot, the heatmap plot, the component plot, the and PCA plot.
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom graphics lines pairs par points strwidth text
#' @importFrom stats biplot cor median na.omit prcomp princomp
#'

plotStats <- function(data, exp_design,  show_labels=F, pdf_mode=F, set_colors=NULL, set_symbols=NULL) {
  data <- data[, !colnames(data) %in% "row.number"]

  # calling the four helper functions
  plot_correlations(data=data)  # correlation plot
  plot_heatmap(data, exp_design, batch_colors=set_colors)  # heatmap with same colors as PCA if set
  try(pcaPlot(data))  # component plot
  try(pcaPlot2(data, exp_design, show_labels=show_labels, pdf_mode=pdf_mode,
               set_colors=set_colors, set_symbols=set_symbols))  # PCA plot

}
