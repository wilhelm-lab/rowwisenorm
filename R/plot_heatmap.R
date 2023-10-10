#' @title plot_heatmap
#'
#' @description
#' Helper function for generating the heatmap plot
#'
#' @param data lowest-level data frame
#' @param main title for the plot
#'
#' @return creates a heatmap plot
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

plot_heatmap <- function(data, main=""){
  data <- data[, !colnames(data) %in% "row.number"]

  # require(pheatmap)
  pheatmap(cor(data, use="p", method="p"), main=main)

}
