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

plotStats <- function(data, main="") {
  require(pheatmap)
  n <- dim(data)[2]
  if (dim(data)[2] > 15) {
    # only draws first 15 columns, otherwise the plot might be too dense and result in an error
    n <- 15
  }
  pairs(data[,1:min(n, dim(data)[2])], lower.panel = panel.smooth2, upper.panel = panel.cor, main=main)  # correlation plot
  pheatmap(cor(data, use="p", method="p"), main=main)  # heatmap
  try(pcaPlot(data, main=main))  # component plot
  try(pcaPlot2(data, main=main))  # PCA plot
}
