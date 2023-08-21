#' @title plotStats
#'
#' @description
#' Generates the plots with the help of the other helper functions
#'

plotStats <- function(data, main="") {
  require(pheatmap)
  n <- dim(data)[2]
  if (dim(data)[2] > 15) {
    # only draws first 15 columns, otherwise the plot might be too dense and result in an error
    n <- 15
  }
  pairs(data[,1:min(n, dim(data)[2])], lower.panel = panel.smooth2, upper.panel = panel.cor, main=main)
  pheatmap(cor(data, use="p", method="p"), main=main)
  try(pcaPlot(data, main=main))  # added main
  try(pcaPlot2(data, main=main))  # added main
}
