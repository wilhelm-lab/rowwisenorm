#' @title plot_correlations
#'
#' @description
#' Helper function for generating the correlation plot
#'
#' @param data lowest-level data frame
#'
#' @return creates a correlation plot
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

plot_correlations <- function(data){
  data <- data[, !colnames(data) %in% "row.number"]
  colnames(data) <- remove_common_prefix(colnames(data))[["strings_without_prefix"]]  # remove common prefix of column names

  # only draws at most first 15 columns, otherwise the plot might be too dense and result in an error
  pairs(data[,1:min(15, dim(data)[2])], lower.panel = panel.smooth2, upper.panel = panel.cor,
        main="Pairwise relationships \n Pearson Correlation Coefficient Matrix and Scatter Plots",
        oma = c(4, 4, 8, 4))  # increase top margin

}
