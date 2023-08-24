#' @title pcaPlot
#'
#' @description
#' Helper function for generating the component plot
#'
#' @param data lowest-level data frame
#' @param main title for the plot
#'
#' @return creates a component plot
#'

pcaPlot <- function(data, main="") {
  data <- data[!apply(data, 1, function(d) any(is.na(d))),]  # remove rows with NA inside
  fit <- princomp(data, cor=TRUE)
  biplot(fit, main=main)
}


