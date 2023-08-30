#' @title pcaPlot2
#'
#' @description
#' Helper function for generating PCA plot with principal components
#'
#' @param data lowest-level data frame
#' @param main title for the plot
#'
#' @return creates a PCA plot
#'
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'


pcaPlot2 <- function(data, main="") {
  data <- data[!apply(data, 1, function(d) any(is.na(d))),]
  fit2 <- prcomp(t(data))
  plot(fit2$x[,1],fit2$x[,2],
       main=main,
       xlab=paste("PCA1 (",round((max(fit2$x[,1])-min(fit2$x[,1])), digits=1),"% )"),
       ylab=paste("PCA2 (",round((max(fit2$x[,2])-min(fit2$x[,2])), digits=1),"% )"),
       pch=7,
       cex=1.5,
       xlim = c(min(fit2$x[,1]+(min(fit2$x[,1])*0.1)),max(fit2$x[,1]+(max(fit2$x[,1])*0.1))),
       ylim = c(min(fit2$x[,2]+(min(fit2$x[,2])*0.1)),max(fit2$x[,2]+(max(fit2$x[,2])*0.1))))
  text(fit2$x[,1],fit2$x[,2], labels=rownames(fit2$x), font=2, pos=1, lwd=2, cex=1.5, offset=1)
}
