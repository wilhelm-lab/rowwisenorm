#' @title panel.smooth2
#'
#' @description
#' Panel function used in the pairs plot (correlation plot) as lower.panel.
#'
#' @param x numeric vector of the same length as y
#' @param y numeric vector of the same length as x
#' @param col numeric or character code for the color(s) of the points
#' @param bg background color for symbol used for the points
#' @param pch symbol used for the points
#' @param cex expansion factor used for the points
#' @param col.smooth color to be used by lines function for drawing the smooths
#' @param span smoothing parameter f for lowess function
#' @param iter number of robustness iterations for lowess function
#' @param ... further arguments
#'
#' @return Lower panel (scatter plots) for pairs.
#'
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

panel.smooth2 <- function(x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
  idxs <- sample(1:length(x), max(length(x)*0.10, min(1000, length(x))))
  points(x[idxs], y[idxs], pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
}


# panel.smooth2 <- function(x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
#   idxs <- sample(1:length(x), max(length(x)*0.10, min(1000, length(x))))
#   points(x[idxs], y[idxs], pch = pch, col = col, bg = bg, cex = cex)
#   ok <- is.finite(x) & is.finite(y)
#   if (any(ok)) {
#     # Save the current par settings and restore them at the end
#     oldpar <- par(no.readonly = TRUE)
#     on.exit(par(oldpar))
#
#     # Set the par settings needed for lines()
#     par(usr = c(0, 1, 0, 1))
#
#     # Now, you can safely use lines()
#     lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
#   }
# }
#
# panel.smooth2 <- function(x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
#   idxs <- sample(1:length(x), max(length(x)*0.10, min(1000, length(x))))
#   points(x[idxs], y[idxs], pch = pch, col = col, bg = bg, cex = cex)
# }
#
# panel.smooth2 <- function(x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
#   idxs <- sample(1:length(x), max(length(x)*0.10, min(1000, length(x))))
#   points(x[idxs], y[idxs], pch = pch, col = col, bg = bg, cex = cex)
#
#   # Return a 'panel' function with no plot output
#   panel
# }
