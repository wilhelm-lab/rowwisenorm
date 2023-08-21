#' @title panel.smooth2
#'
#' @description
#' Helper function for generating plots
#'

panel.smooth2 <- function(x, y, col = par("col"), bg = NA, pch = ".", cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
  idxs <- sample(1:length(x), max(length(x)*0.10, min(1000, length(x))))
  points(x[idxs], y[idxs], pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
}
