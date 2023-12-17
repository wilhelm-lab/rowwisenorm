#' @title panel.cor
#'
#' @description
#' Panel function used in the pairs plot (correlation plot) as upper.panel.
#'
#' @param x numeric vector, the first variable, provided by pairs
#' @param y numeric vector, the second variable, provided by pairs
#' @param digits number of digits to use on the pairs plot for the correlations
#' @param prefix text to prefix the correlation coefficients
#' @param cex.cor character expansion factor
#' @param ... further arguments
#'
#' @return correlation coefficients for pairs
#'
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="p", method="p"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
#   # the same:
#   r <- abs(cor(x, y, use="p", method="p"))
#   txt <- format(c(r, 0.123456789), digits = digits)[1]
#   txt <- paste0(prefix, txt)
#   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#
#   # Save the current par settings and restore them at the end
#   oldpar <- par(no.readonly = TRUE)
#   on.exit(par(oldpar))
#
#   # Set the par settings needed for text output
#   par(usr = c(0, 1, 0, 1))
#
#   # Now, you can safely use text()
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }
#
# panel.cor <- function(x, y, ...) {
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(usr[1], usr[2], 0, 1))
#
#   r <- abs(cor(x, y, use="p", method="p"))
#   txt <- format(c(r, 0.123456789), digits = 2)[1]
#   text(0.5, 0.5, txt, cex = 0.8)
# }
