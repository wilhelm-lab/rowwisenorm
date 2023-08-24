#' @title panel.cor
#'
#' @description
#' Panel function used in the pairs() plot (correlation plot)
#' - Used as upper.panel
#'
#' @param x numeric vector, the first variable, provided by pairs
#' @param y numeric vector, the second variable, provided by pairs
#' @param digits how many digits to use on the pairs plot for the correlations
#' @param prefix text to prefix the correlation coefficients
#' @param cex.cor character expansion factor
#'
#' @return correlation coefficients for pairs
#'

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="p", method="p"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
