#' @title pcaPlot2
#'
#' @description
#' Helper function for generating PCA plot with principal components
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param main title for the plot
#' @param show_labels states whether the labels for the data points are shown
#' @param pdf_mode used for adjusting the plot for the PDF when generated in plot_results
#'
#' @return creates a PCA plot
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text legend
#'

pcaPlot2 <- function(data, exp_design, main="", show_labels=F, pdf_mode=F) {
  data <- data[, !colnames(data) %in% "row.number"]

  number_batches <- ncol(exp_design) -1
  number_conds <- nrow(exp_design)

  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  for (i in 1:number_batches){
    var_name <- paste("batch", i, sep="")
    var_value <- exp_design[, i+1]  # batch 1 is stored in column 2
    var_value <- gsub(" ", ".", var_value)  # TODO remove: again make the names in exp design and column names match
    var_value <- gsub("\\(", ".", var_value)
    var_value <- gsub("\\)", ".", var_value)
    var_value <- gsub("-", ".", var_value)

    vec1 <- append(vec1, length(var_value))  # maybe check when a missing field inside exp design -> filter empty (white space) places out of var_value before?
    vec2 <- append(vec2, var_value)
    vec3 <- append(vec3, var_name)
  }

  my_colors <- sample(colors(), number_batches)  # random pick as much colors as batches

  colors <- rep(my_colors, vec1)
  column_colors <- colors[match(names(data), vec2)]

  vec11 <- c()
  vec22 <- c()
  vec33  <- c()
  for (i in 1:number_conds){
    var_name <- exp_design[i, 1]  # name of the cond
    var_name <- gsub(" ", ".", var_name)
    var_name <- gsub("\\(", ".", var_name)
    var_name <- gsub("\\)", ".", var_name)
    var_name <- gsub("-", ".", var_name)

    var_value <- exp_design[i, 2:ncol(exp_design)]
    var_value <- gsub(" ", ".", var_value)  # again make the names in exp design and column names match
    var_value <- gsub("\\(", ".", var_value)
    var_value <- gsub("\\)", ".", var_value)
    var_value <- gsub("-", ".", var_value)

    vec11 <- append(vec11, length(var_value))  # maybe check when a missing field inside exp design -> filter empty (white space) places out of var_value before?
    vec22 <- append(vec22, var_value)
    vec33 <- append(vec33, var_name)
  }

  my_symbols <- sample(1:18, number_conds)

  symbols <- rep(my_symbols, vec11)
  column_symbols <- symbols[match(names(data), vec22)]

  if(pdf_mode){
    par(mar = c(15, 15, 15, 15), xpd = TRUE) # set margins before plot - bottom, left, top, right
  }
  else {
    par(mar = c(12, 6, 3, 12), xpd = TRUE) # set margins before plot
  }

  data <- data[!apply(data, 1, function(d) any(is.na(d))),]
  fit2 <- prcomp(t(data))

  if(show_labels){
    my_labels <- rownames(fit2$x)  # column names of the data
  }
  else {
    my_labels <- ""
  }


  # percentages for PC1 and PC2
  variance_explained <- round((fit2$sdev^2 / sum(fit2$sdev^2)) * 100, digits = 1)
  my_xlab = paste("PC1 (", variance_explained[1], "% )")
  my_ylab = paste("PC2 (", variance_explained[2], "% )")

  plot(fit2$x[,1],fit2$x[,2],
       col = column_colors,  # added
       pch = column_symbols,  # added
       main=main,
       xlab=my_xlab,  # added
       ylab=my_ylab,  # added
       cex=1.5,
       xlim = c(min(fit2$x[,1]+(min(fit2$x[,1])*0.1)),max(fit2$x[,1]+(max(fit2$x[,1])*0.1))),
       ylim = c(min(fit2$x[,2]+(min(fit2$x[,2])*0.1)),max(fit2$x[,2]+(max(fit2$x[,2])*0.1))))
  text(fit2$x[,1],fit2$x[,2], labels=my_labels, font=2, pos=1, lwd=2, cex=1.5, offset=1)  # show the labels depending on user's choice

  usr <- par("usr")
  greatest_x <- max(fit2$x[,1])  # rightest value of plot, depending on individual data
  right_shift <- ( max(fit2$x[,1]) - min(fit2$x[,1]) ) * 0.15  # 15 % of distance of greatest and smallest value on x axis
  # - alternatively just set a number as right_shift e.g. always 10 units next to greatest x value
  legend_width <- strwidth("xxBatch100xx")  # assume first legend with at most 999 batches + borders (width ~19)

  # first legend:  15 % of distance on x axis right to greatest x value (e.g. x going -50 until 100, this is 10% of 150 = 15 units shift, 15 % is 22,5 units)
  legend(x=greatest_x + right_shift, y=usr[4], title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added

  # second legend:  shifted next to other legend with shift being the width of first legend
  legend(x=greatest_x + right_shift + legend_width, y=usr[4], title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added

}
