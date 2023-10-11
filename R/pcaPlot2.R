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
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

# TODO added exp_design, need to change in all others: plotStats, plot_results, calls of those in shiny app (overgive exp design df)

pcaPlot2 <- function(data, exp_design, main="") {
  data <- data[, !colnames(data) %in% "row.number"]

  number_batches <- ncol(exp_design) -1
  number_conds <- nrow(exp_design)

  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  for (i in 1:number_batches){
    var_name <- paste("batch", i, sep="")
    var_value <- exp_design[, i+1]  # batch 1 is stored in column 2
    var_value <- gsub(" ", ".", var_value)  # again make the names in exp design and column names match
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

  # check
  # print("vec1")
  # print(vec1)
  # print("vec2")
  # print(vec2)
  # print("vec3")
  # print(vec3)
  # print("colors")
  # print(colors)
  # print("column_colors")
  # print(column_colors)
  #
  # print("----------")
  # print("vec11")
  # print(vec11)
  # print("vec22")
  # print(vec22)
  # print("vec33")
  # print(vec33)
  # print("symbols")
  # print(symbols)
  # print("column_symbols")
  # print(column_symbols)

  par(mar = c(6, 6, 3, 6), xpd = TRUE) # set margins before plot

  data <- data[!apply(data, 1, function(d) any(is.na(d))),]
  fit2 <- prcomp(t(data))
  plot(fit2$x[,1],fit2$x[,2],
       col = column_colors,  # added
       pch = column_symbols,  # added
       main=main,
       xlab=paste("PCA1 (",round((max(fit2$x[,1])-min(fit2$x[,1])), digits=1),"% )"), # TODO check labels, should be together not > 100% ?
       ylab=paste("PCA2 (",round((max(fit2$x[,2])-min(fit2$x[,2])), digits=1),"% )"),
       #pch=7,
       cex=1.5,
       xlim = c(min(fit2$x[,1]+(min(fit2$x[,1])*0.1)),max(fit2$x[,1]+(max(fit2$x[,1])*0.1))),
       ylim = c(min(fit2$x[,2]+(min(fit2$x[,2])*0.1)),max(fit2$x[,2]+(max(fit2$x[,2])*0.1))))
  text(fit2$x[,1],fit2$x[,2], labels=rownames(fit2$x), font=2, pos=1, lwd=2, cex=1.5, offset=1)

  legend("topright", title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added
  legend("bottomright", title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added

}
