#' @title pcaPlot2
#'
#' @description
#' PCA plot including the first and second principal components.The data points are colored by batch, while their symbols represent the conditions.
#' In addition, the score of the PCA plot is returned. The score describes how strictly the data points are clustered by condition.
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param show_labels Boolean that states whether the labels for the data points are shown
#' @param pdf_mode Boolean that can be set as T to adjust the plot for the PDF when generated in plot_results
#' @param set_colors optionally set colors for the batches as a vector of Strings
#' @param set_symbols optionally set symbols for the conditions as a vector of numerics
#'
#' @return The returned value is the score of the PCA plot. The PCA plot is displayed.
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text legend
#' @importFrom grDevices colors
#'

pcaPlot2 <- function(data, exp_design, show_labels=F, pdf_mode=F, set_colors=NULL, set_symbols=NULL) {
  data <- data[, !colnames(data) %in% "row.number"]

  # get common prefix of column names and cut it off
  remove_prefix_list <- remove_common_prefix(colnames(data))
  prefix <- remove_prefix_list[["common_prefix"]]
  colnames(data) <- remove_prefix_list[["strings_without_prefix"]]
  # remove the common prefix in exp_design entries
  for (i in 1:nrow(exp_design)){
    for (j in 2:ncol(exp_design)){
      exp_design[i,j] <- sub(paste0("^", prefix), "", exp_design[i,j])
    }
  }

  number_batches <- ncol(exp_design) -1
  number_conds <- nrow(exp_design)

  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  for (i in 1:number_batches){
    var_name <- paste("batch", i, sep="")
    var_value <- exp_design[, i+1]  # batch 1 is stored in column 2 etc
    var_value <- var_value[!grepl("^\\s*$", var_value)]  # exclude missing fields

    vec1 <- append(vec1, length(var_value))  # number of conditions for each batch
    vec2 <- append(vec2, var_value)  # column names inside data for each batch
    vec3 <- append(vec3, var_name)  # batch1, batch2 etc
  }

  is_white_or_nearly_white <- function(color) {
    color <- tolower(color)
    return(color == "white" || color == "snow" || grepl("^white|^whitesmoke|^whitesmoke$", color))
  }

  if (! is.null(set_colors) && length(set_colors) == number_batches){
    # when correct number of colors specified, use them
    my_colors <- set_colors
  } else {
    # generate colors - use no white colors
    colors_not_white <- setdiff(colors(), colors()[sapply(colors(), is_white_or_nearly_white)])
    if (number_batches <= length(colors_not_white)) {
      my_colors <- sample(colors_not_white, number_batches, replace = FALSE)  # pick no color two times
    } else {
      my_colors <- sample(colors_not_white, number_batches, replace = TRUE)  # if more batches than colors, allow a color is picked twice
    }
  }

  colors <- rep(my_colors, vec1)  # repeat each of the chosen colors as often as there are conditions for each of the batches
  column_colors <- colors[match(names(data), vec2)]  # assign to column names of data (vec2 stores the column names batch after batch in the right order)

  vec11 <- c()
  vec22 <- c()
  vec33  <- c()
  for (i in 1:number_conds){
    var_name <- exp_design[i, 1]  # name of the cond
    var_value <- exp_design[i, 2:ncol(exp_design)]
    var_value <- var_value[!grepl("^\\s*$", var_value)]  # exclude missing fields

    vec11 <- append(vec11, length(var_value))  # number of batches for each condition (row i)
    vec22 <- append(vec22, var_value)  # column names inside data for each condition
    vec33 <- append(vec33, var_name)  # names of the conditions
  }

  if (! is.null(set_symbols) && length(set_symbols) == number_conds){
    # when correct number of symbols specified, use them
    my_symbols <- set_symbols
  } else {
    # generate symbols
    if (number_conds <= 18) {
      my_symbols <- sample(0:18, number_conds, replace = FALSE)  # pick no symbol two times
    } else {
      my_symbols <- sample(0:18, number_conds, replace = TRUE)  # if more conditions than symbols, allow that a symbol is picked twice
    }  }

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
    my_labels <- rownames(fit2$x)  # column names of the data (data got transposed in prcomp)
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
       main="PCA Plot of principal components 1 and 2",
       xlab=my_xlab,  # added
       ylab=my_ylab,  # added
       cex=1.5,
       xlim = c(min(fit2$x[,1]+(min(fit2$x[,1])*0.1)),max(fit2$x[,1]+(max(fit2$x[,1])*0.1))),  # buffer for axes: 10% of the minimum value added to the minimum value, analog for maximum
       ylim = c(min(fit2$x[,2]+(min(fit2$x[,2])*0.1)),max(fit2$x[,2]+(max(fit2$x[,2])*0.1))))
  text(fit2$x[,1],fit2$x[,2], labels=my_labels, font=2, pos=1, lwd=2, cex=1.5, offset=1)  # show the labels depending on user's choice

  usr <- par("usr")
  greatest_x <- max(fit2$x[,1])  # rightest value of plot, depending on individual data
  right_shift <- ( max(fit2$x[,1]) - min(fit2$x[,1]) ) * 0.15  # 15 % of distance of greatest and smallest value on x axis
  legend_width <- strwidth("xxBatch100xx")  # assume first legend with at most 999 batches + borders (width ~19)

  # first legend:  15 % of distance on x axis right to greatest x value (e.g. x going -50 until 100, this is 10% of 150 = 15 units shift, 15 % is 22,5 units)
  legend(x=greatest_x + right_shift, y=usr[4], title = "Batches", legend = vec3, col = my_colors, pch = 19)  # add inset = c(-0.3, 0.0), to shift a bit up

  # second legend:  shifted next to other legend with shift being the width of first legend
  legend(x=greatest_x + right_shift + legend_width, y=usr[4], title = "Conditions", legend = vec33, col = "black", pch = my_symbols)


  # score:
  # coordinates of data points - normalized: division by max x and max y
  x <- fit2$x[,1] / greatest_x # x axis
  y <- fit2$x[,2] / max(fit2$x[,2])  # y axis

  my_labels <- rownames(fit2$x)  # newly define here since vector is set empty in case of show_labels=F
  data_coordinates <- data.frame(x, y, label = my_labels)  # assign coordinates to labels

  label_conds <- c()  # assign conditions to labels (data points)
  for (label in my_labels){
    matching_row <- apply(exp_design, 1, function(row) any(row == label))  # which row of exp_design contains the label (in read_files sanity check ensures that each label (= column of data) only once in exp_design)
    cond <- exp_design[matching_row, 1]  # first entry of this row (= the condition for this label)
    label_conds <- append(label_conds, cond)
  }
  data_coordinates$condition <- label_conds

  # mean x and y values for each condition - data frame 1
  mean_values <- aggregate(cbind(x, y) ~ condition, data = data_coordinates, FUN = mean)

  # merge the mean values into the data frame - data frame 2
  data_coordinates <- merge(data_coordinates, mean_values, by = "condition", suffixes = c("", ".mean.cond"))

  # loop over data frame 1: pairwise distances between mean coordinates of conditions, summed up
  distance_across <- 0
  for (i in 1:(nrow(mean_values) -1)){  # current row - run until one row less so not out of border
    index_next <- i+1
    for (j in index_next: nrow(mean_values)){  # remaining rows
      distance_across <- distance_across +
        (mean_values[i, "x"] - mean_values[j, "x"])^2 + (mean_values[i, "y"] - mean_values[j, "y"])^2

    }
  }

  # loop over data frame 2: pairwise distances from each point of a condition to its mean coordinates, summed up
  distance_inner <- 0
  for (i in 1: nrow(data_coordinates)){
    distance_inner <- distance_inner +
      (data_coordinates[i, "x"] - data_coordinates[i, "x.mean.cond"])^2 +
      (data_coordinates[i, "y"] - data_coordinates[i, "y.mean.cond"])^2
  }

  score <- round(distance_across/distance_inner, digits = 3)
  return(score)

}
