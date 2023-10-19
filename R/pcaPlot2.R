#' @title pcaPlot2
#'
#' @description
#' Helper function for generating PCA plot with principal components
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param main title for the plot
#' @param show_labels states whether the labels for the data points are shown
#' @param legend_shift states how much the second legend is shifted to the right inside PDF
#' @param pdf_mode used for adjusting the plot for the PDF when generated in plot_results
#'
#' @return creates a PCA plot
#'
#' @export
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text legend
#'

# TODO remove legend_shift parameter
# -> do not use it, always do legends the same way (as currently)

pcaPlot2 <- function(data, exp_design, main="", show_labels=F, legend_shift=20, pdf_mode=F) {
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

  if(pdf_mode){
    par(mar = c(15, 15, 15, 15), xpd = TRUE) # set margins before plot
  }
  else {
    par(mar = c(6, 6, 3, 6), xpd = TRUE) # set margins before plot
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
       #xlab=paste("PCA1 (",round((max(fit2$x[,1])-min(fit2$x[,1])), digits=1),"% )"),
       #ylab=paste("PCA2 (",round((max(fit2$x[,2])-min(fit2$x[,2])), digits=1),"% )"),
       xlab=my_xlab,  # added
       ylab=my_ylab,  # added
       #pch=7,
       cex=1.5,
       xlim = c(min(fit2$x[,1]+(min(fit2$x[,1])*0.1)),max(fit2$x[,1]+(max(fit2$x[,1])*0.1))),
       ylim = c(min(fit2$x[,2]+(min(fit2$x[,2])*0.1)),max(fit2$x[,2]+(max(fit2$x[,2])*0.1))))
  text(fit2$x[,1],fit2$x[,2], labels=my_labels, font=2, pos=1, lwd=2, cex=1.5, offset=1)  # show the labels depending on user's choice

  # legends
  # legend("topright", title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added
  # legend("topright", title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added

  usr <- par("usr")
  greatest_x <- max(fit2$x[,1])  # rightest value of plot, depending on individual data
  right_shift <- ( max(fit2$x[,1]) - min(fit2$x[,1]) ) * 0.1  # 10 % of distance of greatest and smallest value on x axis
  # - alternatively just set a number as right_shift e.g. always 10 units next to greatest x value
  legend_width <- strwidth("xxBatch100xx")  # assume legend with at most 999 batches plus borders (width ~19)

  # first legend:  10% of distance on x axis right to greatest x value (e.g. x going -50 until 100, this is 10% of 150 = 15 units shift)
  legend(x=greatest_x + right_shift, y=usr[4], title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added

  # second legend:  shifted next to other legend with shift being the width of first legend
  legend(x=greatest_x + right_shift + legend_width, y=usr[4], title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added



  # alternatively: separate for pdf mode (both legends right side and shift parameter)
  # usr <- par("usr")
  # if(pdf_mode){
  #   # Set the x and y coordinates for the legend relative to the user coordinates
  #   x_legend <- usr[2] # right border
  #   y_legend <- usr[4] # top border
  #   legend(x = x_legend, y = y_legend, title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added
  #
  #   # Update the x-coordinate and the y-coordinate for the second legend
  #   x_legend <- usr[2] + legend_shift # right border with shift parameter
  #   y_legend <- usr[4] # top border
  #   legend(x = x_legend, y = y_legend, title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added
  #
  # }
  # else {
  #   legend("bottomleft", title = "Batches", inset = c(-0.3, 0.0), legend = vec3, col = my_colors, pch = 19)  # added
  #   legend("topright", title = "Conditions", inset = c(-0.3, 0.0), legend = vec33, col = "black", pch = my_symbols)  # added
  # }


  # new ggplot

  # pca_plot <- ggplot(as.data.frame(t(data)), aes(x = fit2$x[,1], y = fit2$x[,2], label = my_labels)) +
  #   geom_point(aes(color = as.factor(column_colors), shape = as.factor(column_symbols), size = 3)) +
  #   theme_minimal() +
  #   labs(title = main, x = my_xlab, y = my_ylab) +
  #   scale_color_manual(values = as.factor(column_colors)) +
  #   scale_shape_manual(values = as.factor(column_symbols)) +
  #   xlim(c(min(fit2$x[,1]) - 0.1, max(fit2$x[,1]) + 0.1)) +
  #   ylim(c(min(fit2$x[,2]) - 0.1, max(fit2$x[,2]) + 0.1)) +
  #   geom_text(hjust = 0, vjust = 0) +
  #   theme(legend.position = "none")  # Remove automatic legend
  #
  # print(pca_plot)

  # Create a data frame with PCA results and custom aesthetics
  # pca_data <- data.frame(
  #   PC1 = fit2$x[,1],
  #   PC2 = fit2$x[,2],
  #   Group = column_colors,  # Use your custom colors
  #   Symbol = column_symbols,  # Use your custom symbols,
  #   Label = my_labels
  # )
  #
  # # Convert custom colors and symbols to factors
  # pca_data$Group <- as.factor(pca_data$Group)
  # pca_data$Symbol <- as.factor(pca_data$Symbol)
  #
  # # Create a PCA plot with ggplot2
  # pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, label = Label, color = Group, shape = Symbol)) +
  #   geom_point(size = 3) +
  #   theme_minimal() +
  #   labs(title = main, x = my_xlab, y = my_ylab) +
  #   scale_color_manual(values = unique(pca_data$Group)) +
  #   scale_shape_manual(values = unique(pca_data$Symbol)) +
  #   xlim(c(min(pca_data$PC1) - 0.1, max(pca_data$PC1) + 0.1)) +
  #   ylim(c(min(pca_data$PC2) - 0.1, max(pca_data$PC2) + 0.1)) +
  #   geom_text(hjust = 0, vjust = 0) +
  #   theme(legend.position = "none")  # Remove automatic legend
  #
  # # Add legends manually
  # pca_plot +
  #   annotate("text", x = -2.5, y = -4, label = "Batches", size = 5) +
  #   annotate("text", x = 3.5, y = 4, label = "Conditions", size = 5) +
  #   scale_shape_manual(name = "Batches", values = column_symbols, labels = vec3) +
  #   scale_color_manual(name = "Conditions", values = column_colors, labels = vec33)

  # NEW

  # # Create a data frame containing PCA results, labels, and factors for Batches and Conditions
  # pca_data <- data.frame(
  #   PC1 = fit2$x[, 1],
  #   PC2 = fit2$x[, 2],
  #   Label = ifelse(show_labels, rownames(fit2$x), ""),
  #   Batch = factor(column_colors, levels = unique(column_colors)),
  #   Condition = factor(column_symbols, levels = unique(column_symbols))
  # )
  #
  # # Create a PCA plot using ggplot2
  # pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, label = Label, color = Batch, shape = Condition)) +
  #   geom_point(size = 3) +
  #   theme_minimal() +
  #   labs(
  #     title = main,
  #     x = my_xlab,
  #     y = my_ylab
  #   ) +
  #   xlim(c(min(pca_data$PC1) - 0.1, max(pca_data$PC1) + 0.1)) +
  #   ylim(c(min(pca_data$PC2) - 0.1, max(pca_data$PC2) + 0.1)) +
  #   geom_text(hjust = 0, vjust = 0, size = 5)   # Adjust the size of labels
  #
  # # Add legends for Batches and Conditions
  # pca_plot <- pca_plot +
  # scale_color_manual(name = "Batches", values = my_colors) +
  # scale_shape_manual(name = "Conditions", values = my_symbols) +
  # theme(legend.position = c(0.02, 0.98))  # Adjust the legend position
  #
  # # Display the PCA plot
  # print(pca_plot)

}
