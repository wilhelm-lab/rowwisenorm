#' @title plot_heatmap
#'
#' @description
#' Helper function for generating the heatmap plot
#'
#' @param data lowest-level data frame
#' @param exp_design experimental design data frame
#' @param main title for the plot
#' @param pca_colors can be used to set the same batch colors as for the PCA plot
#'
#' @return creates a heatmap plot
#'
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom stats biplot cor median na.omit prcomp princomp
#' @importFrom graphics lines pairs par points strwidth text
#'

plot_heatmap <- function(data, exp_design, main="", pca_colors=NULL){
  # require(pheatmap)
  data <- data[, !colnames(data) %in% "row.number"]

  names <- colnames(data)
  annotation_df <- data.frame(matrix(NA, nrow = ncol(data), ncol = 2))
  rownames(annotation_df) <- names
  colnames(annotation_df) <- c("batch", "condition")

  # fill annotation data frame
  for (i in 1:nrow(exp_design)){
    for (j in 2:ncol(exp_design)){
      entry <- exp_design[i, j]
      annotation_df[entry, "batch"] <- j-1  # batch number
      annotation_df[entry, "condition"] <- exp_design[i, 1]  # condition name
    }
  }

  # if pca_colors vector is given, assign the colors (first color batch1, second color batch2 etc.)
  if (!is.null(pca_colors)){
    if (length(pca_colors) == (ncol(exp_design) -1)){  # sanity that as many colors as batches
      colors_for_batches <- setNames(pca_colors, seq(1, ncol(exp_design)-1))  # assign to batch numbers
      pheatmap(cor(data, use="p", method="p"), annotation_col = annotation_df,
               annotation_colors = list(batch = colors_for_batches), main=main)
    }
  }
  else {
    # heatmap with automatically generated colors
    pheatmap(cor(data, use="p", method="p"), annotation_col = annotation_df, main=main)
  }

}
