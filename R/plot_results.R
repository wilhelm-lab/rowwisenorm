#' @title plot_results
#'
#' @description
#' Generation and download of all plots inside an output pdf file. Additionally, an SVG file can be generated for each plot.
#' The plots are a correlation plot, a heatmap plot, a component plot, and a PCA plot.
#'
#' @param lowest_level_df lowest-level data frame
#' @param exp_design experimental design data frame
#' @param filename optionally specify the name of the output file
#' @param output_dir optionally specify the directory in which the output file will be saved
#' @param show_labels Boolean that states whether the labels for the data points are shown inside the PCA plot
#' @param svg Boolean that defines whether a svg file is created for each of the plots additionally
#' @param set_colors optionally set colors for the batches inside the PCA plot and inside the heatmap as a vector of Strings
#' @param set_symbols optionally set symbols for the conditions inside the PCA plot as a vector of numerics
#'
#' @return Downloads an output pdf file containing the generated plots.
#'
#' @export
#' @importFrom grDevices dev.off pdf svg
#'

plot_results <- function(lowest_level_df, exp_design, filename="", output_dir="", show_labels=F, svg=F, set_colors=NULL, set_symbols=NULL){
  data <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]

  # important: trim white space at start and end of users entry
  filename <- trimws(filename)
  output_dir <- trimws(output_dir)

  # file name and directory path
  if (filename == ""){
    f <- "results"
  }
  else {
    f <- filename
  }

  if (output_dir != ""){
    if(! dir.exists(output_dir)){
      dir.create(output_dir)
    }
    f <- paste0(output_dir, "/", f, sep="")
  }

  # PDF
  pdf(paste(f, "pdf", sep = "."), width = 10, height = 10)  # Set PDF-specific options
  plotStats(data, exp_design, show_labels=show_labels, pdf_mode=T,
            set_colors=set_colors, set_symbols=set_symbols)  # set here pdf_mode T
  dev.off()

  # SVG
  if(svg){
    svg_filename <- paste(f, "%02d.svg", sep="")  # to get multiple numbered files
    svg(svg_filename, width = 10, height = 10)  # Set SVG-specific options
    plotStats(data, exp_design, show_labels=show_labels, pdf_mode=F,
              set_colors=set_colors, set_symbols=set_symbols)  # set here pdf_mode F
    dev.off()
  }

}
