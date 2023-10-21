#' @title plot_results
#'
#' @description
#' Function to be called to generate all plots and save them inside an output pdf file
#'
#' @param lowest_level_df lowest-level data frame
#' @param exp_design experimental design data frame
#' @param main optionally specify the name of the output file and title for the plots
#' @param output_dir optionally specify the directory in which the output file will be saved
#' @param show_labels states whether the labels for the data points are shown inside the PCA plot
#' @param svg states whether a svg file is created for each of the plots additionally
#'
#' @return Creates an output pdf file containing the generated plots
#'
#' @export
#' @importFrom grDevices dev.off pdf
#'

plot_results <- function(lowest_level_df, exp_design, main="", output_dir="", show_labels=F, svg=F){
  data <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]

  # important: trim white space at start and end of users entry
  main <- trimws(main)
  output_dir <- trimws(output_dir)

  # plots
  if (main == ""){
    filename <- "results"
  }
  else {
    filename <- main
  }

  if (output_dir != ""){
    if(! dir.exists(output_dir)){
      dir.create(output_dir)
    }
    filename <- paste0(output_dir, "/", filename, sep="")
  }

  # PDF
  pdf(paste(filename, "pdf", sep = "."), width = 10, height = 10)  # Set PDF-specific options
  plotStats(data, exp_design, main=main, show_labels=show_labels, pdf_mode=T)  # set here pdf_mode T
  dev.off()

  # SVG
  if(svg){
    svg_filename <- paste(filename, "%02d.svg", sep="")  # to get multiple files, e.g. "myplots%02d.svg"
    svg(svg_filename, width = 10, height = 10)  # Set SVG-specific options
    plotStats(data, exp_design, main=main, show_labels=show_labels, pdf_mode=F)  # set here pdf_mode F
    dev.off()
  }

  # * for only PDF
  # filename <- paste(filename, ".pdf", sep = "")  # TODO remove this when applying multiple file formats

  # pdf(filename, width=10, height=10)
  # plotStats(data, exp_design, main=main, show_labels=show_labels, pdf_mode=T)  # set here pdf_mode True for adjusted margins
  # dev.off()

}
