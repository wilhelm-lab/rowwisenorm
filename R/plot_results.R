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
#'
#' @return Creates an output pdf file containing the generated plots
#'
#' @export
#' @importFrom grDevices dev.off pdf
#'

plot_results <- function(lowest_level_df, exp_design, main="", output_dir="", show_labels=T){
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

  # filename_pdf <- paste(filename, ".pdf", sep = "")
  # filename_svg <- paste(filename, ".svg", sep = "")

  if (output_dir != ""){
    if(! dir.exists(output_dir)){
      dir.create(output_dir)
    }
    filename <- paste0(output_dir, "/", filename, sep="")
    # filename_pdf <- paste0(output_dir, "/", filename_pdf, sep="")
    # filename_svg <- paste0(output_dir, "/", filename_svg, sep="")

  }

  # wanted_devices <- c("pdf", "svg")  # could also do png here
  #
  # plotStats(data, exp_design, main=main, show_labels=show_labels, legend_shift=legend_shift, pdf_mode=T)  # set here pdf_mode True for adjusted legends
  #
  # # Loop over all devices and copy the plots there
  # for (device in wanted_devices) {
  #   dev.copy(
  #     eval(parse(text = device)),
  #     paste(filename, device, sep = ".")
  #   )
  #   dev.off()
  # }

  pdf(filename, width=10, height=10)
  plotStats(data, exp_design, main=main, show_labels=show_labels, pdf_mode=T)  # set here pdf_mode True for adjusted margins
  dev.off()

  # svg(filename_svg, width = 10, height = 10)
  # plotStats(data, exp_design, main=main, show_labels=show_labels, legend_shift=legend_shift, pdf_mode=T)  # set here pdf_mode True for adjusted legends
  # dev.off()
}
