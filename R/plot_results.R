#' @title plot_results
#'
#' @description
#' Function to be called to generate all plots and save them inside an output pdf file
#'
#' @param lowest_level_df lowest-level data frame
#' @param main optionally specify the name of the output file and title for the plots
#' @param output_dir optionally specify the directory in which the output file will be saved
#'
#' @return Creates an output pdf file containing the generated plots
#'
#' @export
#'

plot_results <- function(lowest_level_df, main="", output_dir=""){
  intensities <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]

  # important: trim white space at start and end of users entry
  main <- trimws(main)
  output_dir <- trimws(output_dir)

  # plots
  if (main == ""){
    filename <- "result_rowwisenorm"
  }
  else {
    filename <- main
  }

  filename <- paste(filename, ".pdf", sep = "")

  if (output_dir != ""){
    if(! dir.exists(output_dir)){
      dir.create(output_dir)
    }
    filename <- paste0(output_dir, "/", filename, sep="")
  }

  pdf(filename, width=10, height=10)
  plotStats(intensities, main=main)
  dev.off()
}
