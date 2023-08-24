#' @title plot_results
#'
#' @description
#' Function to be called to generate all plots and save them inside an output pdf file
#'
#' @param lowest_level_df lowest-level data frame
#' @param main title for the plots that is also included in the name of the output file
#' @param output_dir the directory in which the output file will be saved
#'
#' @return Creates an output pdf file containing the generated plots
#'

plot_results <- function(lowest_level_df, main="", output_dir="results"){
  intensities <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]

  # plots
  filename <- "results"
  if (main != ""){
    main_name <- gsub("\\s+", "_", main)  # _ instead of white space
    filename <- paste(filename, main_name, sep = "_")
  }
  filename <- paste(filename, ".pdf", sep = "")

  if(! dir.exists(output_dir)){
    dir.create(output_dir)
  }

  if (trimws(output_dir) != ""){
    filename <- paste0(output_dir, "/", filename, sep="")
  }

  pdf(filename, width=10, height=10)
  plotStats(intensities, main=main)
  dev.off()
}
