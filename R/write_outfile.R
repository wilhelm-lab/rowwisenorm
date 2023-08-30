#' @title write_outfile
#'
#' @description
#' Write the normalized values into a file
#'
#' @param lowest_level_df lowest-level data frame
#' @param additional_cols optionally append the additional columns from the input file
#' @param filename optionally specify the name of the output file
#' @param output_dir optionally specify the directory in which the output file will be saved
#'
#' @return Creating an output file
#'
#' @export
#' @importFrom utils write.table
#'

write_outfile <- function(lowest_level_df, additional_cols=NULL, filename="", output_dir=""){
  if (is.null(additional_cols)){
    if(trimws(filename) == "") file <- "output_rowwisenorm" else file <- trimws(filename)
    file <- paste(file, ".csv", sep = "")

    output_dir <- trimws(output_dir)
    if (output_dir != ""){
      if(! dir.exists(output_dir)){
        dir.create(output_dir)
      }
      file <- paste0(output_dir, "/", file, sep="")
    }

    write.table(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"], file, row.names=F, col.names=T, sep=",")
  }
  else {
    if(trimws(filename) == "") file <- "output_rowwisenorm_complete" else file <- trimws(filename)
    file <- paste(file, ".csv", sep = "")

    output_dir <- trimws(output_dir)
    if (output_dir != ""){
      if(! dir.exists(output_dir)){
        dir.create(output_dir)
      }
      file <- paste0(output_dir, "/", file, sep="")
    }

    # reduce additional cols to the rows named in row.number column (those not filtered out)
    rows_to_keep <- lowest_level_df$row.number
    additional_cols <- additional_cols[rows_to_keep, ]
    # merge
    comb <- cbind(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"], additional_cols)
    write.table(comb, file, row.names=F, col.names=T, sep=",")
  }
}
