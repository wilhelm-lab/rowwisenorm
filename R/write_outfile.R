#' @title write_outfile
#'
#' @description
#' Download of the normalized values written into a file with comma-separated values.
#'
#' @param lowest_level_df lowest-level data frame
#' @param additional_cols optionally append the additional columns from the input file as a data frame
#' @param filename optionally specify the name of the output file
#' @param output_dir optionally specify the directory in which the output file will be saved
#'
#' @return Creates and downloads an output file
#'
#' @export
#' @importFrom utils write.table
#'

write_outfile <- function(lowest_level_df, additional_cols=NULL, filename="", output_dir=""){
  if (is.null(additional_cols)){
    if(trimws(filename) == "") file <- "output" else file <- trimws(filename)
    data <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]
  }
  else {
    if(trimws(filename) == "") file <- "output_complete" else file <- trimws(filename)

    # special case: if additional_cols only stores one single column: save this column name in a variable (because in next step, the additional_cols is no longer a data.frame and loses this column name)
    add_colname <- ""
    if (ncol(additional_cols) == 1){
      add_colname <- colnames(additional_cols)[1]
    }

    # reduce additional cols to the rows named in row.number column (those not filtered out)
    rows_to_keep <- lowest_level_df$row.number
    additional_cols <- additional_cols[rows_to_keep, ]  # special case: if additional_cols has only 1 column, after this step it is no longer a data.frame but gets converted into a vector here

    # special case: if additional_cols only stores one single column: back convert it to a data.frame and set the saved column name back
    if (!is.data.frame(additional_cols)){  # only the case when having exactly one column
      additional_cols <- data.frame(Column1 = additional_cols)
      colnames(additional_cols) <- add_colname
    }

    # merge
    data <- cbind(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"], additional_cols)
  }

  file <- paste(file, ".csv", sep = "")

  output_dir <- trimws(output_dir)
  if (output_dir != ""){
    if(! dir.exists(output_dir)){
      dir.create(output_dir)
    }
    file <- paste0(output_dir, "/", file, sep="")
  }

  write.table(data, file, row.names=F, col.names=T, sep=",")

}
