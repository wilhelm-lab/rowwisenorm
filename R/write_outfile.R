#' @title write_outfile
#'
#' @description
#' Write the normalized values into a file
#'
#' @param lowest_level_df lowest-level data frame
#' @param additional_cols append the additional columns from the input file
#' @param filename can be optionally specified
#'
#' @return Creating an output file
#'

write_outfile <- function(lowest_level_df, additional_cols=NULL, filename=""){
  if (is.null(additional_cols)){
    if(filename == "") file <- "output_rowwisenorm.csv" else file <- filename
    write.table(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"], file, row.names=F, col.names=T, sep=",")
  }
  else {
    if(filename == "") file <- "output_rowwisenorm_complete.csv" else file <- filename
    # reduce additional cols to the rows named in row.number column (those not filtered out)
    rows_to_keep <- lowest_level_df$row.number  # the row numbers mentioned in ID column
    additional_cols <- additional_cols[rows_to_keep, ]  # pick those desired rows
    # merge
    comb <- cbind(lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"], additional_cols)
    write.table(comb, file, row.names=F, col.names=T, sep=",")
  }
}
