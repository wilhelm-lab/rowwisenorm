#' @title sum.normalize
#'
#' @description
#' Total sum normalization of the lowest-level data frame
#'
#' @param data lowest-level data frame
#' @param refFunc default is sum
#' @param norm scaling of values
#' @param na.rm tells whether NA values are removed
#'
#' @return scaled lowest-level data frame
#'

sum.normalize <- function(data, refFunc = sum, norm=T, na.rm=T) {
  data_id <- data$row.number
  data <- data[, !colnames(data) %in% "row.number"]  # exclude ID column

  tsums <- apply(data, 2, function(d) refFunc(d, na.rm=na.rm))  # total sums for each column summed up
  ts <- refFunc(tsums, na.rm=na.rm)  # total sum of complete data
  if (norm) {
    ts <- ts / dim(data)[2]
  }
  fs <- tsums/ts
  fsm <- matrix(rep(fs, each=dim(data)[1]), dim(data)[1], dim(data)[2])  # repeated for all rows so that same dimension

  data_n <- data * fsm

  data_n <- cbind(row.number = data_id, data_n)  # add ID column back
  return(data_n)
}
