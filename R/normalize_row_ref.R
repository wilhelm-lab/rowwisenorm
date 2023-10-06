#' @title normalize_row_ref
#'
#' @description
#' Helper function to perform row-wise normalization
#'
#' @param lowest_level_df lowest-level data frame
#' @param exp_design experimental design data frame
#' @param ref references can be set here as Strings inside a vector
#' @param refFunc String that can be set as "median" or "sum"
#' @param na.rm Boolean that tells whether NA values are removed
#'
#' @return row-wise normalized lowest-level data frame
#'
#' @importFrom stats median na.omit
#'

normalize_row_ref <- function(lowest_level_df, exp_design, ref, refFunc="median", na.rm = TRUE){
  intensities <- lowest_level_df[, !colnames(lowest_level_df) %in% "row.number"]  # without ID column

  # setting the reference function (necessary)
  refFunc <- trimws(refFunc)
  refFunc <- tolower(refFunc)
  if(refFunc == "median"){
    refFunc <- median
  }
  else if(refFunc == "sum"){
    refFunc <- sum
  }
  else {
    stop("Please enter 'median' or 'sum' as refFunc.")
  }


  smeans <- matrix(NA, dim(intensities)[1], (ncol(exp_design)-1))
  # colnames(smeans) <- unique(repeats)
  # rownames(smeans) <- rownames(intensities)


  # indices of those rows in exp_design that correspond to a ref
  row_indices_refs <- which(exp_design[, 1] %in% ref)

  # 1: median or sum of refs within each batch
  for (i in 2:ncol(exp_design)) {
    j <- i-1
    cols <- exp_design[row_indices_refs, i] # column names for the current batch with condition being a ref
    cols <- gsub("\\s", ".", cols)  # replace white space with dots

    if (length(cols) > 1) {  # (otherwise only one ref, median must not be calculated)
      smeans[,j] <- apply(intensities[, cols], 1, function(d) refFunc(d, na.rm=na.rm))  # median or sum across refs per-batch (row-wise) -> smeans column 1 saves medians for refs of R1 (per row), column 2 for R2 etc
    } else {
      smeans[,j] <- intensities[, cols]
    }
  }

  # 2: median of refs across batches
  bmeans <- apply(smeans, 1, function(d) median(d, na.rm=na.rm))  # median of the medians or sums (row-wise)

  # 3+4: applying batch-wise ratio
  intensities_normalized <- intensities
  for (i in 2:ncol(exp_design)) {
    j <- i-1
    cols <- exp_design[, i]  # column names for respective batch
    cols <- gsub("\\s", ".", cols)  # replace white space with dots

    intensities_normalized[, cols] <- intensities[, cols]/(smeans[,j]/bmeans)
  }

  # add ID column back
  intensities_normalized <- cbind(row.number = lowest_level_df$row.number, intensities_normalized)
  return (intensities_normalized)
}
