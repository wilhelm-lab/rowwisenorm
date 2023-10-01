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

  # could do again the safety checks to validate the entry of refFunc (but: theoretically, any function can be set here)
  # and check if the refs inside ref are possible

  # TODO list of repeats: what if a repeat has a column without ANY condition?
  # e.g. LR1, LR3, LR4, MR1, MR2, MR3, HR1, HR3, HR4, R2 -> then R2 not covered in repeats() vector
  # or: what if column names do not contain the conditions at all? is that allowed?

  # list of repeats
  repeats <- c()
  for (cond in na.omit(exp_design$design.conditions)){
    # remove the condition (and if present the word intensity) from the column name containing the condition = the repeat
    words_to_remove <- c("Intensity", "intensity", cond)
    repeats <- append(repeats, gsub(paste(words_to_remove, collapse = "|"), "", colnames(intensities[, grepl(paste0("\\b", cond, "\\b"), colnames(intensities))])))  # only for exact match of the cond as a single word, not part of another word
  }
  repeats <- unique(repeats)
  repeats <- gsub("^\\.+|\\.+$", "", repeats)  # remove dots at beginning or end of string, e.g. "..R.1." turns to "R.1"

  # column names of the references
  ref_colnames <- c()
  for (reference in ref){
    ref_colnames <- append(ref_colnames, colnames(intensities[, grepl(paste0("\\b", reference, "\\b"), colnames(intensities))]))
  }

  smeans <- matrix(NA, dim(intensities)[1], length(repeats))
  colnames(smeans) <- unique(repeats)
  rownames(smeans) <- rownames(intensities)

  # 1: median or sum of refs within each batch
  for (i in 1:length(repeats)) {
    ref_colnames_rep <- c()  # column names with references AND the current batch/rep
    for (k in 1:length(ref_colnames)){
      if (grepl(repeats[i], ref_colnames[k])){
        ref_colnames_rep <- append(ref_colnames_rep, ref_colnames[k])
      }
    }
    if (length(ref_colnames_rep) > 1) {  # (otherwise only one ref, median must not be calculated)
      smeans[,i] <- apply(intensities[, ref_colnames_rep], 1, function(d) refFunc(d, na.rm=na.rm))  # median or sum across refs per-batch (row-wise) -> smeans column 1 saves medians for refs of R1 (per row), column 2 for R2 etc
    } else {
      smeans[,i] <- intensities[,ref_colnames_rep]
    }
  }

  # 2: median of refs across batches
  bmeans <- apply(smeans, 1, function(d) median(d, na.rm=na.rm))  # median of the medians or sums (row-wise)

  # 3+4: applying batch-wise ratio
  intensities_normalized <- intensities
  for (i in 1:length(repeats)) {
    cols <- grep(unique(repeats)[i], colnames(intensities))  # column indices with respective repeat
    intensities_normalized[, cols] <- intensities[, cols]/(smeans[,i]/bmeans)
  }

  # add ID column back
  intensities_normalized <- cbind(row.number = lowest_level_df$row.number, intensities_normalized)
  return (intensities_normalized)
}
