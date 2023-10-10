#' @title read_files
#'
#' @description
#' Reading of the input files
#'
#' @param data data
#' @param design file of the experimental design
#' @param rm_only_by_site remove rows being positive for only by site
#' @param rm_reverse remove rows being positive for reverse
#' @param rm_contaminant remove rows being positive for a contaminant
#'
#' @return A list storing the lowest-level data frame, the data frame for the experimental design
#' and a data frame with the remaining columns if present in this exact order
#'
#' @export
#' @importFrom utils read.csv read.table
#' @importFrom stats median na.omit
#'

read_files <- function(data, design, rm_only_by_site=TRUE, rm_reverse=TRUE, rm_contaminant=TRUE){
  proteingroups <- data.frame()
  exp_design <- data.frame()

  if (grepl(".csv", data, fixed = TRUE)){
    proteingroups <- read.csv(data, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = "NaN")
  }
  else {
    proteingroups <- read.table(data, header = TRUE, sep = "\t", na.strings = "NaN")
  }
  exp_design <- read.table(design, header = FALSE, sep = "\t", na.strings = "NaN")

  # TODO added to replace NA values with "" for missing values (possibly empty fields were read in as NAs)
  exp_design[is.na(exp_design)] <- ""

  # TODO added Remove columns with only missing values (only white space for each row) (user added empty column by accident)
  exp_design <- exp_design[, !apply(exp_design, 2, function(x) all(grepl("^\\s*$", x)))]

  # sanity checks
  colnames_sub <- gsub("[.]", " ", colnames(proteingroups))  # column names without "."

  for (i in 1:nrow(exp_design)){
    cond <- trimws(exp_design[i,1])  # condition for this row
    if (cond == ""){
      stop("A condition name is missing for the first column of at least one row inside the experimental design.")
    }
    for (j in 2:ncol(exp_design)){
      entry <- trimws(exp_design[i,j])  # remove white space at start and end
      # proof that all mentioned column names are present in the data
      if (! (entry %in% colnames_sub | entry == "")){
        stop("The experimental design file does not match the column names of the data.")
      }
      # TODO removed another sanity check
    }
  }

  # sanity check: proof that only one row for each condition - otherwise perhaps wrongly assigning conditions as possible references in normalization
  if(length(exp_design[,1]) > length(unique(exp_design[,1]))){
    stop("At least one condition is assigned to more than one row.")
  }

  # filter data (proteingroups):
  if (rm_only_by_site){
    # search the column that contains "only by site" in any form in its name
    search_string <- "only by site"
    regex_pattern <- gsub("\\s+", ".*", search_string)  # so that also e.g. "only identified by site" matches

    matching_col <- grep(regex_pattern,
                         colnames(proteingroups),
                         value = TRUE,
                         ignore.case = TRUE,
                         perl = TRUE)  # perl allows pattern search

    # exclude the rows where a "+" is in this column
    if(length(matching_col) == 1){  # important: only if there is exactly one matching column
      matching_col <- matching_col[1]  # note: matching_col originally is a vector with the matching column name as an element inside
      proteingroups <- proteingroups[!proteingroups[[matching_col]] %in% c('+'),]
    }
    else {  # warning for the user that the desired filtering was not possible
      warning("There is no or more than one column present for the feature 'only by site'. No filtering was done for this feature.")
    }
  }
  if (rm_reverse){
    search_string <- "reverse"
    regex_pattern <- gsub("\\s+", ".*", search_string)

    reverse_col <- grep(regex_pattern,
                        colnames(proteingroups), value = TRUE, ignore.case = TRUE, perl = TRUE)

    # exclude the rows where a "+" is in this column
    if(length(reverse_col) == 1){
      reverse_col <- reverse_col[1]
      proteingroups <- proteingroups[!proteingroups[[reverse_col]] %in% c('+'),]
    }
    else {
      warning("There is no or more than one column present for the feature 'reverse'. No filtering was done for this feature.")
    }
  }
  if (rm_contaminant){
    search_string <- "contaminant"
    regex_pattern <- gsub("\\s+", ".*", search_string)  # so that also e.g. "potential contaminant" matches

    contaminant_col <- grep(regex_pattern,
                            colnames(proteingroups), value = TRUE, ignore.case = TRUE, perl = TRUE)

    # exclude the rows where a "+" is in this column
    if(length(contaminant_col) == 1){
      contaminant_col <- contaminant_col[1]
      proteingroups <- proteingroups[!proteingroups[[contaminant_col]] %in% c('+'),]
    }
    else {
      warning("There is no or more than one column present for the feature 'contaminant'. No filtering was done for this feature.")
    }
  }


  # set column names of exp_design df
  colnames(exp_design)[1] <- "design.conditions"
  for (i in 2:length(exp_design)){
    colnames(exp_design)[i] <- paste0("design.batch", i-1)  # TODO changed to design.batch instead design.repeat
  }

  # get the column names of the desired columns from experimental design
  exp_design_desired_names <- exp_design[, grep("design.batch", colnames(exp_design))]  # TODO changed to design.batch instead design.repeat
  desired_colnames <- c()
  for (i in 1:nrow(exp_design_desired_names)){
    for (j in 1:ncol(exp_design_desired_names)){
      entry <- trimws(exp_design_desired_names[i, j])
      if (entry != ""){
        desired_colnames <- append(desired_colnames, entry)
      }
    }
  }

  # reformat these column names
  desired_colnames_dots <- gsub(" ", ".", desired_colnames)

  # add "ID" column with the current row numbers to the data before further filtering out rows -> saves the current row numbers before lowest level df gets more filtered
  row_numbers <- seq_along(proteingroups[,1])
  proteingroups <- cbind(row.number = row_numbers, proteingroups)

  # lowest-level df
  lowest_df <- cbind(proteingroups["row.number"], proteingroups[, desired_colnames_dots])

  # replace all 0 values inside lowest-level df with NaN
  lowest_df[, !colnames(lowest_df) %in% "row.number"] <- replace(lowest_df[, !colnames(lowest_df) %in% "row.number"], lowest_df[, !colnames(lowest_df) %in% "row.number"] == 0, NaN)

  # all remaining columns if present
  additional_cols <- proteingroups[, ! colnames(proteingroups) %in% desired_colnames_dots]
  additional_cols <- additional_cols[, ! colnames(additional_cols) %in% c("row.number")]  # exclude row number ID, because additional_cols never gets modified and we already saved the current row numbers inside lowest_df

  # if additional_cols only stores one single column, it is no longer a data frame - convert to a data frame
  # (this is only for exactly 1 column - if additional_cols has 0 columns it still is a data.frame with ncol = 0)
  if (! is.data.frame(additional_cols)){
    additional_cols <- data.frame(Column1 = additional_cols)  # make it a data frame
    other_colnames <- c(desired_colnames_dots, "row.number")  # all the other column names
    add_colname_index <- ! colnames(proteingroups) %in% other_colnames  # index of the single column
    add_colname <- colnames(proteingroups[add_colname_index])  # the column name of the single column
    colnames(additional_cols) <- add_colname
  }

  return_list <- list("lowest_level_df" = lowest_df, "exp_design" = exp_design, "additional_cols" = additional_cols)
  return(return_list)
}

