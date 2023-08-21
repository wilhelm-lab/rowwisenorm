#' @title read_files
#'
#' @description
#' Reading of the input files
#'
#' @param file1 proteinGroups.txt file
#' @param file2 file of the experimental design
#' @param rm_only_by_site remove rows being positive for only by site
#' @param rm_reverse remove rows being positive for reverse
#' @param rm_contaminant remove rows being positive for a contaminant
#'
#' @return A list storing the lowest-level data frame, the data frame for the experimental design
#' and a data frame with the remaining columns if present
#'
#'

read_files <- function(file1, file2, rm_only_by_site=TRUE, rm_reverse=TRUE, rm_contaminant=TRUE){
  proteingroups <- data.frame()
  exp_design <- data.frame()

  if (grepl("proteinGroups", file1, fixed = TRUE) | grepl("proteingroups", file1, fixed = TRUE)){
    if (grepl(".csv", file1, fixed = TRUE)){
      proteingroups <- read.csv2(file1, header = TRUE, na.strings = "NaN")
    }
    else {
      proteingroups <- read.table(file1, header = TRUE, sep = "\t", na.strings = "NaN")
    }
    exp_design <- read.table(file2, header = FALSE, sep = "\t", na.strings = "NaN")
  }
  else if (grepl("proteinGroups", file2, fixed = TRUE) | grepl("proteingroups", file2, fixed = TRUE)){
    if (grepl(".csv", file2, fixed = TRUE)){
      proteingroups <- read.csv2(file2, header = TRUE, na.strings = "NaN")
    }
    else {
      proteingroups <- read.table(file2, header = TRUE, sep = "\t", na.strings = "NaN")
    }
    exp_design <- read.table(file1, header = FALSE, sep = "\t", na.strings = "NaN")
  }
  else {
    stop("Please enter the path for proteinGroups.txt and the path for the experimental design tsv.")
  }

  # sanity check
  intensity_columns_pg <- grep("LFQ Intensity|Intensity", colnames(proteingroups), value=TRUE)  # intensity columns inside proteingroups
  intensity_columns_pg_sub <- gsub("[.]", " ", intensity_columns_pg)  # without "."
  for (i in 1:nrow(exp_design)){
    for (j in 2:ncol(exp_design)){
      if (! (exp_design[i,j] %in% intensity_columns_pg_sub | exp_design[i,j] == "")){
        stop("The experimental design file does not match the column names of proteinGroups.txt")
      }
    }
  }

  # sanity check: proof that only one row for each condition - otherwise perhaps wrongly taking possible references in normalization
  if(length(exp_design[,1]) > length(unique(exp_design[,1]))){
    stop("At least one condition is assigned to more than one row.")
  }

  # filter proteingroups:
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
    proteingroups <- proteingroups[!proteingroups[[matching_col]] %in% c('+'),]
  }
  if (rm_reverse){
    search_string <- "reverse"
    regex_pattern <- gsub("\\s+", ".*", search_string)

    reverse_col <- grep(regex_pattern,
                        colnames(proteingroups), value = TRUE, ignore.case = TRUE, perl = TRUE)

    # exclude the rows where a "+" is in this column
    proteingroups <- proteingroups[!proteingroups[[reverse_col]] %in% c('+'),]
  }
  if (rm_contaminant){
    search_string <- "contaminant"
    regex_pattern <- gsub("\\s+", ".*", search_string)  # so that also e.g. "potential contaminant" matches

    contaminant_col <- grep(regex_pattern,
                            colnames(proteingroups), value = TRUE, ignore.case = TRUE, perl = TRUE)

    # exclude the rows where a "+" is in this column
    proteingroups <- proteingroups[!proteingroups[[contaminant_col]] %in% c('+'),]
  }


  # replace all 0 values inside intensities with NaNs
  proteingroups[, grepl("Intensity", colnames(proteingroups))] <- replace(proteingroups[, grepl("Intensity", colnames(proteingroups))], proteingroups[, grepl("Intensity", colnames(proteingroups))] == 0, NaN)


  # set column names of exp_design df
  colnames(exp_design)[1] <- "design.conditions"
  for (i in 2:length(exp_design)){
    colnames(exp_design)[i] <- paste0("design.repeat", i-1)
  }

  # get the column names of the desired intensity columns from experimental design
  exp_design_intensity_names <- exp_design[, grep("design.repeat", colnames(exp_design))]
  exp_design_intensity_names <- na.omit(exp_design_intensity_names)  # remove NA if present
  intensity_colnames <- c()
  for (i in 1:nrow(exp_design_intensity_names)){
    for (j in 1:ncol(exp_design_intensity_names)){
      if(exp_design_intensity_names[i, j] != ""){  # when not missing
        intensity_colnames <- append(intensity_colnames, exp_design_intensity_names[i, j])
      }
    }
  }

  # reformat these column names
  intensity_colnames_dots <- gsub(" ", ".", intensity_colnames)

  # add "ID" column with the current row numbers to the proteingroups file before further filtering out rows -> saves the current row numbers before lowest level df gets more filtered
  row_numbers <- seq_along(proteingroups[,1])
  proteingroups <- cbind(row.number = row_numbers, proteingroups)

  intensity_df <- cbind(proteingroups["row.number"], proteingroups[, intensity_colnames_dots])
  additional_cols <- proteingroups[, ! colnames(proteingroups) %in% intensity_colnames_dots]
  additional_cols <- additional_cols[, ! colnames(additional_cols) %in% c("row.number")]  # exclude row number ID, because additional_cols never gets modified and we already saved the current row numbers inside the intensity_df

  return_list <- list("lowest_level_df" = intensity_df, "exp_design" = exp_design, "additional_cols" = additional_cols)
  return(return_list)
}
