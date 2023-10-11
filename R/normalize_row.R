#' @title normalize_row
#'
#' @description
#' Row-wise normalization of the lowest-level data
#'
#' @param lowest_level_df lowest-level data frame
#' @param exp_design experimental design data frame
#' @param active Boolean that can be set as TRUE for interactive mode
#' @param ref references can be set here as Strings inside a vector
#' @param refFunc String that can be set as "median" or "sum"
#' @param na.rm Boolean that tells whether NA values are removed
#'
#' @return row-wise normalized lowest-level data frame
#'
#' @export
#' @importFrom stats median na.omit
#'

normalize_row <- function(lowest_level_df, exp_design, active=FALSE, ref=NULL, refFunc="median", na.rm = TRUE){
  # safety check
  refFunc <- trimws(refFunc)
  refFunc <- tolower(refFunc)
  if(! (refFunc == "median" | refFunc == "sum")){
    stop("Please enter 'median' or 'sum' as refFunc.")
  }

  # list of conditions that can be used as references (present for each batch in exp design)
  possible_refs <- c()
  for (i in 1:nrow(exp_design)){
    counter <- 0  # counts how many columns have a value for this row
    for(j in 1:ncol(exp_design)){
      if(trimws(exp_design[i,j]) != ""){
        counter <- counter + 1
      }
    }
    if(counter == ncol(exp_design)){
      possible_refs <- append(possible_refs, trimws(exp_design[i,1]))
    }
  }
  possible_refs <- unique(possible_refs)  # safety
  possible_refs <- trimws(possible_refs)  # safety

  # if empty, skip normalization
  pos_refs_present <- TRUE   # using a Boolean to avoid using stop() here (stop is not recommended for shiny)
  if (length(possible_refs) == 0){
    pos_refs_present <- FALSE
    warning("None of the conditions can be used as reference. No normalization was carried out.")
  }

  data_norm <- data.frame()

  # only go on if possible refs are present - otherwise nothing is done anymore
  if(pos_refs_present){

    if (active){
      # ask for shared sample/pool
      cat("Do you have a shared sample? (y/n) \n")
      reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
      reference_boolean <- tolower(reference_boolean)  # make case-insensitive
      while(reference_boolean != "y" && reference_boolean != "n"){
        cat("Please answer with 'y' for yes or 'n' for no")
        reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
        reference_boolean <- tolower(reference_boolean)  # make case-insensitive
      }

      references <- NULL # default for negative case if user sets no shared samples

      # if yes, get the shared samples as references
      if (reference_boolean == "y"){
        references <- c()

        reference <- ""
        while (! reference %in% possible_refs){
          cat("Please enter one of the possible references: ", possible_refs, "\n", sep = " ")
          reference <- scan(n = 1, what = character(), quiet = TRUE)
        }

        references <- append(references, reference)  # append this reference (shared sample)

        # ask for further shared samples
        another <- TRUE
        while (another == TRUE){
          cat("Do you have another shared sample? (y/n) \n")
          reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
          reference_boolean <- tolower(reference_boolean)  # make case-insensitive
          while(reference_boolean != "y" && reference_boolean != "n"){
            cat("Please answer with 'y' for yes or 'n' for no")
            reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
            reference_boolean <- tolower(reference_boolean)  # make case-insensitive
          }
          if(reference_boolean == "y"){
            reference <- ""
            while (! reference %in% possible_refs){
              cat("Please enter one of the possible references: ", possible_refs, "\n", sep = " ")
              reference <- scan(n = 1, what = character(), quiet = TRUE)
            }
            references <- append(references, reference)  # append this reference
          }
          else {
            another <- FALSE
          }
        }

      }
    }
    else {  # passive mode
      references <- ref  # argument "ref" in function call, default is NULL
    }

    if (! is.null(references)){  # reference(s) (shared sample(s)) was set
      references <- unique(references)  # make unique in case the same reference was two times entered
      references <- trimws(references)  # remove white space at start and end
      # safety check: when refs are set by the user, check that they are possible
      refs_ok <- TRUE
      for (refer in references){
        if (! refer %in% possible_refs){
          refs_ok <- FALSE
          c <- paste(possible_refs, collapse = ", ")
          m <- paste("One or more of the entered conditions is not a possible reference. Possible references are: ", c, " \n No normalization was done.")
          warning(m)
        }
      }
      if(refs_ok){  # normalization only if all entered refs are possible
        data_norm <- normalize_row_ref(lowest_level_df = lowest_level_df, exp_design = exp_design, ref=references, refFunc = refFunc, na.rm = na.rm)
      }
    }
    else {  # no reference(s) (shared sample(s)) set - automatically use all possible references
      data_norm <- normalize_row_ref(lowest_level_df = lowest_level_df, exp_design = exp_design, ref=possible_refs, refFunc = refFunc, na.rm = na.rm)
    }

  }

  return(data_norm)  # normalized lowest level df
}
