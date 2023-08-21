#' @title normalize_row
#'
#' @description
#' Row-wise normalization
#'
#' @param intensities lowest-level data frame
#' @param exp_design experimental design data frame
#' @param active set as TRUE for interactive mode
#' @param ref references can be directly set here
#' @param performTotalSum set as TRUE for total sum scaling
#' @param refFunc can be set as median or sum
#' @param na.rm tells whether NA values are removed
#'
#' @return row-wise normalized lowest-level data frame
#'

normalize_row <- function(intensities, exp_design, active=FALSE, ref=NULL, performTotalSum=F, refFunc=median, na.rm = TRUE){
  # list of conditions that can be used as references (present for each repeat in exp design)
  possible_refs <- c()
  for (i in 1:nrow(exp_design)){
    counter <- 0  # counts how many columns have a value for this row
    for(j in 1:ncol(exp_design)){
      if(exp_design[i,j] != ""){
        counter <- counter + 1
      }
    }
    if(counter == ncol(exp_design)){
      possible_refs <- append(possible_refs, exp_design[i,1])
    }
  }

  possible_refs <- unique(possible_refs)  # TODO remove? only for safety (should make no difference since it is already checked that each condition has exactly one row)

  # if empty, directly stop program
  if (length(possible_refs) == 0){
    stop("None of the conditions can be used as reference.")
  }

  if (active){
    # ask for reference
    cat("Do you have a reference channel? (y/n) \n")  # TODO maybe instead ask do you have a pool/shared sample?
    reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
    while(reference_boolean != "y" && reference_boolean != "n"){
      cat("Please answer with 'y' for yes or 'n' for no")
      reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
      reference_boolean <- tolower(reference_boolean)  # make case-insensitive
    }

    references <- NULL # default if user sets no references

    # if yes, get references
    if (reference_boolean == "y"){
      references <- c()

      reference <- ""
      while (! reference %in% possible_refs){
        cat("Please enter one of the possible references: ", possible_refs, "\n", sep = " ")
        reference <- scan(n = 1, what = character(), quiet = TRUE)
      }

      references <- append(references, reference)  # append this reference

      # ask for further references
      another <- TRUE
      while (another == TRUE){
        cat("Do you have another reference? (y/n) \n")
        reference_boolean <- scan(n = 1, what = character(), quiet = TRUE)
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
  else {
    references <- ref  # argument "ref" in function call, default is NULL
  }

  # total sum argument - doing before row-wise normalization
  if(performTotalSum){
    intensities <- sum.normalize(intensities, norm = F)
  }

  data_norm <- data.frame()
  if (! is.null(references)){  # reference was set
    # make references unique in case the same reference was two times entered
    references <- unique(references)
    # safety check: when refs are set by the user, check that they are possible
    for (refer in references){
      if (! refer %in% possible_refs){
        c <- paste(possible_refs, collapse = " ")
        m <- paste("One or more of the entered conditions is not a possible reference. Possible references are: ", c)
        stop(m)
      }
    }
    data_norm <- normalize_row_ref(intensities, exp_design, ref=references, refFunc = refFunc, na.rm = na.rm)
  }
  else {  # no reference set - automatically set all possible references
    data_norm <- normalize_row_ref(intensities, exp_design, ref=possible_refs, refFunc = refFunc, na.rm = na.rm)
  }

  # TODO total sum argument - doing after row-wise normalization - worse plot?
  # if(performTotalSum){
  #   data_norm <- sum.normalize(data_norm, norm = F)
  # }

  return(data_norm)  # normalized lowest level df
}
