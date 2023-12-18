#' @title remove_common_prefix
#'
#' @description
#' Helper function to remove the common prefix of a set of Strings.
#'
#' @param strings vector of Strings
#'
#' @return List of the common prefix and the set of strings without the common prefix.
#'

remove_common_prefix <- function(strings){
  if (length(strings) == 0) {
    return("")
  }
  char_matrix <- strsplit(strings, NULL)  # list of lists, storing the single characters of the strings
  min_length <- min(sapply(char_matrix, length))   # minimum length of the strings

  # compare characters one by one, across strings
  for (i in 1:min_length) {
    if (length(unique(sapply(char_matrix, `[`, i))) > 1) {
      break  # if characters are not the same at this index, the common prefix ends at the position before
    }
  }

  common_prefix <- paste0(char_matrix[[1]][seq_len(i - 1)], collapse = "")    # combined common prefix
  modified_strings <- sub(paste0("^", common_prefix), "", strings)  # remove the common prefix

  return(list(common_prefix = common_prefix, strings_without_prefix = modified_strings))
}
