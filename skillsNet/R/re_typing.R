
# Functions to reinstate the typing of a variable -------------------------

#' Retype
#'
#' @description Function to re-apply the internal typeing of the stored var
#'
#' @param char the formally typed character element
#'
#' @return
#' @export
#'
#' @examples
reType <- function(char) {
  split_char <- stringr::str_match(char, "^([a-zA-Z]{2})-(.*)")
  input_type <- split_char[,2]
  input_var <- split_char[,3]
  applyType(input_type)(input_var)
}

#' Type Application
#'
#' @param input_type The type prefix for the triplet type
#'
#' @return the formatter function to formally type the input var
#' @export
#'
#' @examples
applyType <- function(input_type){
  if(input_type == "nu"){
    as.numeric
  } else if(input_type == "da"){
    as.Date
  } else {
    function(input) input
  }

}
