
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
  if(is.na(input_type)){
    function(val) val
  } else if(input_type == "nu"){
    as.numeric
  } else if(input_type == "da"){
    as.Date
  } else {
    function(input) gsub("_", " ", input)
  }

}

#' Retype Vectorised
#'
#' @description Function to re-apply the internal typeing of the stored var
#'
#' @param char the formally typed character element
#'
#' @return
#' @export
#'
#' @examples
reTypeVect <- function(vect) {
  split_char <- stringr::str_match(vect, "^([a-zA-Z]{2})-(.*)")
  input_var <- split_char[,3]
  input_type <- split_char[,2]
  split_table <- as.matrix(table(input_type))
  common_type <- rownames(split_table)[which.max(split_table)]
  if(length(split_table) != 1) warning("There are multiple data types in the table")
  applyType(common_type)(input_var)
  }
