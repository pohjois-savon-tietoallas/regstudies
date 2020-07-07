#' Internal function for getting classification name out of classification tibbles
#' 
#' @family help functions
#' @param .data tibble of classification definition
#'   
#' @return Returns a string which tells the name of a classification
#' 
#' @importFrom stringr str_subset
#' @importFrom stringr str_replace
#' 
# @export
get_classification_name <- function(.data) {
  .data %>%
    names() %>%
    stringr::str_subset("class_") %>%
    stringr::str_replace("class_","")
}
