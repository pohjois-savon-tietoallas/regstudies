#' Get variable types
#'
#' This function operates as an tidyverse style version of `class()` function.
#'
#' @family help functions
#' @param .data tibble data to be used
#' @return returns a tibble with two character columns: variable and class.
#' variable tells the variable name and class tells the class of that variable.
#' 
#' @importFrom dplyr slice
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' 
#' @examples
#' \dontrun{
#' get_var_types(sample_cohort)
#' }
#' 
#' @rdname get_var_types
#' @export
#' 
get_var_types <- function(.data) {
  .data %>%
    dplyr::ungroup() %>%
    dplyr::slice(1) %>%
    dplyr::summarise_all(class) %>% 
    tidyr::pivot_longer(tidyselect::everything(),names_to="variable",values_to="class")
#  %>% 
#    tidyr::gather(variable, class)
}