#' Get variable types
#'
#' This function operates as an tidyverse style version of class() function
#'
#' @family help functions
#' @param .data tibble data to be used
#' @return returns a tibble with two character columns: variable and class.
#' variable tells the variable name and class tells the type of that variable.
#' 
#' @rdname get_var_types
#' @export
#' 
get_var_types <- function(.data) {
  .data %>%
    ungroup() %>%
    head(1) %>%
    dplyr::summarise_all(class) %>% 
    tidyr::gather(variable, class)
}