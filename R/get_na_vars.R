#' Function for extracting names of variables which have any NA values
#'
#' @param .data data to be studied
#' @return returns a two-column tibble holding variable name as first column ('variable') and 'contains_na' as second column.
#'
get_na_vars <- function(.data) {
  .data %>%
    ungroup() %>%
    head(1) %>%
    dplyr::summarise_all(function(x) any(is.na(x))) %>% 
    tidyr::gather(variable, contains_na)
}