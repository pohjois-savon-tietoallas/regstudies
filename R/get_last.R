#' Get last value based on datetime variable
#'
#' @family date functions
#' @param .data data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @return The row which is the first (by which.max) in the .date
#' @examples
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' 
#' @examples
#' \dontrun{
#' # searching for the first event of the each class (for each individual):
#' dat %>%
#'   filter(!is.na(label)) %>%
#'   group_by(personid,label) %>%
#'   get_last(adm_date)
#' }
#' @rdname get_last
#' @export
#' 
get_last<-function(.data,var) {
  var_quo <- dplyr::enquo(var)
  .data %>%
    dplyr::slice(which.max(!!var_quo))
}