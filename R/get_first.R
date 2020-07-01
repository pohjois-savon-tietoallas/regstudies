#' Get first value based on datetime variable
#'
#' @family date functions
#' @param .data data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @return The row which is the first (by which.min) in the .date
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
#'   get_first(adm_date)
#' }
#' @rdname get_first
#' @export
#' 
get_first<-function(.data,var) {
  var_quo <- dplyr::enquo(var)
  .data %>%
    dplyr::slice(which.min(!!var_quo)) #NOTE: In case of zero events, there will be no result
}