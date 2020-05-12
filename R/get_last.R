#' Get last value based on datetime variable
#'
#' @param .date data which contains datetime or other comparable variable
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
#'   group_by(lomno1,label) %>%
#'   get_last(tulopvm)
#' }
#' @rdname get_last
#' @export
#' 
get_last<-function(.date,var) {
  var_quo <- dplyr::enquo(var)
  .date %>%
    dplyr::slice(which.max(!!var_quo))
}