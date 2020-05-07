#' Get last value based on datetime variable
#'
#' @param .date data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @return The row which is the first (by which.max) in the .date
#' @examples
#' # searching for the first event of the each class (for each individual):
#' dat %>%
#'   filter(!is.na(label)) %>%
#'   group_by(lomno1,label) %>%
#'   get_last(tulopvm)
#'
get_last<-function(.date,var) {
  var_quo<-enquo(var)
  .date %>%
    slice(which.max(!!var_quo))
}