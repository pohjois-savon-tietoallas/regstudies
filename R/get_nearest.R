#' Get nearest value based on datetime variable
#'
#' @param .date data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @param centroid datetime or other comparable variable as reference point around which to search
#' @param only_previous optional parameter (default=FALSE): should we search only previously observed data prior centroid?
#' @return The row which is the nearest .date
#' @examples
#' # searching for the first event of the each class (for each individual):
#' dat %>%
#'   get_nearest(tulopvm,centroid=Postituspvm)
#'
get_nearest<-function(.date, var, centroid, only_previous=FALSE) {
  var_quo<-enquo(var)
  cen_quo<-enquo(centroid)
  if (only_previous) {
    .date %>%
      filter((!!var_quo) <= (!!cen_quo)) %>%
      slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
      return()
  } else {
    .date %>%
      slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
      return()
  }
}