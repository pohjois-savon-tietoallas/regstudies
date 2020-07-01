#' Get nearest value based on datetime variable
#' 
#' This function helps user to find data rows which have datetime value of interest `date_var` closest to specified `centroid` value.
#' User can use argument `only_previous` to handle if nearest row can be only before or also after the `centroid` date.
#' 
#' @family date functions
#' @param .date data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @param centroid datetime or other comparable variable as reference point around which to search
#' @param only_previous optional parameter (default=FALSE): should we search only previously observed data prior centroid?
#' @return The row which is the nearest .date
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' # searching for the first event of the each class (for each individual):
#' d %>%
#'   group_by(personid) %>%
#'   get_nearest(adm_date,centroid=postingdate)
#' }
#' 
#' @rdname get_nearest
#' @export
#' 
get_nearest<-function(.data, date_var, centroid, only_previous=FALSE) {
  var_quo<-dplyr::enquo(date_var)
  cen_quo<-dplyr::enquo(centroid)
  if (only_previous) {
    .data %>%
      dplyr::filter((!!var_quo) <= (!!cen_quo)) %>%
      dplyr::slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
      return()
  }
  .date %>%
    dplyr::slice(which.min(abs((!!var_quo)-(!!cen_quo))))
}