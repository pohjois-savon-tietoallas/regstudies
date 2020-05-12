#' Get first value based on datetime variable
#'
#' @param .date data which contains datetime or other comparable variable
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
#'   group_by(lomno1,label) %>%
#'   get_first(tulopvm)
#' }
#' @rdname get_first
#' @export
#' 
get_first<-function(.date,var) { # fill=NA
  var_quo <- dplyr::enquo(var)
  .date %>%
    dplyr::slice(which.min(!!var_quo)) #TODO: Jos ei ole yhtään tapahtumaa, niin ei tule mitään tulosta!
}