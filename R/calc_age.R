#' Function for calculating age at eventdate
#'
#' The function helps calculating the age of person at the eventdate
#' @family date functions
#' @param eventdate lubridate date when the age is needed
#' @param birthdate lubridate date of person's birthdate
#' @return 
#' 
#' @importFrom lubridate as.duration
#' 
# @rdname get_age
# @export
#' 
calc_age <- function(eventdate,birthdate,accuracy="years") {
  as.numeric(lubridate::as.duration(eventdate-birthdate),accuracy)
}