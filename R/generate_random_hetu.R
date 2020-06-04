#' Function for generating random finnish personal identification number (Henkilotunnus)
#'
#' The function is able to generate a nearly valid random finnish personal identification number.
#' 
#' @param birthdate this must be valid lubridate date between start of 1800 and end of 2000 centuries.
#' @return character string henkilotunnus
#' 
#' @importFrom dplyr case_when
#' @importFrom lubridate year
#' 
#' @rdname generate_random_hetu
#' @export
#' 
generate_random_hetu <- function(birthdate) {
  yr      <- lubridate::year(birthdate)
  century <- yr - (yr%%100)
  merkki  <- dplyr::case_when(
    century==1800 ~ "+",
    century==1900 ~ "-",
    century==2000 ~ "A"
  )
  nnn <- sample(1:1000,size=14,replace=T)
  l   <- sample(c(LETTERS,0:9),14)
  paste0(format(birthdate, format="%d%m%y"),merkki,regstudies::add_zero(nnn,3),l)
}