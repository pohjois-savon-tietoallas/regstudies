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
  nnn <- regstudies::add_zero(sample(1:1000,size=14,replace=T),3)
  base<- format(birthdate, format="%d%m%y")
  l   <- get_hetu_last_digit(paste0(base,nnn)) #sample(c(LETTERS,0:9),14)
  paste0(base,merkki,nnn,l)
}
get_hetu_last_digit <- function(DDMMYYZZZ) {
  t <- as.numeric(DDMMYYZZZ)
  c(0:9,LETTERS[-26])[1+t%%31]
}
