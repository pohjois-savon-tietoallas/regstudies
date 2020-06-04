#' Function for extracting birthdate from finnish personal identification number (Henkilotunnus)
#'
#' The function is able to extract birthdate of a person from a finnish personal identification number.
#' 
#' @param hetu valid personal identification number (henkilotunnus)
#' @return lubridate::date indicating the birthdate
#' 
#' @importFrom dplyr case_when
#' @importFrom lubridate dmy
#' @importFrom stringr str_sub
#' 
#' @rdname get_birthdate
#' @export
#' 

get_birthdate <- function(hetu) {
  # TODO: regex-testi!
  #  spaiva <- stringr::str_sub(hetu,1,6)
  sign   <- stringr::str_sub(hetu,7,7)
  bdate <- paste0(stringr::str_sub(hetu,1,4),
                  dplyr::case_when(sign == "A" ~ "20",
                                   sign == "-" ~ "19",
                                   sign == "+" ~ "18"),
                  stringr::str_sub(hetu,5,6)
  )
  lubridate::dmy(bdate)
}