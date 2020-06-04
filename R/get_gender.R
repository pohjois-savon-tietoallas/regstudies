#' Function for extracting gender from finnish personal identification number (Henkilotunnus)
#'
#' The function is able to extract gender of a person from a finnish personal identification number.
#' 
#' @param hetu valid personal identification number (henkilotunnus)
#' @return factor object indicating gender of person
#' 
#' @importFrom stringr str_sub
#' 
#' @rdname get_gender
#' @export
#' 
get_gender <- function(hetu) {
  nnn <- stringr::str_sub(hetu,8,10)
  sex <- as.integer(nnn) %% 2
  factor(sex,levels = c(0,1),labels=c("Female","Male"))
}
