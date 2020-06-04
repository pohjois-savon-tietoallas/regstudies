#' Add zero to front of x and make x a character string
#'
#' Function for adding "0" to front of character dates for example.
#' 
#' @param x object for whome the "0" must be added in the beginning. Can be any object what can be converted to char.
#' @param desired_length integer of what length should the output be.
#' 
#' @return character string
#' 
#' @rdname generate_random_hetu
#' @export
#'
add_zero <- function(x,desired_length) {
  x <- as.character(x)
  ifelse(nchar(x) %% desired_length, paste0("0",x), x)
}