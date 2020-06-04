#' Function for creating key table out of personal identification number
#'
#' This functions allows user to create a key table, which maps henkilotunnus to rolling number (id)
#' The 'henkilotunnus' numbers will be mapped to integers starting from 1 to n.
#' 
#' @param .data data which contain henkilotunnus
#' @param hetu variable name contain henkilotunnus
#' @param first_id first number (integer) which  will starts mapping of henkilotunnus.
#' @param randomize if set to TRUE, the order of newly generated ids will be random.
#' 
#' @return tibble which contain henkilotunnus mapped to integer.
#' 
#' @rdname make_hetu_key
#' @export
#' 
make_hetu_key <- function(.data,hetu,first_id=1,randomize=FALSE) {
  hetu_enquo <- enquo(hetu)
  .data <- .data %>%
    select(!!hetu_enquo) %>%
    distinct() %>%
    mutate(id=row_number()+first_id-1)
  if (!randomize) {
    return(.data)
  }
  id_r <- .data %>%
    pull(id)
  d <- dim(.data)
  id_r <- sample(id_r,size = d[1],replace = F)
  .data$id <- id_r
  .data %>%
    arrange(id) # return arranged data
}
