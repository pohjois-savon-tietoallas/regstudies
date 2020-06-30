#' left_join with fill option
#' 
#' Allows user to do left_join and fill missing NA values with 0 or other optional value
#' 
#' @family help functions
#' 
#' @param x tibble data to used as left in a join
#' @param y tibble data to used as right in a join
#' @param na_replace_list a list definitions of na replacement values for each type of variable (based on 'class' or 'get_var_types' functions)
#'
#' @return 
#' 
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' 
#' @examples
#' \dontrun{
#'
#' ## Give list of NA values
#' na_replace_list <- list(
#'   "logical"=FALSE,
#'   "character"="",
#'   "integer"=0L,
#'   "double"=0.0,
#'   "numeric"=0,
#'   "Date"=lubridate::dmy("01-01-1900")
#' )
#' testdata %>%
#'   mutate_all(replace_na_by_type,na_replace_list)
#' }
#' 
#' @rdname left_join_replace_na
#' @export
#' 
left_join_replace_na <- function(x, y, na_replace_list,...) {
  # Tarkistus, että listan "replace_by_type" nimet ovat sopivat:
  out <- left_join(x,y,...)
  found_types <- NULL
  if (is.list(na_replace_list)) {
    found_types <- 
      regstudies::get_var_types(out) %>% # TODO: Miksi tässä on testdata?
      dplyr::select(class) %>% 
      dplyr::distinct() %>% 
      dplyr::pull()
  }
  # if set a is subset of b, return TRUE, otherwise FALSE
  setsubset <- function(a,b) {
    setequal(intersect(a,b),a)
  }

  if (! setsubset(found_types,names(na_replace_list)) ) {
    print("Names of 'na_replace_list' are wrong or missing!")
    print("NA values were not replaced.")
    return(out)
  }
  out %>%
    mutate_all(regstudies::replace_na_by_type,na_replace_list)
}

