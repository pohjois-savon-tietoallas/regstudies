#' Filtering of datetime data within a time range.
#' 
#' @family filter functions
#' @seealso \code{\link{filter_date_interval}} for date filtering by different intervals before and after the occurence
#' @param indexdate index date (which date variable is to be compared with register data)
#' @param range the distance from index date (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' @param ... datetime variables which need to be within the range. May be any number (>1) of variables.  If one variable is within interval, then that row is included in the data (Applies OR operator).
#'
#' @return returns a tibble object with filtered rows
#' 
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom purrr map
#' 
#' @examples
#' \dontrun{
#' TODO:
#' x <- 1
#' }
#' @rdname filter_date_range
#' @export
#' 
filter_date_range <- function(.data,indexdate,range=years(2),...) {
  
  indexdate <- dplyr::enquo(indexdate)
  datevars  <- dplyr::enquos(...)
  ndatevars <- length(datevars)
  
  fwithin <- function(var,ival) {
    expr((!! var) %within% ival)
  }
  vars_within_ival <- purrr::map(datevars, fwithin, ival=ival)
  
  .data %>% # join the datas
    dplyr::mutate(ival= ((!!indexdate)-range) %--% ((!!indexdate)+range)) %>% # 2. create an interval out of 'indexdate'
    dplyr::filter(!!!vars_within_ival) %>%# this works with arbitrary number of date variables entered
    dplyr::select(-ival)
}
