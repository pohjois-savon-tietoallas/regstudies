#' Filtering of datetime data within interval.
#' 
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
#' #'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#'
#' dat <- ostprekoh %>%
#' left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm,icd),by="lomno1") %>%
#'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#'   filter_date(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm)
#' }
#' @rdname filter_date
#' @export
#' 
filter_date <- function(.data,indexdate,range=years(2),...) { #,datevars=c("tulopvm","lahtopvm")
  # .data: data to be used
  # indexdate: index date (which date variable is to be compared with register data)
  # range: the distance from index date (lubridate format, e.g years(1), weeks(10), days(20) etc.)
  # ...: variables which user wants to be used for filtering. May be any number (>1) of variables. If one variable is within interval, then that row is included in the data (Applies OR operator).
  indexdate <- dplyr::enquo(indexdate)
  datevars  <- dplyr::enquos(...)
  ndatevars <- length(datevars)
  
  #require(purrr) # TODO: not needed?
  fwithin <- function(var,ival) {
    expr((!! var) %within% ival)
  }
  vars_within_ival <- purrr::map(datevars, fwithin, ival=ival)
  
  .data %>% # join the datas
    dplyr::mutate(ival= ((!!indexdate)-range) %--% ((!!indexdate)+range)) %>% # 2. create an interval out of 'indexdate'
    dplyr::filter(!!!vars_within_ival) %>%# this works with arbirary number of date variables entered
    dplyr::select(-ival)
}
