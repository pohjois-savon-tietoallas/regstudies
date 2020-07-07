#' Filtering of date interval data within study interval.
#' Can be used for filtering e.g. for hospital visits, which are date intervals (time periods).
#' 
#' @family filter functions
#' @seealso \code{\link{filter_date_in_ival}} for date filtering by fixed interval
#' @param index_date index date (which date variable is to be compared with register data)
#' @param time_before the time before the index date what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' @param time_after the time before the index date (default is 0 days) what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' @param admission_date date of admission as lubridate format
#' @param discharge_date date of discharge as lubridate format
#' 
#' @return returns a tibble object with filtered rows
#' 
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' 
#' @examples
#' \dontrun{
#' d <- dplyr::left_join(sample_cohort,sample_regdata)
#' filtered_d <- d %>% 
#'   filter_ival_olap_ival(ival_var = lubridate::interval(adm_date,disc_date),
#'                         index_date = postingdate,
#'                         time_before = years(2),
#'                         time_after = days(0))
#' head(filtered_d)
#' }
#' @rdname filter_ival_olap_ival
#' @export
#' 
filter_ival_olap_ival <- function(.data,
                                  ival_var, #=lubridate::interval(adm_date,disc_date)
                                  index_date,
                                  time_before=years(2),
                                  time_after=days(0)) {
  ival_var <- dplyr::enquo(ival_var)
  index_date <- dplyr::enquo(index_date)

  .data %>% # join the datas
    dplyr::mutate(study_interval= ((!!index_date)-time_before) %--% ((!!index_date)+time_after)) %>% # create an interval out of 'index_date'
    dplyr::filter(lubridate::int_overlaps(study_interval,!!ival_var)) %>%
    select(-study_interval)
}
# old version:
#filter_ival_olap_ival_old <- function(.data,index_date,
#                                      time_before=years(2),time_after=days(0),
#                                      admission_date,discharge_date) {
#  index_date <- dplyr::enquo(index_date)
#  admission_date <- dplyr::enquo(admission_date)
#  discharge_date <- dplyr::enquo(discharge_date)
#  
#  .data %>% # join the datas
#    dplyr::mutate(study_interval= ((!!index_date)-time_before) %--% ((!!index_date)+time_after)) %>% # 1. create an interval out of 'index_date'
#    dplyr::mutate(hosp_interval= (!!admission_date) %--% (!!discharge_date)) %>% # 2. create an interval out of hospitalisation period
#    dplyr::filter(int_overlaps(study_interval,hosp_interval)) %>%
#    select(-study_interval,-hosp_interval)
#}


#' Filtering of datetime data within a time range.
#' 
#' @family filter functions
#' @seealso \code{\link{filter_ival_olap_ival}} for date filtering by different intervals before and after the occurence
#' @param index_date index date (which date variable is to be compared with register data)
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
#' @importFrom lubridate interval
#' @importFrom lubridate %within%
#' 
#' @examples
#' \dontrun{
#' TODO:
#' x <- 1
#' }
#' @rdname filter_date_in_ival
#' @export
#' 
filter_date_in_ival <- function(.data,...,index_date,range=years(2)) {
  
  datevars  <- dplyr::enquos(...)
  index_date <- dplyr::enquo(index_date)
  ndatevars <- length(datevars)
  
  fwithin <- function(var,ival) {
    expr((!! var) %within% ival)
  }
  vars_within_ival <- purrr::map(datevars, fwithin, ival=ival)
  
  .data %>% # join the datas
    dplyr::mutate(ival= ((!!index_date)-range) %--% ((!!index_date)+range)) %>% # 2. create an interval out of 'index_date'
    dplyr::filter(!!!vars_within_ival) %>%# this works with arbitrary number of date variables entered
    dplyr::select(-ival)
}


#' Get first value based on datetime variable
#'
#' @family date functions
#' @param .data data which contains datetime or other comparable variable
#' @param date_var datetime or other comparable variable
#' @return The row which is the first (by which.min) in the .date
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' 
#' @examples
#' \dontrun{
#' # searching for the first event of the each class (for each individual):
#' dat %>%
#'   filter(!is.na(label)) %>%
#'   group_by(personid,label) %>%
#'   filter_date_first(adm_date)
#' }
#' @rdname filter_date_first
#' @export
#' 
filter_date_first<-function(.data,date_var) {
  date_var_quo <- dplyr::enquo(date_var)
  .data %>%
    dplyr::slice(which.min(!!date_var_quo)) #NOTE: In case of zero events, there will be no result
}

#' Get last value based on datetime variable
#'
#' @family date functions
#' @param .data data which contains datetime or other comparable variable
#' @param date_var datetime or other comparable variable
#' @return The row which is the first (by which.max) in the .date
#' @examples
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' 
#' @examples
#' \dontrun{
#' # searching for the first event of the each class (for each individual):
#' dat %>%
#'   filter(!is.na(label)) %>%
#'   group_by(personid,label) %>%
#'   filter_date_last(adm_date)
#' }
#' @rdname filter_date_last
#' @export
#' 
filter_date_last<-function(.data,date_var) {
  date_var_quo <- dplyr::enquo(date_var)
  .data %>%
    dplyr::slice(which.max(!!date_var_quo))
}

#' Get nearest value based on datetime variable
#' 
#' This function helps user to find data rows which have datetime value of interest `date_var` closest to specified `index_date` value.
#' User can use argument `only_previous` to handle if nearest row can be only before or also after the `index_date` date.
#' 
#' @family date functions
#' @param .date data which contains datetime or other comparable variable
#' @param var datetime or other comparable variable
#' @param index_date datetime or other comparable variable as reference point around which to search
#' @param only_previous optional parameter (default=FALSE): should we search only previously observed data prior index_date?
#' @return The row which is the nearest .date
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' # searching for the first event of the each class (for each individual):
#' d %>%
#'   group_by(personid) %>%
#'   filter_date_nearest(adm_date,index_date=postingdate)
#' }
#' 
#' @rdname filter_date_nearest
#' @export
#' 
filter_date_nearest<-function(.data, date_var, index_date, only_previous=FALSE) {
  var_quo<-dplyr::enquo(date_var)
  cen_quo<-dplyr::enquo(index_date) #cen = centroid
  if (only_previous) {
    .data %>%
      dplyr::filter((!!var_quo) <= (!!cen_quo)) %>%
      dplyr::slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
      return()
  }
  .date %>%
    dplyr::slice(which.min(abs((!!var_quo)-(!!cen_quo))))
}