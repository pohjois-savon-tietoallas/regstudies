#' Filtering of date interval data within study interval.
#' Can be used for filtering e.g. for hospital visits, which are date intervals (time periods).
#' The function finds rows for which `ival_var` overlaps with interval created based on `index_date`, `time_before` and `time_after`.
#' 
#' @family filter functions
#' @seealso \code{\link{filter_date_in_ival}} for date filtering by fixed interval
#' @param ival_var the interval to be filtered
#' @param index_date date or other lubridate variable as reference point around which to search
#' @param time_before the time before the index date what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' @param time_after the time after the index date (default is 0 days) what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' 
#' @return returns a tibble object with filtered rows
#' 
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' @importFrom tidyr %>%
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


#' Filtering of datetime data within a time interval.
#' The function finds rows for which `date_var` is within interval created based on `index_date`, `time_before` and `time_after`.
#' 
#' @family filter functions
#' @seealso \code{\link{filter_ival_olap_ival}} for date filtering by different intervals before and after the occurence
#' @param date_var 
#' @param index_date date or other lubridate variable as reference point around which to search
#' @param time_before the time before the index date what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' @param time_after the time after the index date (default is 0 days) what defines the start of filtering interval (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#'
#' @return returns a tibble object with filtered rows
#' 
#' @importFrom rlang enquo
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom lubridate %--%
#' @importFrom lubridate %within%
#' @importFrom tidyr %>%
#' 
#' @examples
#' \dontrun{
#' TODO:
#' x <- 1
#' }
#' @rdname filter_date_in_ival
#' @export
#' 
filter_date_in_ival <- function(.data,date_var,index_date,time_before=years(2),time_after=days(0)) {
  
  date_var  <- dplyr::enquo(date_var)
  index_date <- dplyr::enquo(index_date)
  time_before <- dplyr::enquo(time_before)
  time_after <- dplyr::enquo(time_after)
  
  .data %>%
    dplyr::mutate(ival= ((!!index_date)-!!time_before) %--% ((!!index_date)+!!time_after)) %>% # create an interval out of 'index_date'
    dplyr::filter(!! date_var %within% ival) %>%
    dplyr::select(-ival)
}


#' Get first value based on datetime variable
#'
#' @family date functions
#' @param .data data which contains datetime or other comparable variable
#' @param date_var datetime or other comparable variable
#' 
#' @return The row which is the first (by which.min) in the .date
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' @importFrom tidyr %>%
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
#' @importFrom tidyr %>%
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
#' @param index_date date or other lubridate variable as reference point around which to search
#' @param only_previous optional parameter (default=FALSE): should we search only previously observed data prior index_date?
#' @return The row which is the nearest .date
#' 
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' @importFrom dplyr filter
#' @importFrom tidyr %>%
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