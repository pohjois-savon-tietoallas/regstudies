#' Filtering of hospitalisation interval data within study interval.
#' 
#' @param indexdate index date (which date variable is to be compared with register data)
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
#' 
#' @examples
#' \dontrun{
#' #'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#'
#' dat <- cohort %>%
#' left_join(reg %>% select(personid,CODE1,admissiondate,dischargedate,icd),by="personid") %>%
#'   filter_date_hosp(indexdate=postingdate,
#'                    time_before=years(2),time_after=years(2),
#'                    admission_date=admissiondate,
#'                    discharge_date=dischargedate) %>%
#'   select(-study_interval,-hosp_interval)
#' head(dat)
#' }
#' @rdname filter_date_hosp
#' @export
#' 
filter_date_hosp <- function(.data,indexdate,time_before=years(2),time_after=days(0),admission_date,discharge_date) {
  indexdate <- dplyr::enquo(indexdate)
  admission_date <- dplyr::enquo(admission_date)
  discharge_date <- dplyr::enquo(discharge_date)

  .data %>% # join the datas
    dplyr::mutate(study_interval= ((!!indexdate)-time_before) %--% ((!!indexdate)+time_after)) %>% # 1. create an interval out of 'indexdate'
    dplyr::mutate(hosp_interval= (!!admission_date) %--% (!!discharge_date)) %>% # 2. create an interval out of hospitalisation period
    dplyr::filter(int_overlaps(study_interval,hosp_interval))
}
