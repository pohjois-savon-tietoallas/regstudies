#' left_join with fill option
#' 
#' Allows user to do left_join and fill missing NA values with 0 or other optional value depending on the type of variable.
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
#' @importFrom dplyr mutate_all
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' 
#' @examples
#' \dontrun{
#'
#' ## Give list of NA values
#' replace_list <- list(
#'   "logical"=FALSE,
#'   "character"="",
#'   "integer"=0L,
#'   "double"=0.0,
#'   "numeric"=0,
#'   "Date"=lubridate::dmy("01-01-1900")
#' )
#' d_na <- left_join_replace_na(sample_cohort,sample_regdata)
#' d_filled <- left_join_replace_na(sample_cohort,sample_regdata,replace_list)
#' 
#' c("d_na"=sum(is.na(d_na)),"d_filled"=sum(is.na(d_filled)))
#' }
#' 
#' @rdname left_join_replace_na
#' @export
#' 
left_join_replace_na <- function(x, y, na_replace_list,...) {
  # Tarkistus, että listan "replace_by_type" nimet ovat sopivat:
  out <- dplyr::left_join(x,y,...)
  found_types <- NULL
  if (is.list(na_replace_list)) {
    found_types <- 
      regstudies::get_var_types(out) %>%
      dplyr::select(class) %>% 
      dplyr::distinct() %>% 
      dplyr::pull()
  }

  if (! setsubset(found_types,names(na_replace_list)) ) {
    print("Names of 'na_replace_list' are wrong or missing!")
    print("NA values were not replaced.")
    return(out)
  }
  out %>%
    dplyr::mutate_all(regstudies::replace_na_by_type,na_replace_list)
}

# if set a is subset of b, return TRUE, otherwise FALSE
setsubset <- function(a,b) {
  base::setequal(base::intersect(a,b),a)
}

#' Replace NA values as default values based no the type of the variable
#' 
#' This function allows user to replace NA values based on type of the variable. 
#' For example NA in character columns can be replaced by "", logical NA values as FALSE, integer NA values as 0L and so on.
#' User need to give the list of the default values what he or she want to be used in replacing.
#' @family help functions
#' @param replace_na_by_type the list of default values for each type to be used
#' 
# @importFrom tidyr %>%
# @importFrom dplyr left_join
# @importFrom dplyr filter
# @importFrom dplyr pull
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
#'  # "Date"=lubridate::dmy("01-01-1900")
#' )
#' d <- left_join(sample_cohort,sample_regdata)
#' d %>%
#'   mutate_all(replace_na_by_type,na_replace_list)
#' }
#' 
# @rdname replace_na_by_type
# @export
# 
replace_na_by_type <- function(x,na_replace_list) {
  # NOTE: Unless we expose this function to users, we need to do testing here!
  #  types <- dplyr::left_join(regstudies::get_var_types(testdata),regstudies::get_na_vars(testdata)) %>% 
  #    dplyr::filter(contains_na) %>%
  #    dplyr::pull(class)
  #
  #  if( !setsubset(types,names(na_replace_list)) ) {
  #    print(
  #      paste("Error: default values are not set up for required types:",setdiff(types,names(na_replace_list)),sep=" ")
  #    )
  #  }
  cl <- class(x)
  ifelse(is.na(x),
         na_replace_list[[cl]],
         x)
}

#' Function for extracting names of variables which have any NA values
#'
#' @family help functions
#' @param .data data to be studied
#' @return returns a two-column tibble holding variable name as first column ('variable') and 'contains_na' as second column.
#'
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{
#'  d<-left_join(sample_cohort,sample_regdata)
#'  get_na_vars(d)
#' }
#' 
#' @rdname get_na_vars
#' @export
#'
get_na_vars <- function(.data) {
  .data %>%
    dplyr::ungroup() %>%
    dplyr::summarise_all(function(x) any(is.na(x))) %>% 
    pivot_longer(tidyselect::everything(),names_to="variable",values_to="contains_na")
}

