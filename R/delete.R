#' Extends the original data with the classification table and keeps data in long format.
#'
#' Computes classification table and attaches it to original data. Data stays in long format.
#'
#' @param .data tibble of register data which we want to study
#' @param icdcodes name of the variable holding ICD-codes
#' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   'regex' must hold a string with a regular expression defining classes.
#'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' @return
#' 
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom stringr str_starts
#' 
#' @examples
#' \dontrun{
#' x<-1
#' }
#' 
#' @rdname classify_codes_wide
#' @export
#'
classify_codes_wide <- function(.data, icdcodes, diag_tbl, wide=TRUE) {
  icdcodes_quo <- rlang::enquo(icdcodes)
  ctobj <- regstudies::make_classify_table(.data=.data,icdcodes=!!icdcodes_quo,diag_tbl=diag_tbl,return_binary=FALSE) #classification table object'
  
  classification_name <- .data %>% regstudies::get_classification_name()
  nimet <- names(ctobj)
  
  if (wide) {
    ctobj <- pivot_wider(ctobj %>% select(-tidyselect::contains("label_")) %>% dplyr::filter(!is.na(!!icdcodes_quo)),
                         names_from=nimet[stringr::str_starts(nimet,"class_")], #TODO: Ei välttämättä ole class_elixhauser- nimistä muuttujaa!
                         values_from=match)
  }
  
  na_replace_list <- list(
    "logical"=FALSE,
    "character"=NA_character_,
    "integer"=0L,
    "numeric"=0,
    "Date"=lubridate::dmy("01-01-1900")
  )
  text <- c(rlang::as_label(icdcodes_quo),"icd")
  print(text)
  #return(ctobj)
  outdat <- .data %>%
    #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
    regstudies::left_join_replace_na(ctobj,# %>% dplyr::filter(match) %>% dplyr::select(-match),
                                     na_replace_list = na_replace_list,
                                     by=text) # ... arguments
  outdat
}
