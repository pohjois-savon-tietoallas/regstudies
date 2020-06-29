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
#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom dplyr select
#' 
#' 
#' @examples
#' \dontrun{
#' # we calculate the table which can be used for classification.
#' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' 
#' dat<-ostprekoh %>%
#' left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm,icd),by="lomno1") %>%
#'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>%
#'   classify_data_long(icdcodes=KOODI1,diag_tbl=sel_classes) %>%
#'   filter(match>0)
#' }
#' 
#' @rdname classify_codes
#' @export
#'
classify_codes <- function(.data, codes, diag_tbl) {
  icdcodes_quo <- rlang::enquo(codes)
  ctobj <- make_classify_table(.data=.data,codes=!!icdcodes_quo,diag_tbl=diag_tbl,return_binary=FALSE) #classification table object'
  
#  classification_name <- .data %>% get_classification_name()
  nimet <- names(ctobj)

  # codes = KOODI1
  # id = lomno1 # user needs to enter this currently!
  text <- c(rlang::as_label(icdcodes_quo),"icd")
  outdat <- .data %>%
    #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
    dplyr::left_join(ctobj %>% dplyr::filter(match) %>% dplyr::select(-match),by=text)
  outdat
}

