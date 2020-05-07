#' Deprecated: Extends the original data with the classification table and makes wide data.
#'
#' Computes classification table and attaches it to original data. Attached data is wide format (many new variables).
#'
#' @param .data tibble of register data which we want to study
#' @param id personal identification number
#' @param icdcodes name of the variable holding ICD-codes
#' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   'regex' must hold a string with a regular expression defining classes.
#'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' @param return_binary returns 0 or 1 if set as TRUE, TRUE/FALSE if set up as FALSE
#' @return
#' @examples
#' # we calculate the table which can be used for classification.
#' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#'
classify_data <- function(.data,id,icdcodes,diag_tbl,return_binary=TRUE) {
  icdcodes_quo <- enquo(icdcodes)
  id_quo <- enquo(id)
  ctobj<-classify(.data=.data,icdcodes=!!icdcodes_quo,diag_tbl=diag_tbl,return_binary=return_binary) #classification table object'
  
  classification_name<-ctobj %>%
    select(classification) %>%
    distinct() %>% as.vector()
  nimet<-names(ctobj)
  nimet
  print(names(ctobj))
  # icdcodes = KOODI1
  # id = lomno1 # user needs to enter this currently!
  text<-c(as_label(icdcodes_quo),"icd")
  outdat<-.data %>%
    #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
    left_join(ctobj,by=text)
  outdat
}