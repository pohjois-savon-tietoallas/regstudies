#' Deprecated: Classify diagnosis codes (no exceptions)
#'
#' Computes classification table which can be attached to original data using left_join(). Does not utilise exceptions to class definitions via 'regex.rm'
#'
#' @param .data tibble of register data which we want to study
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
#' ostprekoh %>%
#'   left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm),by="lomno1") %>%
#'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>% # filtering the diagnosis codes which are in the interval for each individual!
#'   classify_long(icdcodes=KOODI1,diag_tbl=sel_classes)
#'
classify_no_rm <- function(.data,icdcodes,diag_tbl,return_binary=TRUE) {
  # .data: tibble from which we want to study
  # icdcodes: name of the variable holding ICD-codes
  # diag_tbl: tibble which holds the classification details: needs to have variables 'regex' and 'label'
  # 'regex' must hold a string with a regular expression defining classes.
  # 'label' defines the names of the variables of classes (comorbidity indicators)
  
  icdcodes <- enquo(icdcodes)
  if (!dplyr::setequal(intersect(c("regex","label"),names(diag_tbl)),c("regex","label"))) {
    print("Names of the diag_tbl are wrong. Needs to have 'regex' and 'label'.")
    lt<-diag_tbl # TODO: Throw an error or return some sensible object!
  } else {
    diag_tbl<-diag_tbl %>% select(regex,label) # regex.rm ei ole implementoitu! (viel?)
    codes<-.data %>% select(!! icdcodes) %>% distinct()
    cr<-crossing(codes,diag_tbl %>% select(regex))
    cr <- cr %>%
      left_join(diag_tbl,by="regex") %>%
      mutate(match=str_detect(!! icdcodes,regex))
    if(return_binary) {
      cr<-cr %>%
        mutate(match=as.integer(match))
    }
    lt <- pivot_wider(cr %>%
                        select(-regex),
                      names_from=label,
                      values_from=match)
  }
  lt
}