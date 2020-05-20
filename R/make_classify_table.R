#' Classify diagnosis codes to long format with exemptions
#'
#' Computes classification table which can be attached to original data using left_join().
#'
#' @param .data tibble of register data which we want to study
#' @param icdcodes name of the variable holding ICD-codes
#' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   'regex' must hold a string with a regular expression defining classes.
#'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#'   
#' @return Returns a tibble containing classification table which can be joined to initial data
#' 
#' @importFrom dplyr setequal
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom stringr str_detect
#' @importFrom tidyr crossing
#' @importFrom tidyselect contains
#' @importFrom dplyr select 
#' 
#' @examples
#' \dontrun{
#' # we calculate the table which can be used for classification.
#' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' ostprekoh %>%
#'   left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm),by="lomno1") %>%
#'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#'   filter_date(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>% # filtering the diagnosis codes which are in the interval for each individual!
#'   classify_long(icdcodes=KOODI1,diag_tbl=sel_classes)
#' }
#' @rdname make_classify_table
#' @export
#' 
make_classify_table <- function(.data,icdcodes,diag_tbl,return_binary=TRUE) {
  # .data: tibble from which we want to study
  # icdcodes: name of the variable holding ICD-codes (you can use any type of string codes but change your classification definitions according to that)
  # diag_tbl: tibble which holds the classification details: needs to have variables 'regex' and 'label'
  # 'regex' must hold a string with a regular expression defining classes.
  # 'regex.rm' is optional, defines exceptions to 'regex'
  # 'label' defines the names of the variables of classes (comorbidity indicators)
  
  icdcodes <- rlang::enquo(icdcodes)
  classification_name <- .data %>% get_classification_name()
  if (!dplyr::setequal(intersect(c("regex","label"),str_sub(names(diag_tbl),1,5)),c("regex","label"))) {
    print("Names of the diag_tbl are wrong. Need to have 'regex' and 'label'.")
    lt <- diag_tbl # TODO: Throw an error or return some sensible object!
  } else {
    #diag_tbl<-diag_tbl %>% select(regex,regex.rm,label) # regex.rm ei ole implementoitu! (viel?)
    codes <- .data %>% 
      dplyr::select(!! icdcodes) %>% 
      dplyr::distinct()
    cr <- tidyr::crossing(codes,diag_tbl %>% 
                            dplyr::select(regex)
                          )
    cr <- cr %>%
      dplyr::left_join(diag_tbl,by="regex") %>%
      dplyr::mutate(match.yes=stringr::str_detect(!! icdcodes,regex))
    if(is.element("regex.rm",names(diag_tbl))) {
      print("Element 'regex.rm' in use. Taking exceptions in use.")
      cr <- cr %>%
        dplyr::mutate(match.rm=stringr::str_detect(!! icdcodes,regex.rm),
               match.rm=ifelse(is.na(match.rm),FALSE,match.rm)
        )
    } else {
      print("Element 'regex.rm' NOT in use. Exceptions omitted.")
      cr <- cr %>%
        dplyr::mutate(match.rm = FALSE,regex.rm=NA)
    }
    cr <- cr %>%
      dplyr::mutate(match = match.yes & !match.rm) %>%
      dplyr::select(-match.yes,-match.rm)
    #print(cr)# %>% filter(!is.na(match.rm) & match.rm))
    if(return_binary) {
      cr <- cr %>%
        dplyr::mutate(match=as.integer(match))
    }
#    classification_name <- diag_tbl %>%
#      dplyr::select(classification) %>%
#      dplyr::distinct() %>% as.vector()
    #    print(paste0("Using classification '",classification_name,"'")) # TODO: Ei ole tarkastettu, ett? on annettu vain yksi luokittelu
    lt <- cr %>%
      dplyr::select(-tidyselect::contains("regex")) #%>%
    #mutate(label=paste0(substr(classification_name,1,5),"_",label))
    #    lt <- pivot_wider(lt,
    #                      names_from=label,
    #                      values_from=match)
  }

  findnames <- function(name) {is.element(name,c("label","class"));}
  newnames <- function(name,classification_name) { paste0(name,"_",classification_name); }
  make_new_names <- function(.names,clname) {
    ifelse(findnames(.names),
           newnames(.names,clname),
           .names)
  }
  names(lt) <- names(lt) %>% make_new_names(classification_name)
  lt #%>%
    #rename_at(vars(class,label),funs(newnames,classification_name)) # %>% 
#    select(-classification)
}


