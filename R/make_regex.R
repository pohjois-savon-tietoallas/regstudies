#' Make regural expression classifications from LIKE% classifications
#'
#' @family help functions
#' @param .data holds the data which has variable names given as 'classname' and 'diagnosis'. Must be convertible to a tibble.
#' @param classname name of the variable containing names/labels for the classes
#' @param diagnosis name of the variable containing strings in LIKE% -format
#' @param diagnosis.rm name of the variable defining exceptions of ICD-codes not to be included in the class
#' @param sep separator of different diagnoses in 'diagnosis'. String.
#'
#' @return returns a tibble with regular expression definitions
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' 
#' @examples 
#' \dontrun{
#' # We load classification here
#' excelfile <- readxl::read_excel("./datas/disease_classification.xlsx")
#' cl9 <- excelfile %>%
#'   make_regex(classname=Lyhenne,diagnosis=sairdiag_icd9,diagnosis.rm=sairdiagpl_icd9) %>%
#'   mutate(icd="icd9")
#' cl10 <- excelfile %>%
#'   make_regex(classname=Lyhenne,diagnosis=sairdiag,diagnosis.rm=sairdiagpl) %>%
#'   mutate(icd="icd10")
#' 
#' sel_classes<-bind_rows(cl9,cl10) %>%
#'   mutate(classification="own_definition")
#' }
#' @rdname make_regex
#' @export
#' 
make_regex <- function(.data,classname,diagnosis,diagnosis.rm,sep=", ") {
  .data = tibble::as_tibble(.data)
  diagnosis = rlang::enquo(diagnosis)
  diagnosis.rm = rlang::enquo(diagnosis.rm)
  nr<-dim(.data)[1]
  # lets transform diag.list and diag.list.rm to 'regex' format!
  diag.vec <- vector("character",nr)
  diag.vec.rm <- diag.vec
  for(l in 1:nr) {
    vec1 <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis)),", "))
    vec.rm <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis.rm)),", "))
    diag.vec[l]    = paste(paste0("^",gsub(pattern="%",replacement="",x=vec1)),collapse="|")
    diag.vec.rm[l] = paste(paste0("^",gsub(pattern="%",replacement="",x=vec.rm)),collapse="|")
  }
  diag.names <- .data$Lyhenne #TODO: .data %>% tidyselect::select(Lyhenne)
  # lets create a table 'diag_tbl' which holds diagnosis classification!
  diag_tbl <- tibble::tibble(regex=diag.vec,regex.rm=diag.vec.rm,label=diag.names) %>%
    dplyr::mutate(regex.rm=ifelse(regex.rm=="^NA",NA,regex.rm))
  diag_tbl
}

