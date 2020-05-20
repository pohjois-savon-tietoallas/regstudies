#' Extends the original data with the classification table and keeps data in long format.
#' Computes classification table and attaches it to original data. Data stays in long format.
#'
#' @param .data tibble of register data which we want to study
#' @param icd_codes name of the variable holding ICD-codes
#' @return data frame
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
#' }
#' 
#' @rdname classify_elixhauser
#' @export

classify_elixhauser <- function(.data, icd_codes) {
  diag_tbl <- regstudies::read_classes_tibble(regstudies:::elixhauser_classes) ## TODO: is this the best way or should data be ready in good format?
  
  icdcodes_quo <- rlang::enquo(icd_codes)
  id_quo <- rlang::enquo(id)
  
  ctobj <- classify_long(.data = .data, 
                         icdcodes = !!icdcodes_quo,
                         diag_tbl = diag_tbl,
                         return_binary = FALSE) #classification table object'
  
  classification_name <- ctobj %>%
    dplyr::select(classification) %>%
    dplyr::distinct() %>% as.vector()
  nimet <- names(ctobj)
  
  # icdcodes = KOODI1
  # id = lomno1 # user needs to enter this currently!
  text <- c(rlang::as_label(icdcodes_quo),"icd")
  outdat <- .data %>%
    #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
    dplyr::left_join(ctobj %>% dplyr::filter(match) %>% dplyr::select(-match),by=text)
  
  return(outdat)
}

