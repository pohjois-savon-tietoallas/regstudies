#' Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.
#'
#'
#' @param .data the data which contains the variables to be summed over.
#' @param ... the names of the variables to be summed over.
#'
#' @return returns scores summed over the given variables.
#' 
#' @importFrom rlang enquos
#' @importFrom tidyselect select
#' @importFrom dplyr filter %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_at
#' 
#' @examples
#' \dontrun{
#' # Lets calculate two different Elixhauser scores for a data set 'filtereddata'
#' elixhauser_classes <- classes_to_wide(vroom(file = "data/classification_codes/elixhauser_classes_wide.csv"))
#' elixscore <- filtereddata %>%
#'   classify_data_long(icdcodes=CODE1,diag_tbl=elixhauser_classes) %>%
#'   sum_score(score_AHRQ,score_van_Walraven)
#' }
#' @rdname sum_score
#' @export
#' 
sum_score <- function(.data,...) {
  vars<-rlang::enquos(...)
  # A bad code to extract names given by "..." (I could not figure out the better way):
  nimet <- .data %>% head(0) %>% tidyselect::select(!!! vars) %>% names()
  
  output <- .data %>%
    tidyselect::select(c("personid","classification","class",nimet)) %>%
    dplyr::filter(!is.na(classification)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(personid,classification) %>%
    dplyr::summarise_at(nimet,sum,na.rm=T)
  names(output)[3:(3+length(nimet)-1)]<-paste("sum(",nimet,")",sep="")
  output
}