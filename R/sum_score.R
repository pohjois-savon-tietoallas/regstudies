#' Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.
#'
#' @family classification functions
#' @param .data the data which contains the variables to be summed over.
#' @param ... the names of the variables to be summed over.
#'
#' @return returns scores summed over the given variables.
#' 
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise_at
#' @importFrom tidyselect all_of
#' @importFrom rlang enquos
#' 
#' @examples
#' \dontrun{
#' # Lets calculate two different Elixhauser scores for a data set 'filtered_d'
#' elixh_score <- filtered_d %>%
#'   classify_elixhauser(icdcodes=CODE1) %>%
#'   sum_score(score_AHRQ,score_van_Walraven)
#'   head(elixh_score)
#' }
#' @rdname sum_score
#' @export
#' 
sum_score <- function(.data,...) {
  vars<-rlang::enquos(...)
  # A bad code to extract names given by "..." (I could not figure out the better way):
  nimet <- .data %>% head(0) %>% dplyr::select(!!! vars) %>% names()
  
  left_variables    <- c("personid")
  left_var_contains <- c("class_")
  output <- .data %>%
    dplyr::select(personid,dplyr::contains("class_"),tidyselect::all_of(nimet)) %>%
#    dplyr::filter(!is.na(classification)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(personid) %>%#    dplyr::group_by(personid,classification) %>%
    dplyr::summarise_at(nimet,sum,na.rm=T)
  n <- length(left_variables)+length(left_var_contains)
  names(output)[n:(n+length(nimet)-1)]<-paste("sum(",nimet,")",sep="")
  output
}