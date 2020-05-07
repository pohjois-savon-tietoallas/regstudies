#' Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.
#'
#'
#' @param .data the data which contains the variables to be summed over.
#' @param ... the names of the variables to be summed over.
#'
#' @return returns scores summed over the given variables.
#' @examples
#'
#' # Lets calculate two different Elixhauser scores for a data set 'filtereddata'
#' elixhauser_classes <- classes_to_wide(vroom(file = "datas/classification_codes/elixhauser_classes_wide.csv"))
#' elixscore <- filtereddata %>%
#'   classify_data_long(icdcodes=CODE1,diag_tbl=elixhauser_classes) %>%
#'   sum_score(score_AHRQ,score_van_Walraven)
#'
sum_score <- function(.data,...) {
  vars<-enquos(...)
  # A bad code to extract names given by "..." (I could not figure out the better way):
  nimet <- .data %>% head(0) %>% select(!!! vars) %>% names()
  
  output <- .data %>%
    select(c("personid","classification","class",nimet)) %>%
    filter(!is.na(classification)) %>%
    distinct() %>%
    group_by(personid,classification) %>%
    summarise_at(nimet,sum,na.rm=T)
  names(output)[3:(3+length(nimet)-1)]<-paste("sum(",nimet,")",sep="")
  output
}