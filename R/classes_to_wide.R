#' Internal function for providing long classification definitions.
#'
#' Operates classification definitions from wide format to long format.
#'
#' @param sel_classes This variable contains the data in the wide format. See the classifications defined in /datas/classification_codes/
#' @return returns classification definintion in long format (tidy data) as that is what the package needs in operation.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr distinct
#' @importFrom tidyselect all_of
#' @importFrom tidyselect select
#' @importFrom tidyselect contains
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' dontrun{
#' sel_classes <- vroom(file = "data/classification_codes/elixhauser_classes_wide.csv")
#' sel_classes2 <- classes_to_wide(sel_classes = sel_classes)
#' head(sel_classes2)
#' }
#' @rdname classes_to_wide
#' @export
#' 
classes_to_wide <- function(sel_classes) {
  #require(tidyselect) # not needed anymore? Now uses tidyselect::contains
  main        <- sel_classes %>% select(classification, label, tidyselect::contains("score")) %>% dplyr::distinct()
  dat_to_long <- sel_classes %>% select(-contains("score"), -classification)
  nimet       <- setdiff(names(dat_to_long), c("class","label"))
  sel         <- grep(".rm", x=nimet) # names of exceptions!
  nexcep      <- length(sel) # how many exceptions
  vnim<-nimet
  pnim<-c()
  if(nexcep>0) {
    vnim        <- nimet[-sel] # varsinaiset nimet
    pnim        <- nimet[sel]  # poikkeusnimet
  }
  
  sel_classes2 <- tidyr::pivot_longer(dat_to_long %>% tidyselect::select(-tidyselect::all_of(pnim)), -c("class","label"), names_to="icd", values_to="regex")
  if (nexcep>0) {
    sel_classes2rm <- tidyr::pivot_longer(dat_to_long %>% tidyselect::select(-all_of(vnim)), -c("class","label"), names_to="icd", values_to="regex.rm") %>%
      dplyr::mutate(icd=sub(pattern=".rm", "", x=icd))
    sel_classes2 <- dplyr::left_join(sel_classes2, sel_classes2rm)
  }
  sel_classes2 <- dplyr::right_join(main, sel_classes2)
  nimet2       <- c("classification","icd","class","label","regex")
  if(nexcep>0) {
    nimet2<-c(nimet2,"regex.rm")
  }
  sel_classes2 %>%
    select(tidyselect::contains(c("classification","icd","class","label","regex","score")))
}
