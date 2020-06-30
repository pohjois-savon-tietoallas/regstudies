#' Internal function for providing long classification definitions.
#'
#' Operates classification definitions from wide format to long format.
#' @family help functions
#' @param sel_classes This variable contains the data in the wide format. See the classifications defined in /datas/classification_codes/
#' @return returns classification definintion in long format (tidy data) as that is what the package needs in operation.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr distinct
#' @importFrom tidyselect all_of
#' @importFrom dplyr select
#' @importFrom tidyselect contains
#' @importFrom tidyr pivot_longer
#'
#' @examples
#'  \dontrun{
#'  sel_classes <- vroom(file = "data/classification_codes/elixhauser_classes_wide.csv")
#'  sel_classes2 <- read_classes_tibble(sel_classes = sel_classes)
#'  head(sel_classes2)
#'  }
#'  
#' @rdname read_classes_tibble
#' @export
#' 
read_classes_tibble <- function(sel_classes) {
  main        <- sel_classes %>% dplyr::select(tidyselect::contains("label"), tidyselect::contains("score")) %>% dplyr::distinct()
  dat_to_long <- sel_classes %>% dplyr::select(-contains("score"))
  names_class_and_label <- names(dat_to_long)[grep("class|label", names(dat_to_long))]
  nimet       <- setdiff(names(dat_to_long), names_class_and_label)
  sel         <- grep(".rm", x=nimet) # names of exceptions!
  nexcep      <- length(sel) # how many exceptions
  vnim <- nimet
  pnim <- c()
  if(nexcep>0) {
    vnim <- nimet[-sel] # varsinaiset nimet
    pnim <- nimet[sel]  # poikkeusnimet
  }
  
  sel_classes2 <- tidyr::pivot_longer(dat_to_long %>% dplyr::select(-tidyselect::all_of(pnim)), -all_of(names_class_and_label), names_to="icd", values_to="regex")
  if (nexcep>0) {
    sel_classes2rm <- tidyr::pivot_longer(dat_to_long %>% dplyr::select(-all_of(vnim)), -all_of(names_class_and_label), names_to="icd", values_to="regex.rm") %>%
      dplyr::mutate(icd=sub(pattern=".rm", "", x=icd))
    sel_classes2 <- dplyr::left_join(sel_classes2, sel_classes2rm)
  }
  sel_classes2 <- dplyr::right_join(main, sel_classes2)
  nimet2       <- c("icd",names_class_and_label,"regex")
  if(nexcep>0) {
    nimet2<-c(nimet2,"regex.rm")
  }
  sel_classes2 %>%
    select(tidyselect::contains(c("icd",names_class_and_label,"regex","score")))
}
