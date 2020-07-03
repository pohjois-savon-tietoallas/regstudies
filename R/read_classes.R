#' Internal function for providing long classification definitions.
#'
#' Operates classification definitions from wide format to long format.
#' @family read functions
#' @param classdef This variable contains the data in the wide format. See the classifications defined in /datas/classification_codes/
#' @return returns a tibble of classification definition in long format (tidy data) as that is what the package needs in operation.
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
#'  class_definitions <- vroom(file = "data/elixhauser_classes.csv")
#'  class_definitions <- read_classes(class_def = class_definitions)
#'  head(class_definitions)
#'  }
#'  
#' @rdname read_classes
#' @export
#' 
read_classes <- function(class_def) {
  main        <- class_def %>% dplyr::select(tidyselect::contains("label"), tidyselect::contains("score")) %>% dplyr::distinct()
  dat_to_long <- class_def %>% dplyr::select(-contains("score"))
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
  
  classdef2 <- tidyr::pivot_longer(dat_to_long %>% dplyr::select(-tidyselect::all_of(pnim)), -all_of(names_class_and_label), names_to="icd", values_to="regex")
  if (nexcep>0) {
    classdef2rm <- tidyr::pivot_longer(dat_to_long %>% dplyr::select(-all_of(vnim)), -all_of(names_class_and_label), names_to="icd", values_to="regex.rm") %>%
      dplyr::mutate(icd=sub(pattern=".rm", "", x=icd))
    classdef2 <- dplyr::left_join(classdef2, classdef2rm)
  }
  classdef2 <- dplyr::right_join(main, classdef2)
  nimet2       <- c("icd",names_class_and_label,"regex")
  if(nexcep>0) {
    nimet2<-c(nimet2,"regex.rm")
  }
  classdef2 %>%
    select(tidyselect::contains(c("icd",names_class_and_label,"regex","score")))
}
