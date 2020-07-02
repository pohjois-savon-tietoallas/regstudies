#' Replace NA values as default values based no the type of the variable
#' 
#' This function allows user to replace NA values based on type of the variable. 
#' For example NA in character columns can be replaced by "", logical NA values as FALSE, integer NA values as 0L and so on.
#' User need to give the list of the default values what he or she want to be used in replacing.
#' @family help functions
#' @param replace_na_by_type the list of default values for each type to be used
#' 
#' @rdname replace_na_by_type
#' @export
#' 
replace_na_by_type <- function(x,na_replace_list) {
  # TODO: Tee tarkistus, että listan "replace_by_type" nimet ovat sopivat!
  cl <- class(x)
  ifelse(is.na(x),
         na_replace_list[[cl]],
         x)
}