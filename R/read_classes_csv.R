#' Read classes definitions from csv file
#'
#' Reads classes definitions based on the file path and operates classification definitions from wide format to long format.
#'
#' @param file the filename or path to the filename as a character string.
#' 
#' @return returns classification definintion in long format (tidy data) as that is what the package needs in operation.
#'
#' @importFrom vroom vroom
#' 
#' @examples
#' elixhauser_cl <- read_classes_csv(file = "data/classification_codes/elixhauser_classes_wide.csv")
#' @rdname read_classes_csv
#' @export
#' 
read_classes_csv <- function(file) {
  read_classes_tibble(sel_classes = vroom::vroom(file = file))
}