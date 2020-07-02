#' Read class definitions from csv file
#'
#' Use this function for your own code classification. It reads class definition file based on the file path and operates classification definitions from wide format to long format. You can edit you own classifications in Spreadsheet Program (ex. LibreOffice, Excel) and save file to csv-file.
#'
#' @family read functions
#' @param file filename or file path as a character string.
#' @return returns classification definintion in long format (tidy data) as that is what the package needs in operation.
#' @importFrom vroom vroom
#' @examples
#'  \dontrun{
#' my_classes <- read_classes_csv(file = "my_classes.csv")
#' }
#' @rdname read_classes_csv
#' @export
#' 
read_classes_csv <- function(file) {
  read_classes_tibble(sel_classes = vroom::vroom(file = file))
}