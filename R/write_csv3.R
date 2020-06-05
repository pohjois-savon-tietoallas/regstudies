#' Write csv file with dot decimal mark 
#'
#' @author Original authors Hadley Wickham, Jim Hester, Romain Francois, modified by Juho Kopra
#' 
#' @importFrom readr write_delim
#' 
#' @rdname write_csv3
#' @export
#' 
write_csv3 <- function(x, path, na = "NA", append = FALSE, col_names = !append,
                       quote_escape = "double") {
  x <- change_decimal_separator(x, decimal_mark = ".")
  readr::write_delim(x, path, delim = ";", na = na, append = append,
                     col_names = col_names, quote_escape = quote_escape)
}
#' Change decimal separator
#' 
#' Copy from readr package
#' @author Original authors Hadley Wickham, Jim Hester, Romain Francois
#'
change_decimal_separator <- function(x, decimal_mark = ",") {
  stopifnot(is.data.frame(x))
  numeric_cols <- vapply(x, is.numeric, logical(1))
  
  format_seps <- function(x, decimal_mark) {
    nas <- is.na(x)
    x <- format(x, decimal.mark = decimal_mark)
    x[nas] <- NA_character_
    x
  }
  
  x[numeric_cols] <- lapply(x[numeric_cols], format_seps, decimal_mark)
  
  x
}