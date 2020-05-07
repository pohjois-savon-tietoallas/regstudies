#' Make regural expression classifications from LIKE\% classifications
#'
#' @param .data holds the data which has variable names given as 'classname' and 'diagnosis'. Must be convertible to a tibble.
#' @param classname name of the variable containing names/labels for the classes
#' @param diagnosis name of the variable containing strings in LIKE\% -format
#' @param diagnosis.rm name of the variable defining exceptions of ICD-codes not to be included in the class
#' @param sep separator of different diagnoses in 'diagnosis'. String.
#'
#' @return returns a tibble with regular expression definitions
#'
make_regex <- function(.data,classname,diagnosis,diagnosis.rm,sep=", ") {
  .data=as_tibble(.data)
  diagnosis = enquo(diagnosis)
  diagnosis.rm = enquo(diagnosis.rm)
  nr<-dim(.data)[1]
  # lets transform diag.list and diag.list.rm to 'regex' format!
  diag.vec<-vector("character",nr)
  diag.vec.rm<-diag.vec
  for(l in 1:nr) {
    vec1 <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis)),", "))
    vec.rm <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis.rm)),", "))
    diag.vec[l]    = paste(paste0("^",gsub(pattern="%",replacement="",x=vec1)),collapse="|")
    diag.vec.rm[l] = paste(paste0("^",gsub(pattern="%",replacement="",x=vec.rm)),collapse="|")
  }
  diag.names<-.data$Lyhenne
  # lets create a table 'diag_tbl' which holds diagnosis classification!
  diag_tbl<-tibble(regex=diag.vec,regex.rm=diag.vec.rm,label=diag.names) %>%
    mutate(regex.rm=ifelse(regex.rm=="^NA",NA,regex.rm))
  diag_tbl
}

