#' left_join with fill option
#' 
#' Allows user to do left_join and fill missing NA values with 0 or other optional value
#' @param x tibble data to used as left in a join
#' @param y tibble data to used as right in a join
#' @param fill value to be filled into NA values
#' @return resulting tibble after join and fill
left_join0<-function(x,y,fill=0) {
  out<-left_join(x,y)
  out[is.na(out)]<-fill
  out
}