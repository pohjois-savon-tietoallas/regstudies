mode <- function(x) { # by Ken Williams at stackoverflow
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
