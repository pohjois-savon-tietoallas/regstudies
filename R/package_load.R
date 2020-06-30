

.onAttach <- function(libname, pkgname){
  packageStartupMessage("regstudies - register studies with R")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.regstudies <- list(
    regstudies.path = "~/R-dev",
    regstudies.install.args = "",
    regstudies.name = "Juho Kopra, Jani Miettinen & Reijo Sund",
    regstudies.desc.author = c("Juho Kopra <juho.kopra@uef.fi> [aut, cre]",
                               "Jani Miettinen <jani.miettinen@uef.fi> [aut]",
                               "Reijo Sund <reijo.sund@uef.fi> [aut]"),
    regstudies.desc.license = "MIT + File LICENCE.txt",
    regstudies.desc.suggests = NULL,
    regstudies.desc = list()
  )
  toset <- !(names(op.regstudies) %in% names(op))
  if(any(toset)) options(op.regstudies[toset])
  
  invisible()
}
