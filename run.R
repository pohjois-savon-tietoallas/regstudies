## Käännä paketti ----------
# devtools::document()
pkgdown::build_site()


## Vie classification datat sysdataan ----
## Elixhauser scores
elixhauser_classes <- read.csv2("./data/classification_codes/elixhauser_classes_wide.csv")
# elix <- elix %>% rename(ICD = icd)
save(elixhauser_classes, file = "./R/sysdata.rda")
# usethis::use_data(elixhauser_classes, internal = TRUE) # toinen tapa
# tähän voi paketin sisässä viitata kolmella ::: eli 
# regstudies:::elixhauser_classes

