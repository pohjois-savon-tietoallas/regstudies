## Käännä paketti ----------
# devtools::document()
pkgdown::build_site()


## Vie classification datat sysdataan ----

## Elixhauser scores
elixhauser_classes <- read.csv2("./data/classification_codes/elixhauser_classes_wide.csv")
# elix <- elix %>% rename(ICD = icd)
elixhauser_classes <- as.tibble(elixhauser_classes)
save(elixhauser_classes, file = "./R/sysdata.rda")
# usethis::use_data(elixhauser_classes, internal = TRUE) # toinen tapa
# tähän voi paketin sisässä viitata kolmella ::: eli 
regstudies:::elixhauser_classes

## 

## Testing Publice docs -----
# if(!dir.exists("../regstudies_webpage/")) dir.create("../regstudies_webpage/")
# if(dir.exists("../regstudies_webpage/")) system("rsync -r ./docs/ ../regstudies_webpage")
