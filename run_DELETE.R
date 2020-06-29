## Käännä paketti ----------
# devtools::document()
pkgdown::build_site()


## Vie classification datat sysdataan ----

## Elixhauser scores
elixhauser_classes <- read.csv2("./data/elixhauser_classes_wide.csv")
elixhauser_classes <- tibble::as.tibble(elixhauser_classes)
charlson_classes <- read.csv2("./data/charlson_classes_wide.csv")
charlson_classes <- tibble::as.tibble(charlson_classes)
save(charlson_classes, elixhauser_classes, file = "./R/sysdata.rda")

# usethis::use_data(elixhauser_classes, internal = TRUE) # toinen tapa
# tähän voi paketin sisässä viitata kolmella ::: eli 
regstudies:::elixhauser_classes
regstudies:::charlson_classes




## 

## Testing Publice docs -----
# if(!dir.exists("../regstudies_webpage/")) dir.create("../regstudies_webpage/")
# if(dir.exists("../regstudies_webpage/")) system("rsync -r ./docs/ ../regstudies_webpage")
