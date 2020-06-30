## Käännä paketti ----------
# devtools::document()
pkgdown::build_site()

## Vie classification datat sysdataan ----

## Elixhauser scores
elixhauser_classes <- readr::read_csv2("./data/elixhauser_classes.csv")
elixhauser_classes <- tibble::as_tibble(elixhauser_classes)
charlson_classes <- readr::read_csv2("./data/charlson_classes.csv")
charlson_classes <- tibble::as_tibble(charlson_classes)
save(charlson_classes, elixhauser_classes, file = "./R/sysdata.rda")

# usethis::use_data(elixhauser_classes, internal = TRUE) # toinen tapa
# tähän voi paketin sisässä viitata kolmella ::: eli 
regstudies:::elixhauser_classes
regstudies:::charlson_classes




## 

## Testing Publice docs -----
# if(!dir.exists("../regstudies_webpage/")) dir.create("../regstudies_webpage/")
# if(dir.exists("../regstudies_webpage/")) system("rsync -r ./docs/ ../regstudies_webpage")
