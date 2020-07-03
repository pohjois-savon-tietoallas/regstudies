library(dplyr)
library(stringr)
library(tibble)

## Build sysdata --------------
if(TRUE){
  ## Elixhauser scores
  elixhauser_classes <- readr::read_csv2("./data/elixhauser_classes.csv")
  elixhauser_classes <- tibble::as_tibble(elixhauser_classes)
  charlson_classes <- readr::read_csv2("./data/charlson_classes.csv")
  charlson_classes <- tibble::as_tibble(charlson_classes)
  ## save file
  save(charlson_classes, elixhauser_classes, file = "./R/sysdata.rda")
  
  # usethis::use_data(elixhauser_classes, internal = TRUE) # toinen tapa
  # tähän voi paketin sisässä viitata kolmella ::: eli 
  # regstudies:::elixhauser_classes
  # regstudies:::charlson_classes
  
  # Generate persons with 0 events at regdata
  sample_cohort_extra <- tibble::tibble(
    personid = sort(sample(1101:1500,100)),
    gender = sample(c(1,2), 100, replace = T),
    postingdate = rep(as.Date("2000-01-01"), 100)
  )
  ## Generate random cohort id data
  sample_cohort <- tibble::tibble(
    personid = setdiff(seq(1101, 1500),sample_cohort_extra$personid),
    gender = sample(c(1,2), 300, replace = T),
    postingdate = rep(as.Date("2000-01-01"), 300)
  )
  
  ## Read ICD-codes so that we generate from all classes:
  regstudies::read_classes(elixhauser_classes) %>% 
    filter(!is.na(regex)) %>% 
    mutate(regex2=str_replace_all(regex,"^\\^","")) %>% 
    group_by(icd,class_elixhauser) %>%
    mutate(regex3=str_split(regex2,pattern="\\|\\^")) %>%
    mutate(regex4=purrr::map_chr(regex3,magrittr::extract(1))) %>% 
    pull(regex4) -> random_icd_codes
  
  sample_at_least_once <- function(x, size, replace = FALSE, prob = NULL) {
    # size must be more than length(x)!
    if (size >= length(x)) {
      # take all elements at least once
      sample1<-sample(x=x,size=length(x),replace=FALSE,prob=prob)
      sample2<-sample(x=x,size=size-length(x),replace = replace,prob=prob)
      return(c(sample1,sample2))
    } else {
      return(NULL)
    }
  }
  ## Generate reg_data
  sample_regdata <- tibble::tibble(
    personid = sample(sample_cohort$personid, 10000, replace = TRUE),
    CODE1 = sample_at_least_once(random_icd_codes, 10000, replace = TRUE),
    adm_date = sample(seq(as.Date("1990-01-01"), as.Date("2005-12-31"), by = 1), 10000, replace = T)
    # disc_date = ,
  )
  sample_regdata$disc_date <- (sample_regdata$adm_date + sample(seq(0,180), 10000, replace = T))
  
  # Determine if codes are ICD-8, ICD-9 or ICD-10
  sample_regdata <- sample_regdata %>%
    mutate(icd = case_when(
      lubridate::year(disc_date) < 1987 ~ "icd8",
      lubridate::year(disc_date) < 1996 & lubridate::year(disc_date)>=1987 ~ "icd9",
      lubridate::year(disc_date) >= 1996 ~ "icd10"
    ))
  
  sample_cohort <- rbind(sample_cohort_extra,sample_cohort)
  
  save(sample_regdata, file = "./data/sample_regdata.RData")
  save(sample_cohort, file = "./data/sample_cohort.RData")

}

## Knit and Translate package ----------
if(TRUE){
  #rm(list = c("charlson_classes", "elixhauser_classes"))
  devtools::document()
  pkgdown::build_home()
  pkgdown::build_site()
}


