# devailua, Juho Kopra

library(regstudies) 
library(tidyverse)
library(vroom)
library(lubridate)

#-----


## Generate random cohort id data
cohort_data <- tibble::tibble(
  personid = seq(1201, 1500),
  gender = sample(c(1,2), 300, replace = T),
  postingdate = rep(as.Date("2000-01-01"), 300)
)

# Read ICD-codes so that we generate from all classes:
out <- read_classes_tibble(regstudies:::charlson_classes) %>% 
  filter(!is.na(regex)) %>% 
  mutate(regex2=str_replace_all(regex,"\\^","")) %>%
  group_by(icd,class_charlson) %>%
  summarise(regex3=as.list(str_split(regex2,pattern="\\|")))
get_var_types(out)
#View(out)

read_classes_tibble(regstudies:::charlson_classes) %>%
  filter(icd=="icd10") %>%
  select(regex) %>%
  mutate(out=str_sub(string=regex,start=2,end=4)) %>%
  filter(!is.na(out)) %>%
  distinct() %>%
  pull(out) ->
  random_icd_codes


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
#table(random_icd_codes)
#sample_at_least_once(unique(random_icd_codes), 17, replace = TRUE) %>%
#  table()

random_icd_codes <- genclasses # TODO: Modify!
reg_data <- tibble::tibble(
  personid = sample(cohort_data$personid, 10000, replace = TRUE),
  CODE1 = sample_at_least_once(random_icd_codes, 10000, replace = TRUE),
  adm_date = sample(seq(as.Date("1990-01-01"), as.Date("2005-12-31"), by = 1), 10000, replace = T)
  # disc_date = ,
)
reg_data$disc_date <- (reg_data$adm_date + sample(seq(0,180), 10000, replace = T))