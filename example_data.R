library(dplyr)
library(stringr)

## Generate random cohort id data
sample_cohort <- tibble::tibble(
  personid = seq(1201, 1500),
  gender = sample(c(1,2), 300, replace = T),
  postingdate = rep(as.Date("2000-01-01"), 300)
)

## Read ICD-codes so that we generate from all classes:
random_icd_codes <- read_classes_tibble(regstudies:::charlson_classes) %>%
  filter(icd=="icd10") %>%
  select(regex) %>%
  mutate(out=str_sub(string=regex,start=2,end=4)) %>%
  filter(!is.na(out)) %>%
  distinct() %>%
  pull(out) 

## Generate reg_data
sample_regdata <- tibble::tibble(
  personid = sample(cohort_data$personid, 10000, replace = TRUE),
  CODE1 = sample(random_icd_codes, 10000, replace = TRUE),
  adm_date = sample(seq(as.Date("1990-01-01"), as.Date("2005-12-31"), by = 1), 10000, replace = T)
  # disc_date = ,
)
sample_regdata$disc_date <- (sample_regdata$adm_date + sample(seq(0,180), 10000, replace = T))


# save(sample_cohort, sample_regdata, charlson_classes, elixhauser_classes, file = "./R/sysdata.rda")
