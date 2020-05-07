# This dev_v3.R code starts to use wide classification data files.
rm(list=ls())
setwd("C:/Users/jukop/Documents/UEFhommat/Tietoallasprojekti/rekkarityokalu/regstudies")
#install.packages(c("dplyr", "tidyr", "stringr", "purrr"))
### How to use

library(tidyverse)
library(regstudies) # requires dplyr, tidyr, stringr, purrr
# read cohort data
library(vroom)
library(lubridate)
cohort <- vroom("./data/fake_cohort.csv",col_types=c(personid='i',postingdate='D'))
#write.csv2(cohort%>%select(-status),
#           file="./data/fake_cohort2.csv",row.names=FALSE)
cohort %>%
#  mutate(postingdate=ymd(postingdate)) %>%
  head()
# personid: id for each individual in the study
# postingdate: date of interest to find comorbidities
# status: some kind of additional data (random 0/1)

# prepare some register data:
#C:/Users/jukop/Documents/UEFhommat/Tietoallasprojekti/rekkarityokalu
reg <- vroom(file="../rekisteridatat/ostpre_hilmo_9616_dgt_eng.csv")
reg<-reg %>%
  mutate(admissiondate=dmy(admissiondate),dischargedate=dmy(dischargedate)) %>%
  mutate(CODE1=ifelse(CODE1=="-",NA,CODE1),CODE2=ifelse(CODE2=="-",NA,CODE2))
reg %>%
  head()

# lets calculate if the code is icd-8, icd-9 or icd-10
reg <- reg %>%
  mutate(icd=case_when( year(dischargedate)<1987 ~ "icd8",
                        year(dischargedate)<1996 & year(dischargedate)>=1987 ~ "icd9",
                        year(dischargedate)>=1996 ~ "icd10"
                      )
        ) #%>% mutate(startletter=regexpr(pattern="^[A-Z]",CODE1)>0)


# complete code examples:
filtereddata <- cohort %>%
  left_join(reg %>% select(personid,CODE1,admissiondate,dischargedate,icd),by="personid") %>%
  filter_date(indexdate=postingdate,range=years(2),admissiondate,dischargedate)

# Elixhauser scores:
elixhauser_classes <- read_class(file = "data/classification_codes/elixhauser_classes_wide.csv")
#View(elixhauser_classes)
elixscore <- filtereddata %>%
  classify_data_long(icdcodes=CODE1,diag_tbl=elixhauser_classes) %>%
  sum_score(score_AHRQ,score_van_Walraven)
elixscore %>%
  get_var_types()

elixscore
elixscore <- left_join0(cohort %>% select(personid), elixscore)
#View(elixscore)

# [X] summary(score_AHRQ) muotoon sum(score_AHRQ)
# [X] classify_data_long tulostaa liikaa
# [X] classes_to_wide voisi olla read_classes, jolle annetaan tiedostopolku
# [X] cohort datasta status pois, postingdate muotoon indexdate (ehkä)
# [X] left_join0 lisätty
# [X] get_var_types lisätty
# TODO: 
# - class, label ja score muotoon class_charlson, label_charlson ja score_charlson
# - selvitä, mitkä funktiot on tarpeettomia ja mitkä tarpeellisia
# - demoa Hospital Frailty Risk Score -indeksiä!

# Charlson score:
#charlson_classes <- classes_to_wide(vroom(file = "data/classification_codes/charlson_classes_wide.csv"))
charlson_classes <- read_class(file = "data/classification_codes/charlson_classes_wide.csv")
#View(charlson_classes)
charlsonscore <- filtereddata %>%
  classify_data_long(icdcodes=CODE1,diag_tbl=charlson_classes) %>%
  rename(score_charlson=score) %>%
  sum_score(score_charlson)
charlsonscore <- left_join(cohort %>% select(personid), charlsonscore)
#View(charlsonscore)

dim(filtereddata %>% select(personid) %>% distinct())
dim(cohort)
dim(elixscore)
dim(charlsonscore)

bothscores<-left_join(elixscore %>% select(-classification),
                      charlsonscore %>% select(-classification),by="personid")
fill_na<-function(x,fill=0) ifelse(is.na(x),fill,x)
cohortscores<-left_join(cohort %>% select(personid),bothscores) %>%
  mutate_all(fill_na)
cohortscores %>%
  head(10)
cohortscores %>%
  filter(`sum(score_AHRQ)`+`sum(score_van_Walraven)`+`sum(score_charlson)`>0) %>%
  head(10)
plot(cohortscores)
cor(cohortscores)
