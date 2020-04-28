# This dev_v3.R code starts to use wide classification data files.
rm(list=ls())
setwd("C:/Users/jukop/Documents/UEFhommat/Tietoallasprojekti/rekkarityokalu/regstudies")
#install.packages(c("dplyr", "tidyr", "stringr", "purrr"))
### How to use

library(tidyverse)
library(regstudies) # requires dplyr, tidyr, stringr, purrr
# read cohort data
library(vroom)
cohort <- vroom("./datas/fake_cohort.csv")
cohort %>%
  head()
# personid: id for each individual in the study
# postingdate: date of interest to find comorbidities
# status: some kind of additional data (random 0/1)

# prepare some register data:
reg<-vroom(file="./datas/ostpre_hilmo_9616_dgt_fake.csv")
library(lubridate)
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
  datefilter(indexdate=postingdate,range=years(2),admissiondate,dischargedate)

# Elixhauser scores:
elixhauser_classes <- classes_to_wide(vroom(file = "datas/classification_codes/elixhauser_classes_wide.csv"))
elixscore <- filtereddata %>%
  classifydata_long(icdcodes=CODE1,diag_tbl=elixhauser_classes) %>%
  summaryscore(score_AHRQ,score_van_Walraven)
elixscore <- left_join(cohort %>% select(personid), elixscore)
#View(elixscore)

# Charlson score:
charlson_classes <- classes_to_wide(vroom(file = "datas/classification_codes/charlson_classes_wide.csv"))
charlsonscore <- filtereddata %>%
  classifydata_long(icdcodes=CODE1,diag_tbl=charlson_classes) %>%
  rename(score_charlson=score) %>%
  summaryscore(score_charlson)
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
  filter(`summary(score_AHRQ)`+`summary(score_van_Walraven)`+`summary(score_charlson)`>0) %>%
  head(10)
plot(cohortscores)
cor(cohortscores %>% select(-classification))
