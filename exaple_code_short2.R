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
cohort %>%
  head()
# personid: id for each individual in the study
# postingdate: date of interest to find comorbidities

# prepare some register data:
reg <- vroom(file="../rekisteridatat/ostpre_hilmo_9616_dgt_eng.csv")
reg <- reg %>%
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
        )

# complete code examples:
filtereddata <- cohort %>%
  left_join(reg %>% select(personid,CODE1,admissiondate,dischargedate,icd),by="personid") %>%
  filter_date_hosp(indexdate=postingdate,
                   time_before=years(2),
                   admission_date=admissiondate,
                   discharge_date=dischargedate)

# Elixhauser scores:
elixhauser_classes2 <- read_classes_csv(file = "data/classification_codes/elixhauser_classes.csv")
elixhauser_classes2 %>% get_classification_name()
View(elixhauser_classes2)
file.show("data/classification_codes/elixhauser_classes.csv")
# classification table:
elixhauser_icdtable <- filtereddata %>%
  make_classify_table(icdcodes=CODE1,diag_tbl=elixhauser_classes2)

elixhauser_classified <- filtereddata %>%
  classify_codes(icdcodes=CODE1,diag_tbl=elixhauser_classes2)
elixhauser_classified <- left_join(cohort %>% select(personid), elixhauser_classified)
elixhauser_classified_score <- elixhauser_classified %>%
  sum_score(score_AHRQ,score_van_Walraven)
#View(elixhauser_classified_score)

charlson_classes2 <- read_classes_csv(file = "data/classification_codes/charlson_classes.csv")
charlson_classes2 %>% get_classification_name()

# Multiple classifications on the same data
two_classifications <- filtereddata %>%
  classify_codes(icdcodes=CODE1,diag_tbl=elixhauser_classes2) %>%
  classify_codes(icdcodes=CODE1,diag_tbl=charlson_classes2)
#View(two_classifications)
two_cohort_founds <- left_join(cohort %>% select(personid), two_classifications)
elix_charlson_score <- two_cohort_founds %>%
  rename(score_charlson=score) %>%
  sum_score(score_AHRQ,score_van_Walraven,score_charlson)
#View(elix_charlson_score)


register_founds <- filtereddata %>%
  classify_codes(icdcodes=CODE1,diag_tbl=elixhauser_classes2)
cohort_founds <- left_join(cohort %>% select(personid), register_founds)

elixscore <- cohort_founds %>%
  sum_score(score_AHRQ,score_van_Walraven)
#View(elixscore)






#------------------------ POIKKI ------------------

#na_fill0<-function(x) {
#  ifelse(is.na(x),0,x)
#}
#elixscore <- left_join(cohort %>% select(personid), elixscore) %>%
#  mutate_all(na_fill0)

# [X] demoa Hospital Frailty Risk Score -indeksiä!
# [X] selvitä, mitkä funktiot on tarpeettomia ja mitkä tarpeellisia
# [X] class, label ja score muotoon class_charlson, label_charlson ja score_charlson
# [X]  -> classify_data_long -funktiossa!
# [X] tarvitaan left_join, joka tekee muuttujan tyypin mukaan käyttäjän haluaman täytön: esim. c("numeric"=0,"character"="Elixhauser")
# [X]  -> tulee ehkä tarpeettomaksi, jos muutetaan class, label ja score uusille nimille
# TODO: 
# - korjaa classify_data_long -funktio sellaiseksi että se palauttaa koko datan!

# Charlson score:
#charlson_classes <- classes_to_wide(vroom(file = "data/classification_codes/charlson_classes_wide.csv"))
sel_classes <- vroom(file = "data/classification_codes/charlson_classes_wide.csv")
charlson_classes <- read_classes_csv(file = "data/classification_codes/charlson_classes_wide.csv")
#View(charlson_classes)

# dev:
dev1<-filtereddata %>%
  classify_data_long(icdcodes=CODE1,diag_tbl=charlson_classes)
dev2<-filtereddata %>%
  classify_long(icdcodes=CODE1,diag_tbl=charlson_classes)
dim(filtereddata)
dim(dev1)
dim(dev2)
# TODO: Jotain kummaa tässä on!

charlsonscore <- filtereddata %>%
  classify_data_long(icdcodes=CODE1,diag_tbl=charlson_classes) %>%
  rename(score_charlson=score) %>%
  sum_score(score_charlson)
charlsonscore <- left_join(cohort %>% select(personid), charlsonscore)
#View(charlsonscore)

# Hospital Frailty Risk Score:
hfrs_dat<-vroom(file = "data/classification_codes/hospital_frailty_risk_score_wide.csv")
hfrs_dat<-hfrs_dat %>%
  mutate(label="Frail group") %>%
  select(classification,class,label,score,icd10)
names(hfrs_dat)
write.table(hfrs_dat,file="data/classification_codes/hospital_frailty_risk_score_wide_v2.csv",sep=";",dec=".",row.names = FALSE)
View(hfrs_dat)
hfrs_classes <- read_classes_csv(file = "data/classification_codes/hospital_frailty_risk_score_wide_v2.csv")
#View(charlson_classes)
hfrsscore <- filtereddata %>%
  classify_data_long(icdcodes=CODE1,diag_tbl=hfrs_classes) %>%
  rename(score_charlson=score) %>%
  sum_score(score_charlson)

mode <- function(x) { # by Ken Williams at stackoverflow
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}
mode(hfrsscore$classification)


hfrsscore <- left_join(cohort %>% select(personid), hfrsscore) %>%
  mutate(classification=mode(classification))
#  mutate(classification=ifelse(is.na(classification),mode(classification),classification))
View(hfrsscore)

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
