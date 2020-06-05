library(vroom)
library(regstudies)
library(tidyverse)
library(stringr)
library(lubridate)
library(regstudies)

dat <- tibble(bdate=seq(date("1890-01-01"),date("2020-01-01"),by=365*10))
dat <- dat %>%
  mutate(hetu=generate_random_hetu(bdate))

get_var_types(dat)
dat <- dat %>%
  mutate(sukup=get_gender(hetu),birthdate=get_birthdate(hetu))

dat <- dat %>% 
  mutate(vpaiva=seq(date("1890-01-01"),date("2020-01-01"),by=365*10)+years(rpois(14,40))+months(rpois(14,5))+days(10))

dat <- dat %>%
  mutate(age=get_age(vpaiva,birthdate,"years"))
#View(dat)

# pseudonymisointi:
hetukey <- make_hetu_key(.data=dat,hetu=hetu,randomize = TRUE)

dat <- dat %>%
  left_join(hetukey)
dat <- dat %>%
  select(-hetu,-Nimi,-nnn,-POTILASNUMERO)
#View(dat)
#View(hetu)

?write_csv3
write_csv3(dat,path="hetu_example.csv")
#write.csv2(hetu,file="hetu_covid2019s.csv",row.names = FALSE)

