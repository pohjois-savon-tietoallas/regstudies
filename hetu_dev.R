library(vroom)
library(regstudies)
library(tidyverse)
library(stringr)
library(lubridate)
library(regstudies)

hetusim <- tibble(bdate=seq(date("1890-01-01"),date("2020-01-01"),by=365*10))
hetusim <- hetusim %>%
  mutate(Henkilotunnus=generate_random_hetu(bdate))

View(hetusim)
format(hetusim$hetu, format="%d%m%y")
day(hetusim)
names(dat)
get_var_types(dat)

dat <- dat %>%
  mutate(spaiva=str_sub(hetu,1,6),nnn=str_sub(hetu,8,10)) %>%
  mutate(spaiva=paste0(str_sub(spaiva,1,4),ifelse(str_sub(hetu,7,7)=="A","20","19"),str_sub(spaiva,5,6))) %>%
  mutate(spaiva_pvm=dmy(spaiva),sukup=as.integer(nnn) %% 2)# nainen=0, mies=1
dat <- dat %>% 
  mutate(vpaiva=date(dmy_hm(Ilmpvm)))

dat <- dat %>% 
  mutate(dg=str_sub(`Diagnoosi oire syy`,1,5))

dat <- dat %>%
  mutate(mage=as.numeric(as.duration(vpaiva-spaiva_pvm),"years"),
         mageround=round(mage)) %>%
  mutate(byear=year(spaiva_pvm))
#View(dat)

# pseudonymisointi:

hetu <- dat %>%
  select(hetu) %>%
  distinct()

alku <- 0 # montako lukua lisätään juoksevaa numeroon
hetu <- hetu %>%
  mutate(id=row_number()+alku)

dat <- dat %>%
  left_join(hetu)
dat <- dat %>%
  select(-hetu,-Nimi,-nnn,-POTILASNUMERO)
View(dat)
View(hetu)

write.csv2(dat,file="skoodi_covid2019s.csv",row.names = FALSE)
write.csv2(hetu,file="hetu_covid2019s.csv",row.names = FALSE)

