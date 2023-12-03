library(tidyverse)

# country_txt == Incident Location
# natlty1_txt == This is the nationality of the target that was attacked
# nkill == The number includes  victims and attackers who died as a direct result of the incident.
# nkillter ==  only perpetrator fatalities

globalterrorismdb_il <-
  globalterrorismdb_0919dist %>%
  select(country_txt  , targtype1_txt, nkill, nwound ,
         provstate,city ,location ,attacktype1_txt,suicide,nkillter, 
         multiple ,related ,summary,addnotes, natlty1_txt,eventid , iyear , imonth , iday) %>% 
  filter(iyear>=1994,country_txt=="Israel"|
           country_txt=="West Bank and Gaza Strip" ,
         natlty1_txt== "Israel")

library(lubridate)

globalterrorismdb_il <- globalterrorismdb_il %>%
  mutate(date = make_date(iyear, imonth, iday))

globalterrorismdb_il$yearweek <-
  strftime(globalterrorismdb_il$date, format = "%Y-W%U")

# copy column targtype1_txt
globalterrorismdb_il <- globalterrorismdb_il %>%
  mutate( targtype1_txt_new = targtype1_txt )

globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Military"] <- "security force"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Police"] <- "security force"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Private Citizens & Property"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Educational Institution"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Business"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Government (General)"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Unknown"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Food or Water Supply"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Journalists & Media"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Airports & Aircraft"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Transportation"] <- "civilians"
 globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Government (Diplomatic)"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Tourists"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Utilities"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Religious Figures/Institutions"] <- "civilians"

attach(globalterrorismdb_il)

globalterrorismdb_il$nincident <- rep(1)

# Creating a list of weeks numbers
library(data.table)
date <- seq(as.IDate("1994-01-01"), as.IDate("2019-12-31"), 1)
dt <- data.table(date=date)
dt$yearweek <-  strftime(date, format = "%Y-W%U") # this table: "date" and "yearweek"
calander_1994_2019 <- dt[,2] %>% count(yearweek) %>% select(1) # only "yearweek"

rm(dt,df,date)

# Incident Location in West Bank and Gaza Strip
# targtype1_txt "Military" "Police"
# targtype1_txt "Citizens"

terrorismdb <- globalterrorismdb_il

terrorismdb <- terrorismdb %>% 
  group_by(date,targtype1_txt_new,country_txt) %>% 
  summarise(incidents=sum(nincident,na.rm = T),killd=sum(nkill,na.rm = T))

#split column to two
terrorismdb <-
  terrorismdb %>%
  mutate(total_incidents_IL  = ifelse(country_txt == "Israel",1,0)) %>% 
  mutate(total_incidents_yesha  = ifelse(country_txt == "West Bank and Gaza Strip",1,0)) %>%
  mutate(targt_civilians = ifelse(targtype1_txt_new == "civilians",1,0)) %>% 
  mutate(targt_security_force = ifelse(targtype1_txt_new == "security force",1,0)) 


# ---------------------------------------------------
# This code also appears in peace_Q6 tab
------------------------------------------
peace_and_war$date <- as.IDate(peace_and_war$date)
class(peace_and_war$date)

dt_terror <- terrorismdb
dt_peace <- peace_and_war


# ------------------  days data.table
library(data.table)
date <- seq(as.IDate("1994-01-01"), as.IDate("2018-12-28"), 1)
day_df <- data.table(date=date)

# -----------------   security_situation

x1 <- day_df %>% filter(date>="2000-09-27",date<"2005-03-05") %>%
  group_by(date) %>% count() %>% mutate( security_situation = n ) #(copy=original)
x1$n[x1$n== 1] <- "intifada2"

x2 <- day_df %>% filter(date>="2006-06-28",date<"2006-07-11") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x2$n[x2$n== 1] <- "gishmey_kayitz"

x3 <-  day_df %>% filter(date>="2006-07-12",date<"2006-08-14") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x3$n[x3$n== 1] <- "lebanon2"

x4 <-  day_df %>% filter(date>="2008-12-27",date<"2009-01-18") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x4$n[x4$n== 1] <- "oferet"

x5 <-  day_df %>% filter(date>="2012-11-14",date<"2012-11-21") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x5$n[x5$n== 1] <- "amud_anan"

x6 <-  day_df %>% filter(date>="2014-07-08",date<"2014-08-26") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x6$n[x6$n== 1] <- "tzuk_eitan"

security_situation <- rbind(x1,x2,x3,x4,x5,x6)
security_situation <- day_df %>% left_join(security_situation) %>% 
  rename(security_situation_num=security_situation ) %>% 
  rename(security_situation_txt=n)


dtdt <- left_join(security_situation,dt_peace,by="date") 
peace_and_terror <- left_join(dtdt,dt_terror,by="date") %>% 
  select(1,4:25,2,3)









