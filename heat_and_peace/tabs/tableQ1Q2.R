#----read_csv----
library(readr)
dfo <- read_csv("dfo.csv", col_types = cols(date = col_date(format = "%d/%m/%Y")))
write.csv(dfo, file="C:/Users/Dan/Documents/R/dfo.csv")
#----Useful functions for DATE----
meteorologic
range(meteorologic$date)

meteorologic$date[17] - meteorologic$date[1]# days count

b1 = ISOdate(1997,7,13)
b2 = ISOdate(2013,8,14)
b2 - b1
difftime(b2,b1,units='weeks')

df$year <- format(as.Date(df$date), "%Y")

#----week #0-----
# last week is #52- from the last sunday  till 31-12
# first week is #0- from 01-01 till first saturday
library(lubridate)
daydate <- as.Date("1994-04-03")
dates <- seq(daydate, daydate + 8424, by = "1 day") 

dat <- data.frame(dates = dates, Week = format(dates, format = "%U"))

# ---week #53-----
# last week is #53- from the last sunday  till 31-12
# first week is 1- from 01-01 till first saturday
library(lubridate)
library(data.table)

dd <- seq(as.IDate("2018-01-01"), as.IDate("2019-04-10"), 1)
dt <- data.table(day = dd)

dt[, week       := week(day),]
dt[, week_start := floor_date (day, unit = "week"),]
dt[, week_range := interval(week_start, week_start + days(6))]
dt[, week_start := NULL,]

# ---name the weekdays----
dd <- seq(as.IDate("2018-01-01"), as.IDate("2018-04-10"), 1)
dt <- data.table(day = dd,weekday = weekdays(dd))
# --------
x <- peace_index[31730:31749,1:3]

zy <- peace_index[31730:31749,1:3] %>%group_by(date,oslosp) %>%
  summarise(n = n()) %>%  mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))
  mutate(freq = n / sum(n))
zy$n <- NULL  

library(tidyr)
spread(zy, oslosp, freq)
# -----peace_Q1Q2----
library(tidyverse)
library(data.table)
peace_Q1Q2 <- peace_index[,1:3]
peace_Q1Q2$oslosp [is.na(peace_Q1Q2$oslosp)] <- 0 #replace NA to 0
peace_Q1Q2$oslobl [is.na(peace_Q1Q2$oslobl)] <- 0

Q1 <- peace_Q1Q2[,1:2] %>% group_by(date,oslosp) %>%
  summarise(n = n()) %>%  mutate(freq=n/sum(n)) %>% mutate(i=oslosp*freq) %>% 
  group_by(date) %>% summarise(q1=sum(i))

Q2 <- peace_Q1Q2[,1:3] %>% group_by(date,oslobl) %>%
  summarise(n = n()) %>% mutate(freq=n/sum(n)) %>% mutate(i=oslobl*freq) %>% 
  group_by(date) %>% summarise(q2=sum(i))

peace_Q1Q2 <- inner_join(Q1,Q2,by="date")

write.csv(peace_Q1Q2, file="C:/Users/Dan/Documents/R/peace_Q1Q2.csv")

library(readr)
peace_Q1Q2 <- read_csv("~/R/peace_Q1Q22.csv", col_types = cols(date = col_date(format = "%d/%m/%Y")))
dfo <- read_csv("dfo.csv", col_types = cols(date = col_date(format = "%d/%m/%Y")))
                       

day <- as.IDate(peace_Q1Q2$date)
peace_Q1Q2$yearweek <-  strftime(day, format = "%Y-W%U")
peace_Q1Q2 <- peace_Q1Q2 %>% select(yearweek,2,3,1)

# -----at_btselem----
library(data.table)
at_btselem <- Attacks_Btselem_data[,c(1,3:5)]

#renam column
`target: civilian(1) security-forces(2)`
`location: IL(1)  Yesha(2)`

at_btselem <-  at_btselem %>% 
  rename(location = `location: IL(1)  Yesha(2)`) 
at_btselem <-  at_btselem %>% 
  rename(target = `target: civilian(1) security-forces(2)`) 
#split column to two
at_btselem <- at_btselem %>% mutate(total_events_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(total_events_located_yesha  = ifelse(location == 2, "1",NA))

at_btselem <- at_btselem %>% mutate(total_events_target_civilian  = ifelse(target == 1, "1",NA)) %>%
  mutate(total_events_target_security_forces  = ifelse(target == 2, "1",NA))

at_btselem <- at_btselem[,c(1:2,5:8)]

# new column- number of attacks 

at_btselem$number_of_attacks <-rep(1,515)

# combind row for day
#at_btselem <- at_btselem%>% group_by(date) %>%
#  summarise(total_killed=sum(`number of killed`),
#            location_IL = max(location_IL, na.rm = T),
#            location_yesha = max(location_yesha, na.rm = T),
#            target_civilian = max(target_civilian, na.rm = T),
 #           target_security_forces = max(target_security_forces, na.rm = T),
  #          total_attacks=sum(number_of_attacks))

dd <- as.IDate(at_btselem$date)
dt <- data.table(i = 1:length(dd), date = dd)
dt[, week_start := floor_date (date, unit = "week"),]
dt[, week_range := interval(week_start, week_start + days(6))]
dt$week_range <- as.factor(dt$week_range)

at_btselem$yearweek <-  strftime(dd, format = "%Y-W%U")

x <- bind_cols(at_btselem,dt)
at_btselem <- bind_cols(at_btselem,dt[,4])

class(at_btselem$date)
class(dt$week_range)
at_btselem$total_events_located_IL <- as.numeric(at_btselem$total_events_located_IL)
at_btselem$total_events_located_yesha <- as.numeric(at_btselem$total_events_located_yesha)
at_btselem$total_events_target_civilian <- as.numeric(at_btselem$total_events_target_civilian)
at_btselem$total_events_target_security_forces <- as.numeric(at_btselem$total_events_target_security_forces)

at_btselem <- at_btselem %>%  rename(total_killed = `number of killed`) %>% 
  rename(total_attacks = number_of_attacks)

btselemWeeks <- at_btselem %>%
  group_by(yearweek,week_range) %>% 
  summarise_at(vars(total_killed:total_attacks), sum, na.rm = TRUE)
  
#-------------------
peace_Btslm <- peace_Q1Q2 %>% filter(yearweek>"2000-W39")

bt_pe <- full_join (btselemWeeks,peace_Btslm) %>% arrange(yearweek) %>% rename(survey_date =date)

dm <- as.IDate(meteorologic$date)
meteorologic$yearweek <-  strftime(dm, format = "%Y-W%U")

me <- meteorologic%>% filter(yearweek>"2000-W39") %>%   group_by(yearweek) %>% 
  summarise_at(vars(temperature:humidity), mean, na.rm = TRUE)


write.csv(bt_pe_me, file="C:/Users/Dan/Documents/R/bt_pe_me.csv")
bt_pe_me <- read_csv("~/R/bt_pe_me_4.csv", 
                     col_types = cols(survey_date = col_date(format = "%d/%m/%Y")))

bt_pe_me %>% select(1,2,8,14)
bt_pe_me <- bt_pe_me %>% filter(yearweek!="2000-W39")

week0ago <- bt_pe_me$total_killed
week1ago <- lag(week0ago, 1)
week2ago <- lag(week0ago, 2)
week3ago <- lag(week0ago, 3)
week4ago <- lag(week0ago, 4)
weeks01234 <- cbind(week0ago,week1ago,week2ago,week3ago,week4ago)

weeks01234 <- weeks01234 %>% data.frame()
weeks01234[is.na(weeks01234)] <- 0
bt_pe_me <- cbind(bt_pe_me,weeks01234) 

