library(readxl)
Attacks_Foreign_Office_data <- read_excel("data/Attacks- Foreign Office data.xlsx")
View(Attacks_Foreign_Office_data)

Foreign_Office <-  Attacks_Foreign_Office_data

Foreign_Office$yearweek <-  strftime(Foreign_Office$date, format = "%Y-W%U") 

Foreign_Office <-  Foreign_Office %>% 
  rename(location = `location: IL(1) Yesha(2)`) 
Foreign_Office <-  Foreign_Office %>% 
  rename(target = `target: civilian(1) security-forces(2)`) 
#split column to two
Foreign_Office <-  Foreign_Office %>% mutate(FO_attacks_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(FO_attacks_located_yesha  = ifelse(location == 2, "1",NA))

Foreign_Office <-  Foreign_Office %>% 
  mutate(FO_attacks_target_civilian  = ifelse(target == 1, "1",NA)) %>%
  mutate(FO_attacks_target_security_forces  = ifelse(target == 2, "1",NA))

  
colnames(Foreign_Office)[3] <- "FO_total_killed"
colnames(Foreign_Office)[1] <- "FO_date"

Foreign_Office$FO_number_of_attacks <-rep(1,156)

Foreign_Office <-  Foreign_Office %>% select(6,11,7:10,1,3)

Foreign_Office$FO_attacks_located_IL <- as.numeric(Foreign_Office$FO_attacks_located_IL)
Foreign_Office$FO_attacks_located_yesha <- as.numeric(Foreign_Office$FO_attacks_located_yesha)
Foreign_Office$FO_attacks_target_civilian <- as.numeric(Foreign_Office$FO_attacks_target_civilian)
Foreign_Office$FO_attacks_target_security_forces <- as.numeric(Foreign_Office$FO_attacks_target_security_forces)

Foreign_Office[is.na(Foreign_Office)] <- 0 #replace NA to 0

------------------------
# Foreign_Office_attacks
------------------------
Foreign_Office_attacks <- Foreign_Office %>% group_by(yearweek) %>%
  summarise(FO_total_attacks=sum(FO_number_of_attacks),
            FO_attacks_located_IL=sum(FO_attacks_located_IL),
            FO_attacks_located_yesha=sum(FO_attacks_located_yesha),
            FO_attacks_target_civilian=sum(FO_attacks_target_civilian),
            FO_attacks_target_security_forces=sum(FO_attacks_target_security_forces),
            FO_total_killed=sum(FO_total_killed)) 


------------------------
# Foreign_Office_killed
------------------------
FO_il <- Foreign_Office %>%filter(FO_attacks_located_IL==1) %>% 
  group_by(yearweek) %>% 
  summarise(FO_killed_located_IL=sum(FO_total_killed))

FO_ye <- Foreign_Office %>%filter(FO_attacks_located_yesha==1) %>% 
  group_by(yearweek) %>% 
  summarise(FO_killed_located_yesha=sum(FO_total_killed))

FO_civ <- Foreign_Office %>%filter(FO_attacks_target_civilian==1) %>% 
  group_by(yearweek) %>% 
  summarise(FO_killed_target_civilian=sum(FO_total_killed))

FO_sec <- Foreign_Office %>%filter(FO_attacks_target_security_forces==1) %>% 
  group_by(yearweek) %>% 
  summarise(FO_killed_target_security_forces=sum(FO_total_killed))

FO1 <- full_join(FO_il,FO_ye) %>% full_join(FO_civ) %>% full_join(FO_sec)

--------------
#     bind 
--------------
  
date <- seq(as.IDate("1994-04-06"), as.IDate("2016-04-19"), 1)
dt <- data.table(date=date)
dt$yearweek <-  strftime(date, format = "%Y-W%U") # this table: "date" and "yearweek"
calander_94_17 <- dt[,2] %>% count(yearweek) %>% select(1) # only "yearweek"

Foreign_Office1 <- full_join(calander_94_17,Foreign_Office_attacks) %>% left_join(FO1)

Foreign_Office1[is.na(Foreign_Office1)] <- 0 #replace NA to 0

Foreign_Office1

# --------


# Creating a peace table to Q3 +"yearweek"
peace_Q12 <- peace_index %>% select(1:3,6:7) %>%  filter(date<"2001-07-31")

Q1 <- peace_Q12[,1:2] %>% group_by(date,oslosp) %>%
  summarise(n = n()) %>%  mutate(freq=n/sum(n)) %>% mutate(i=oslosp*freq) %>% 
  group_by(date) %>% summarise(q1=sum(i))

Q2 <- peace_Q12[,1:3] %>% group_by(date,oslobl) %>%
  summarise(n = n()) %>% mutate(freq=n/sum(n)) %>% mutate(i=oslobl*freq) %>% 
  group_by(date) %>% summarise(q2=sum(i))

peace_Q12 <- inner_join(Q1,Q2,by="date")
peace_Q12[peace_Q12 == 0] <- NA #replace 2 to 1
peace_Q12$q3 <- 100
peace_Q12$q3 <- as.numeric(peace_Q12$q3)

peace_Q12$yearweek <-  strftime(peace_Q12$date, format = "%Y-W%U") 

peace_Q123 <- peace_Q12 %>%rename(survey_date=date) %>%  select(5,4,2,3,1) %>% 
  right_join(calander_94_17[1:388,])


# last bind------

peace_btselm_Cope

Foreign_Office1

dim(peace_btselm_Cope)
dim(Foreign_Office1)

peace_btselm_Cope <- full_join(peace_btselm_Cope,Foreign_Office1,by="yearweek")

peace_and_attackes <- peace_btselm_Cope[,c(-19)]
colnames(peace_and_attackes)[17] <- "temperature"

--------------------------------------------------------------
library(hrbrthemes)
library(tidyverse)
library(ggplot2)
names(peace_and_attackes)
attach(peace_and_attackes)

plot(total_attacks, q3, main="Scatterplot ",
     xlab=" ", ylab=" ", pch=19)







