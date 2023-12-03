# -----
# Creating a list of weeks numbers
library(data.table)
date <- seq(as.IDate("1994-06-22"), as.IDate("2017-07-01"), 1)
dt <- data.table(date=date)
dt$yearweek <-  strftime(date, format = "%Y-W%U") # this table: "date" and "yearweek"
yw_94_17 <- dt[,2] %>% count(yearweek) %>% select(1) # only "yearweek"

# Creating a peace table to Q3 +"yearweek"
peace_Q3Q4 <- peace_index %>% select(1,4:7) %>%  filter(date>="2001-07-31")
peace_Q3Q4 [is.na(peace_Q3Q4)] <- 0 #replace NA to 0

Q3 <- peace_Q3Q4[,1:2] %>% group_by(date,negot_sp) %>%
  summarise(n = n()) %>%  mutate(freq=n/sum(n)) %>% mutate(i=negot_sp*freq) %>% 
  group_by(date) %>% summarise(q3=sum(i))
Q3$date <- as.IDate(Q3$date)
Q3 <- left_join(Q3,dt,by="date") %>% select(3,2,1)%>% rename(survey_date =date)


#btselm10: total_killed  - in general----
btselem10 <- at_btselem %>%
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))


bind_peace10_Q3 <- full_join (btselem10,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29",yearweek<"2018-W20")
  
# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

bind_peace10_Q3 <- bind_peace10_Q3 %>%  left_join (Intifada,by="yearweek")
bind_peace10_Q3$intifada[is.na(bind_peace10_Q3$intifada)] <- 0

# weeks -total_killed

bind11 <- bind_peace10_Q3%>% select(1,3,9:11)

week0ago <- bind11$total_killed
week1ago <- lag(week0ago, 1)
week2ago <- lag(week0ago, 2)
week3ago <- lag(week0ago, 3)
week4ago <- lag(week0ago, 4)
week5ago <- lag(week0ago, 5)
week6ago <- lag(week0ago, 6)
week7ago <- lag(week0ago, 7)
week8ago <- lag(week0ago, 8)
weeks0_8 <- cbind(week0ago,week1ago,week2ago,week3ago,week4ago,
                    week5ago,week6ago,week7ago,week8ago)

bind11 <- as.data.table(bind11) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
bind11 <- cbind(bind11,weeks0_8) 

print(bind11)
View(bind11)
# weeks - number of attacks

----------------------------------------------------------------------------
                              regression                               
----------------------------------------------------------------------------
names(bind11)
library(jtools)
bind11$q3[bind11$q3 == 0] <- NA #replace 0 to NA

model1  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago ,data = bind11)
summary(model1)

model2  <- lm(q3 ~ intifada + week1ago + week2ago + week3ago + week4ago ,data = bind11)
summary(model2)

------------------------------------------------------
# btselem10_il- total_killed - total_events_located_IL----
btselem10_il <- at_btselem %>%filter(total_events_located_IL==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

btselem10 <- at_btselem %>%
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))


bind_peace10_Q3 <- full_join (btselem10,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29",yearweek<"2018-W20")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

bind_peace10_Q3 <- bind_peace10_Q3 %>%  left_join (Intifada,by="yearweek")
bind_peace10_Q3$intifada[is.na(bind_peace10_Q3$intifada)] <- 0

# weeks -total_killed

bind11 <- bind_peace10_Q3%>% select(1,3,9:11)

week0ago <- bind11$total_killed
week1ago <- lag(week0ago, 1)
week2ago <- lag(week0ago, 2)
week3ago <- lag(week0ago, 3)
week4ago <- lag(week0ago, 4)
week5ago <- lag(week0ago, 5)
week6ago <- lag(week0ago, 6)
week7ago <- lag(week0ago, 7)
week8ago <- lag(week0ago, 8)
weeks0_8 <- cbind(week0ago,week1ago,week2ago,week3ago,week4ago,
                  week5ago,week6ago,week7ago,week8ago)

bind11 <- as.data.table(bind11) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
bind11 <- cbind(bind11,weeks0_8) 

print(bind11)
View(bind11)
# weeks - number of attacks

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  names(bind11)
library(jtools)
bind11$q3[bind11$q3 == 0] <- NA #replace 0 to NA

model1  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago ,data = bind11)
summary(model1)

model2  <- lm(q3 ~ intifada + week1ago + week2ago + week3ago + week4ago ,data = bind11)
summary(model2)

# btselem10_ye- total_killed - total_events_located_yesha----
btselem10_ye <- at_btselem %>%filter(total_events_located_yesha==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

# btselem10_civ- total_killed - total_events_target_civilian----
btselem10_civ <- at_btselem %>%filter(total_events_target_civilian==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

# btselem10_sec- total_killed - total_events_target_security_forces----
btselem10il_sec <- at_btselem %>%filter(total_events_target_security_forces==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))






