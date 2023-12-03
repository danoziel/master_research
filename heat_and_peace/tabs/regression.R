
library(sjPlot)
library(sjmisc)
library(sjlabelled) 

[1] "yearweek"                            "total_killed"                       
[3] "total_events_located_IL"             "total_events_located_yesha"         
[5] "total_events_target_civilian"        "total_events_target_security_forces"
[7] "total_attacks"                       "q1"                                 
[9] "q2"                                  "survey_date"                        
[11] "temperature"                         "humidity"                           
[13] "week_range"                          "week_number"                        
[15] "week0ago"                            "week1ago"                           
[17] "week2ago"                            "week3ago"                           
[19] "week4ago" 


library(jtools)
dfQ1 <- bt_pe_me %>% filter(q1>0)
model  <- lm(q1 ~ week0ago+week1ago + week2ago + week3ago + week4ago,data = dfQ1)
summary(model)
summ(model)

dfQ2 <- bt_pe_me %>% filter(q2>0)
model2  <- lm(q2 ~ week0ago+week1ago + week2ago + week3ago + week4ago,data = dfQ2)
summary(model2)
summ(model2)

# total_events_located_IL - bt_il
yearAweek <- bt_pe_me %>% select(1)
bt_il <- at_btselem%>% filter(total_events_located_IL==1) %>%group_by(yearweek) %>% 
  summarise(total_killed=sum(`number of killed`)) %>% right_join(yearAweek) %>%
  left_join(peace_Q1Q2)

week0ago <- bt_il$total_killed
week1ago <- lag(week0ago, 1)
week2ago <- lag(week0ago, 2)
week3ago <- lag(week0ago, 3)
week4ago <- lag(week0ago, 4)
weeks01234 <- cbind(week0ago,week1ago,week2ago,week3ago,week4ago)

weeks01234 <- weeks01234 %>% data.frame()
weeks01234[is.na(weeks01234)] <- 0
bt_il <- cbind(bt_il,weeks01234) 

dfQ1 <- bt_il %>% filter(q1>0)
model_il  <- lm(q1 ~ week1ago + week2ago + week3ago + week4ago,data = dfQ1)
summary(model_il)
summ(model_il)

dfQ2 <- bt_il %>% filter(q2>0)
model2_il  <- lm(q2 ~week0ago+ week1ago + week2ago + week3ago + week4ago,data = dfQ2)
summary(model2_il)
summ(model2_il)








