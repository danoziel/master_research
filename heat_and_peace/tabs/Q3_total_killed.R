# -----
# Creating a list of weeks numbers
library(data.table)
library(tinytex)
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
Q3$q3[Q3$q3 == 0] <- NA #replace 0 to NA


#btselm10: total_killed  - in general----
bind_peace10_Q3 <- at_btselem %>%
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))


bind_peace10_Q3 <- full_join (bind_peace10_Q3,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

bind_peace10_Q3 <- bind_peace10_Q3 %>%  left_join (Intifada,by="yearweek")
bind_peace10_Q3$intifada[is.na(bind_peace10_Q3$intifada)] <- 0

# weeks -total_killed
week0ago <- bind_peace10_Q3$total_killed
week1ago <- lag(week0ago, 1)
week2ago <- lag(week0ago, 2)
week3ago <- lag(week0ago, 3)
week4ago <- lag(week0ago, 4)
week5ago <- lag(week0ago, 5)
week6ago <- lag(week0ago, 6)
week7ago <- lag(week0ago, 7)
week8ago <- lag(week0ago, 8)
weeks0_8<-cbind(week0ago,week1ago,week2ago,week3ago,week4ago,week5ago,week6ago,week7ago,week8ago)

library(data.table)
bind_peace10_Q3 <- as.data.table(bind_peace10_Q3) 
weeks0_8 <- as.data.table(weeks0_8) 
weeks0_8[is.na(weeks0_8)] <- 0

bind_peace10_Q3 <- cbind(bind_peace10_Q3,weeks0_8) 

View(bind_peace10_Q3)

------------------------------------------
|               regression               |                
------------------------------------------
names()
library(jtools)
summ(model14,digits = 4)

library(sjPlot)
tab_model(model14,digits = 5)

library(flextable)

library(memisc)
mt <- mtable(model14, model18,digits = 5)

model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,data = bind_peace10_Q3)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = bind_peace10_Q3)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = bind_peace10_Q3)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = bind_peace10_Q3)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
              week5ago + week6ago + week7ago + week8ago,data = bind_peace10_Q3)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = bind_peace10_Q3)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = bind_peace10_Q3)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = bind_peace10_Q3)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = bind_peace10_Q3)
summary(model27)

model28  <- lm(q3 ~ intifada+week0ago + week1ago + week2ago + week3ago + week4ago+ 
              week5ago + week6ago + week7ago + week8ago,data = bind_peace10_Q3)
summary(model28)



library(readr)
write_csv(bind_peace10_Q3, "bind_peace10_Q3.csv")

library(sjPlot)
sjPlot::tab_model(model14,file = "model24.doc")
show_sjplot_pals()

tab_model(model14,model15,model16,model17,model18,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))

tab_model(model24,model25,model26,model27,model28,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))
          
          




-----------------------------------------------------------------------------

-----------------------------------------------------------------------------  
# btselem10_il- total_killed - total_events_located_IL----
-----------------------------------------------------------------------------
  
btselem10_il <- at_btselem %>%filter(total_events_located_IL==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))


btselem10_il <- full_join (btselem10_il,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselem10_il <- btselem10_il %>%  left_join (Intifada,by="yearweek")
btselem10_il$intifada[is.na(btselem10_il$intifada)] <- 0

# weeks -total_killed
week0ago <- btselem10_il$total_killed
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

library(data.table)
btselem10_il <- as.data.table(btselem10_il) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselem10_il <- cbind(btselem10_il,weeks0_8) 

View(btselem10_il)
print(btselem10_il)

     -------------------------------------
     |           regression              |              
     -------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,data = btselem10_il)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_il)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_il)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_il)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_il)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselem10_il)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_il)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_il)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_il)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago+ week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_il)
summary(model28)


library(sjPlot)
sjPlot::tab_model(model14,file = "model24.doc")
show_sjplot_pals()

tab_model(model14,model15,model16,model17,model18,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))

tab_model(model24,model25,model26,model27,model28,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))


library(sjPlot)
sjPlot::tab_model(model24, file = "model24.doc")


# btselem10_ye- total_killed - total_events_located_yesha----
btselem10_ye <- at_btselem %>%filter(total_events_located_yesha==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

btselem10_ye <- full_join (btselem10_ye,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselem10_ye <- btselem10_ye %>%  left_join (Intifada,by="yearweek")
btselem10_ye$intifada[is.na(btselem10_ye$intifada)] <- 0

# weeks -total_killed
week0ago <- btselem10_ye$total_killed
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

library(data.table)
btselem10_ye <- as.data.table(btselem10_ye) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselem10_ye <- cbind(btselem10_ye,weeks0_8) 

print(btselem10_ye)
View(btselem10_ye)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,data = btselem10_ye)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_ye)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_ye)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_ye)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_ye)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,data = btselem10_ye)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_ye)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_ye)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_ye)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_ye)
summary(model28)

library(sjPlot)

tab_model(model14,model15,model16,model17,model18,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))

tab_model(model24,model25,model26,model27,model28,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))




# btselem10_civ- total_killed - total_events_target_civilian----
btselem10_civ <- at_btselem %>%filter(total_events_target_civilian==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

btselem10_civ <- full_join (btselem10_civ,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselem10_civ <- btselem10_civ %>%  left_join (Intifada,by="yearweek")
btselem10_civ$intifada[is.na(btselem10_civ$intifada)] <- 0

# weeks -total_killed
week0ago <- btselem10_civ$total_killed
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

library(data.table)
btselem10_civ <- as.data.table(btselem10_civ) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselem10_civ <- cbind(btselem10_civ,weeks0_8) 

print(btselem10_civ)
View(btselem10_civ)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselem10_civ)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_civ)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_civ)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_civ)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_civ)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselem10_civ)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_civ)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_civ)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_civ)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_civ)
summary(model28)

library(sjPlot)
tab_model(model14,model15,model16,model17,model18,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))

tab_model(model24,model25,model26,model27,model28,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 7 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))


# btselem10_sec- total_killed - total_events_target_security_forces----
btselem10_sec <- at_btselem %>%filter(total_events_target_security_forces==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_killed=sum(total_killed, na.rm = TRUE))

btselem10_sec <- full_join (btselem10_sec,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselem10_sec <- btselem10_sec %>%  left_join (Intifada,by="yearweek")
btselem10_sec$intifada[is.na(btselem10_sec$intifada)] <- 0

# weeks -total_killed
week0ago <- btselem10_sec$total_killed
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

library(data.table)
btselem10_sec <- as.data.table(btselem10_sec) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselem10_sec <- cbind(btselem10_sec,weeks0_8) 

print(btselem10_sec)
View(btselem10_sec)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselem10_sec)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_sec)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_sec)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_sec)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_sec)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselem10_sec)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselem10_sec)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselem10_sec)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselem10_sec)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselem10_sec)
summary(model28)

library(sjPlot)
library(sjPlot)
tab_model(model14,model15,model16,model17,model18,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 5 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))

tab_model(model24,model25,model26,model27,model28,digits = 4,
          dv.labels = c("last 4 weeks","last 5 weeks","last 6 weeks",
                        "last 5 weeks","last 8 weeks"),
          show.ci = FALSE,CSS = list(
            css.lasttablerow = 'font-weight: bold;', 
            css.firsttablecol = 'font-weight: bold;',
            css.summary = 'color: royalblue;'))








