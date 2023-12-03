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
Q3$q3[Q3$q3 == 0] <- NA #replace 0 to NA

------------------------------------------------------------------------------------
  
#btselm20: total_attacks  - in general----

------------------------------------------------------------------------------------
btselm20 <- at_btselem %>% 
  group_by(yearweek) %>% 
  summarise(total_attacks=sum(total_attacks, na.rm = TRUE))

btselm20 <- full_join (btselm20,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselm20 <- btselm20 %>%  left_join (Intifada,by="yearweek")
btselm20$intifada[is.na(btselm20$intifada)] <- 0

# weeks -total_attacks
week0ago <- btselm20$total_attacks
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
btselm20 <- as.data.table(btselm20) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselm20 <- cbind(btselm20,weeks0_8) 

print(btselm20)
View(btselm20)

attach(btselm20)
plot(week3ago, q3)

plot(NULL, ylim=c(1,2.5),xlim=c(0,15), 
     xlab = " Attackes 3 week ago", 
     ylab = "Peace Index Q3", 
     font.lab = 6,
     main = "Peace Index vs. Attackes",
     font.main = 7,
     col.main = "black",)
abline(v = 0:15, col = "aliceblue", lwd = 200)
abline(v = 0:15, col = "white")
abline(h=0:4, col = "white")
points(week3ago, q3, 
       pch = 20,
       cex = 1,col="darkblue")











----------------------------------------------------------------------------
                              regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselm20)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselm20)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselm20)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselm20)
summary(model28)

library(sjPlot)
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


-----------------------------------------------------------------------------------
------------------------------------------------------------------------------------
  
  #btselm20_il: total_attacks  - total_events_located_IL----

------------------------------------------------------------------------------------
btselm20_il <- at_btselem %>%filter(total_events_located_IL==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_attacks=sum(total_attacks, na.rm = TRUE))

btselm20_il <- full_join (btselm20_il,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselm20_il <- btselm20_il %>%  left_join (Intifada,by="yearweek")
btselm20_il$intifada[is.na(btselm20_il$intifada)] <- 0

# weeks -total_attacks
week0ago <- btselm20_il$total_attacks
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
btselm20_il <- as.data.table(btselm20_il) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselm20_il <- cbind(btselm20_il,weeks0_8) 

print(btselm20_il)
View(btselm20_il)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselm20_il)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_il)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_il)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_il)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_il)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselm20_il)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_il)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_il)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_il)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_il)
summary(model28)

library(sjPlot)
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

----------------------------------------------------------------------------
--------------------------------------------------------------------------
  
  #btselm20_ye: total_attacks  - total_events_located_yesha----

------------------------------------------------------------------------------------
  btselm20_ye <- at_btselem %>%filter(total_events_located_yesha==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_attacks=sum(total_attacks, na.rm = TRUE))

btselm20_ye <- full_join (btselm20_ye,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselm20_ye <- btselm20_ye %>%  left_join (Intifada,by="yearweek")
btselm20_ye$intifada[is.na(btselm20_ye$intifada)] <- 0

# weeks -total_attacks
week0ago <- btselm20_ye$total_attacks
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
btselm20_ye <- as.data.table(btselm20_ye) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselm20_ye <- cbind(btselm20_ye,weeks0_8) 

print(btselm20_ye)
View(btselm20_ye)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselm20_ye)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_ye)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_ye)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_ye)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_ye)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselm20_ye)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_ye)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_ye)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_ye)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_ye)
summary(model28)

library(sjPlot)
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

----------------------------------------------------------------------------
  --------------------------------------------------------------------------
  
  #btselm20_civ: total_attacks  - total_events_target_civilian----

------------------------------------------------------------------------------------
  btselm20_civ <- at_btselem %>%filter(total_events_target_civilian==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_attacks=sum(total_attacks, na.rm = TRUE))

btselm20_civ <- full_join (btselm20_civ,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselm20_civ <- btselm20_civ %>%  left_join (Intifada,by="yearweek")
btselm20_civ$intifada[is.na(btselm20_civ$intifada)] <- 0

# weeks -total_attacks
week0ago <- btselm20_civ$total_attacks
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
btselm20_civ <- as.data.table(btselm20_civ) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselm20_civ <- cbind(btselm20_civ,weeks0_8) 

print(btselm20_civ)
View(btselm20_civ)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselm20_civ)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_civ)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_civ)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_civ)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_civ)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselm20_civ)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_civ)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_civ)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_civ)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_civ)
summary(model28)

library(sjPlot)
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

----------------------------------------------------------------------------
  --------------------------------------------------------------------------
  
  #btselm20_sec: total_attacks  - total_events_target_security_forces----

------------------------------------------------------------------------------------
  btselm20_sec <- at_btselem %>%filter(total_events_target_security_forces==1) %>% 
  group_by(yearweek) %>% 
  summarise(total_attacks=sum(total_attacks, na.rm = TRUE))

btselm20_sec <- full_join (btselm20_sec,Q3,by="yearweek") %>%
  right_join (yw_94_17,by="yearweek") %>% 
  arrange(yearweek) %>%
  filter(yearweek>"2001-W29")

# Intifada_Al_Aqsa "2000-09-28" - "2005-02-08" 
Intifada <- yw_94_17 %>% filter(yearweek>="2000-W40",yearweek<"2005-W07") %>%
  group_by(yearweek) %>% count() %>% rename(intifada=n)

btselm20_sec <- btselm20_sec %>%  left_join (Intifada,by="yearweek")
btselm20_sec$intifada[is.na(btselm20_sec$intifada)] <- 0

# weeks -total_attacks
week0ago <- btselm20_sec$total_attacks
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
btselm20_sec <- as.data.table(btselm20_sec) 
weeks0_8 <- as.data.table(weeks0_8) 

weeks0_8[is.na(weeks0_8)] <- 0
btselm20_sec <- cbind(btselm20_sec,weeks0_8) 

print(btselm20_sec)
View(btselm20_sec)

----------------------------------------------------------------------------
  regression                               
----------------------------------------------------------------------------
  model14 <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago,
                data = btselm20_sec)
summary(model14)

model15  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_sec)
summary(model15)

model16  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_sec)
summary(model16)

model17  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_sec)
summary(model17)

model18  <- lm(q3 ~ week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_sec)
summary(model18)

# intifada

model24  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago,
               data = btselm20_sec)
summary(model24)

model25  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago ,data = btselm20_sec)
summary(model25)

model26  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago ,data = btselm20_sec)
summary(model26)

model27  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+
                 week5ago + week6ago + week7ago ,data = btselm20_sec)
summary(model27)

model28  <- lm(q3 ~ intifada +week0ago + week1ago + week2ago + week3ago + week4ago+ 
                 week5ago + week6ago + week7ago + week8ago,data = btselm20_sec)
summary(model28)

library(sjPlot)
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

