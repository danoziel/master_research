# ------
library(tidyverse)
library(data.table)

class(peace_index$date)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")

class(peace_index$date1)
peace_index$date1 <- as.numeric(peace_index$date1)

class(peace_index$oslobl)
class(peace_index$oslosp)

class(peace_index$negot_sp)
peace_index$negot_sp <- as.numeric(peace_index$negot_sp)

class(peace_index$negot_bl)
peace_index$negot_bl <- as.numeric(peace_index$negot_bl)

class(peace_index$peacesp)

class(peace_index$gender)
peace_index$gender <- as.numeric(peace_index$gender)

class(peace_index$age)

class(peace_index$relig )



political_spectrum,relig,incom

peace_Q16 <- peace_index %>%
  select(date,date1,survey_year,
         oslosp,oslobl,negot_sp,negot_bl,peacesp,peacebl,
         gender,age,educ,party,relig,inc,pvote99,pvote03,pvote06,pvote09)


peace_Q16$oslosp [is.na(peace_Q16$oslosp)] <- 0 #replace NA to 0
peace_Q16$oslobl [is.na(peace_Q16$oslobl)] <- 0

peace_Q16$negot_sp [is.na(peace_Q16$negot_sp)] <- 0
peace_Q16$negot_bl [is.na(peace_Q16$negot_bl)] <- 0

peace_Q16$peacesp [is.na(peace_Q16$peacesp)] <- 0
peace_Q16$peacebl [is.na(peace_Q16$peacebl)] <- 0

# political spectrum----


peace_Q16b <- peace_Q16 %>%  
  mutate(political_spectrum=case_when(
    date1 >= 1 & date1 <= 27 & party %in% c(1,4,5,6,7,8,9,12,13)~1,#election 92
    date1 >= 1 & date1 <= 27 & party %in% c(2,3,10,11)~2,
    date1 >= 1 & date1 <= 27 & party %in% c(14,15,16,17)~4,
    
    date1 >= 28 & date1 <= 62 & party %in% c(1,4,5,6,8,9,12,13)~1, #election 96
    date1 >= 28 & date1 <= 62 & party %in% c(2,3,10,11)~2, #election 96
    date1 >= 28 & date1 <= 62 & party %in% c(7)~3, #election 96
    date1 >= 28 & date1 <= 62 & party %in% c(14,15,16,17)~4, #election 96
    
    date1 >= 63 & date1 <= 107 & pvote99 %in% c(2,3,5,6,9,11,12,16)~1, #election 99
    date1 >= 63 & date1 <= 107 & pvote99 %in% c(1,4,7,31,32,33,34)~2, #election 99
    date1 >= 63 & date1 <= 107 & pvote99 %in% c(8,10,14,15,17)~3, #election 99
    date1 >= 63 & date1 <= 107 & pvote99 %in% c(18,19,20,21,22)~4, #election 99
    
    date1 >= 108 & date1 <= 145 & pvote03 %in% c(2,3,5,6,9,10)~1,#election 03
    date1 >= 108 & date1 <= 145 & pvote03 %in% c(1,4,7,11,12,13)~2,#election 03
    date1 >= 108 & date1 <= 145 & pvote03 %in% c(8)~3,#election 03
    date1 >= 108 & date1 <= 145 & pvote03 %in% c(14,15,16,17)~4,#election 03
    
    date1 >= 146 & date1 <= 179 & pvote06 %in% c(2,4,6,7,9)~1, #election 06
    date1 >= 146 & date1 <= 179 & pvote06 %in% c(1,5,7,10,11,12)~2, #election 06
    date1 >= 146 & date1 <= 179 & pvote06 %in% c(3,8,13,14,15,16)~3, #election 06
    date1 >= 146 & date1 <= 179 & pvote06 %in% c(17,18,19,20)~4 ,  #election 06
    
    date1 >= 180 & date1 <= 193 & pvote09 %in% c(2,4,6,5,8,9)~1, #election 09
    date1 >= 180 & date1 <= 193 & pvote09 %in% c(1,5,7,11,12,13)~2, #election 09
    date1 >= 180 & date1 <= 193 & pvote09 %in% c(3,10,14,15,16)~3, #election 09
    date1 >= 180 & date1 <= 193 & pvote09 %in% c(17,18,19,20)~4,#election 09
    
    date1 >= 194 & date1 <= 223 & party %in% c(2,4,6,5,8,9)~1, #also election 09
    date1 >= 194 & date1 <= 223 & party %in% c(1,5,7,11,12,13)~2, #also election 09
    date1 >= 194 & date1 <= 223 & party %in% c(3,10,14,15,16)~3, #also election 09
    date1 >= 194 & date1 <= 223 & party %in% c(17,18,19,20,21,22)~4, #also election 09
    
    date1 >= 224 & date1 <= 247 & party %in% c(3,4,5,11,7,10,16)~1, #election 13
    date1 >= 224 & date1 <= 247 & party %in% c(2,6,13,15,14)~2, #election 13
    date1 >= 224 & date1 <= 247 & party %in% c(1,12,9,8,20,18)~3, #election 13
    date1 >= 224 & date1 <= 247 & party %in% c(17,21:39)~4, #election 13
    
    date1 >= 248 & date1 <= 273 & party %in% c(2,3,4,5,7,9,10)~1, #election 15
    date1 >= 248 & date1 <= 273 & party %in% c(1,6,11,13,25)~2, #election 15
    date1 >= 248 & date1 <= 273 & party %in% c(8,12,26)~3, #election 15
    date1 >= 248 & date1 <= 273 & party %in% c(14:24,27:30)~4, #election 15
    
  ))

peace_Q16b <- peace_Q16b %>%
  select(date, date1, survey_year,
         oslosp, oslobl, negot_sp, negot_bl, peacesp, peacebl,
         gender, age, educ,party, political_spectrum)

p_relig_inc <- peace_index %>% select(relig,inc)
 
peace_Q16b <-cbind(peace_Q16b,p_relig_inc)

peace_Q16bc <- peace_Q16b %>% 
  select(date, date1, survey_year, oslosp, oslobl, negot_sp, negot_bl, peacesp,peacebl,
         gender, age, educ, party, relig, incom, political_spectrum )





peace_index_17_18 %>%
  select(date, date1, survey_year, oslosp, oslobl, negot_sp, negot_bl, peacesp,peacebl,
         gender, age, age_range, educ, party, relig, incom, political_spectrum )

#X17_05_2011_2913----xx17_5_11----

X17_05_2011_2913$date <- "2011-05-17"
class(X17_05_2011_2913$date)
X17_05_2011_2913$date <-  as.Date(X17_05_2011_2913$date, "%Y-%m-%d")

X17_05_2011_2913$date1 <- rep(203.5)
class(X17_05_2011_2913$date1)

X17_05_2011_2913$survey_year <- rep(2011)

X17_05_2011_2913$oslosp <- as.numeric(NA)
class(X17_05_2011_2913$oslosp)

X17_05_2011_2913$oslobl <- as.numeric(NA)
class(X17_05_2011_2913$oslobl)

X17_05_2011_2913 <- X17_05_2011_2913 %>% 
  rename(negot_sp = q1,
         negot_bl = q2) 

class(X17_05_2011_2913$negot_sp)
X17_05_2011_2913$negot_sp <- as.numeric(X17_05_2011_2913$negot_sp)
X17_05_2011_2913$negot_bl <- as.numeric(X17_05_2011_2913$negot_bl)

X17_05_2011_2913$peacesp <- as.numeric(NA)
X17_05_2011_2913$peacebl <- as.numeric(NA)
class(X17_05_2011_2913$peacebl)

class(X17_05_2011_2913$q25)
class(X17_05_2011_2913$gender)

X17_05_2011_2913 <- X17_05_2011_2913 %>% 
  rename(gender = q23,
         age = q24,
         educ = q26,
         political_spectrum = q22,
         relig = q25,
         incom = q30)

X17_05_2011_2913$incom <- as.numeric(X17_05_2011_2913$incom)
X17_05_2011_2913$gender <- as.numeric(X17_05_2011_2913$gender)


xx17_5_11 <- X17_05_2011_2913 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,educ,political_spectrum,relig,incom)

xx17_5_11 <- xx17_5_11 %>% rename(party=political_spectrum)

xx17_5_11 <- xx17_5_11 %>%
  mutate(political_spectrum=case_when(
    party %in% c(2,4,6,5,8,9)~1,
    party %in% c(1,5,7,11,12,13)~2,
    party %in% c(3,10,14,15,16)~3,
    party %in% c(17,18,19,20,21,22)~4 ))
    
    
  


# X29_11_12----
xx29_11_12 <- X29_11_12 %>% 
  rename(negot_sp = Q1,
         negot_bl = Q2,
         gender = Q1c,
         age = Q18,
         educ = Q21,
         political_spectrum = Q15,
         relig = Q19,
         incom = Q26)

xx29_11_12$date <- "2012-11-29"
class(xx29_11_12$date)
xx29_11_12$date <-  as.Date(xx29_11_12$date, "%Y-%m-%d")

xx29_11_12$date1 <- rep(220.5)
class(xx29_11_12$date1)

xx29_11_12$survey_year <- rep(2012)
class(xx29_11_12$survey_year)

xx29_11_12$oslosp <- as.numeric(NA)
xx29_11_12$oslobl <- as.numeric(NA)
xx29_11_12$peacesp <- as.numeric(NA)
xx29_11_12$peacebl <- as.numeric(NA)
class(xx29_11_12$oslosp)

xx29_11_12 <- xx29_11_12 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,educ,political_spectrum,relig,incom)

xx29_11_12 <- xx29_11_12 %>% rename(party=political_spectrum)

xx29_11_12 <- xx29_11_12 %>%
  mutate(political_spectrum=case_when(
    party %in% c(1,2)~1,
    party %in% c(4,5)~2,
    party %in% c(3)~3,
    party %in% c(6,7)~4 ))







