library(tidyverse)
# D X05_2017 ----
xd05_2017 <- X05_2017 %>%
  filter(!is.na(Q3C)) %>%
  rename(negot_sp = Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q26 ,
         age_range =Q2C ,
         educ =Q31 ,
         political_spectrum =Q24 ,
         relig =Q3C ,
         incom =Q30 )

xd05_2017$date <- "2017-05-29"
class(xd05_2017$date)
xd05_2017$date <-  as.Date(xd05_2017$date, "%Y-%m-%d")

xd05_2017$month <- 05

xd05_2017$survey_year <- rep(2017)
class(xd05_2017$survey_year)

xd05_2017$oslosp <- as.numeric(NA)
xd05_2017$oslobl <- as.numeric(NA)
xd05_2017$peacesp <- as.numeric(NA)
xd05_2017$peacebl <- as.numeric(NA)
class(xd05_2017$oslosp)

xd05_2017$date1 <- as.numeric(274)

xd05_2017 <- xd05_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# D X06_2017 ----
xd06_2017 <- X06_2017 %>%
  rename(negot_sp = Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q26 ,
         age_range =Q2C ,
         educ =Q31 ,
         political_spectrum =Q24 ,
         relig =Q3C ,
         incom =Q30 )

xd06_2017$date <- "2017-06-28"
class(xd06_2017$date)
xd06_2017$date <-  as.Date(xd06_2017$date, "%Y-%m-%d")

xd06_2017$month <- 06

xd06_2017$survey_year <- rep(2017)
class(xd06_2017$survey_year)

xd06_2017$oslosp <- as.numeric(NA)
xd06_2017$oslobl <- as.numeric(NA)
xd06_2017$peacesp <- as.numeric(NA)
xd06_2017$peacebl <- as.numeric(NA)
class(xd06_2017$oslosp)

xd06_2017$date1 <- as.numeric(275)

xd06_2017 <- xd06_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)




# D X07_2017 ----

xx07_2017 <- X07_2017 %>% filter(!is.na(relig)) 

xx07_2017$age <- as.numeric(NA)

xx07_2017$oslosp <- as.numeric(NA)
xx07_2017$oslobl <- as.numeric(NA)
xx07_2017$peacesp <- as.numeric(NA)
xx07_2017$peacebl <- as.numeric(NA)
class(xx07_2017$oslosp)

xx07_2017$date <- "2017-07-25"
class(xx07_2017$date)
xx07_2017$date <-  as.Date(xx07_2017$date, "%Y-%m-%d")

xd07_2017$survey_year <- rep(2017)
class(xd07_2017$survey_year)

xd07_2017$date1 <- as.numeric(276)

xd07_2017 <- xd07_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom)



# D X08_2017 ----

xx08_2017 <- X08_2017 %>% filter(!is.na(Q3C)) %>% 
  rename(negot_sp =Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q33 ,
         age_range =Q2C ,
         educ =Q38 ,
         political_spectrum =Q31 ,
         relig =Q3C ,
         incom =Q37 )

xx08_2017$oslosp <- as.numeric(NA)
xx08_2017$oslobl <- as.numeric(NA)
xx08_2017$peacesp <- as.numeric(NA)
xx08_2017$peacebl <- as.numeric(NA)
class(xx08_2017$oslosp)

xx08_2017$date <- "2017-08-30"
class(xx08_2017$date)
xx08_2017$date <-  as.Date(xx08_2017$date, "%Y-%m-%d")

xd08_2017$survey_year <- rep(2017)
class(xx08_2017$survey_year)

xd08_2017$date1 <- as.numeric(277)

xd08_2017 <- xd08_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# D  X09_2017 ----
xx09_2017 <- X09_2017 %>%
  filter(!is.na(Q3c)) %>%
  rename(negot_sp = Q5 ,
         negot_bl =Q6 ,
         gender =Q1c ,
         age_range =Q2c ,
         educ =Q26 ,
         political_spectrum =Q19 ,
         relig =Q3c ,
         incom =Q25)

xx09_2017$age <- as.numeric(NA)

xx09_2017$survey_year <- rep(2017)
class(xx09_2017$survey_year)

xx09_2017$oslosp <- as.numeric(NA)
xx09_2017$oslobl <- as.numeric(NA)
xx09_2017$peacesp <- as.numeric(NA)
xx09_2017$peacebl <- as.numeric(NA)
class(xx09_2017$oslosp)

xx09_2017$date1 <- as.numeric(278)

xx09_2017$date <- "2017-09-27"
class(xx09_2017$date)
xx09_2017$date <-  as.Date(xx09_2017$date, "%Y-%m-%d")

xx09_2017 <- xx09_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# D X10_2017 ----
xd10_2017 <- X10_2017 %>% filter(!is.na(Q3C)) %>%
  rename(negot_sp =Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q22 ,
         age_range =Q2C ,
         educ =Q27 ,
         political_spectrum =Q20 ,
         relig =Q3C ,
         incom =Q26 )

xd10_2017$date <- "2017-10-31"
class(xd10_2017$date)
xd10_2017$date <-  as.Date(xd10_2017$date, "%Y-%m-%d")

xd10_2017$date1 <- as.numeric(279)

xd10_2017$survey_year <- rep(2017)
class(xd10_2017$survey_year)

xd10_2017$oslosp <- as.numeric(NA)
xd10_2017$oslobl <- as.numeric(NA)
xd10_2017$peacesp <- as.numeric(NA)
xd10_2017$peacebl <- as.numeric(NA)
class(xd10_2017$oslosp)

xd10_2017 <- xd10_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

  


#  D X11_2017 ----
xx11_2017 <- X11_2017 %>% filter(!is.na(Q3C)) %>% 
  rename(negot_sp =Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q32 ,
         age_range =Q2C ,
         educ =Q37 ,
         political_spectrum =Q30 ,
         relig =Q3C ,
         incom =Q36 )

xx11_2017$month <- 11

xx11_2017$survey_year <- rep(2017)
class(xx11_2017$survey_year)

xx11_2017$oslosp <- as.numeric(NA)
xx11_2017$oslobl <- as.numeric(NA)
xx11_2017$peacesp <- as.numeric(NA)
xx11_2017$peacebl <- as.numeric(NA)
class(xx11_2017$oslosp)

xx11_2017$date1 <- as.numeric(280)

xx11_2017$date <- "2017-11-30"
class(xx11_2017$date)
xx11_2017$date <-  as.Date(xx11_2017$date, "%Y-%m-%d")

xx11_2017 <- xx11_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)





# D X12_2017 ----
xd12_2017 <- X12_2017 %>%
  filter(!is.na(Q3c)) %>%
  rename(negot_sp = Q5 ,
         negot_bl =Q6 ,
         gender =Q1c ,age = Q27,
         age_range =Q2c ,
         educ =Q32 ,
         political_spectrum =Q25 ,
         relig =Q3c ,
         incom =Q31)

xd12_2017$date <- "2017-12-27"
class(xd12_2017$date)
xd12_2017$date <-  as.Date(xd12_2017$date, "%Y-%m-%d")

xd12_2017$month <- 12

xd12_2017$survey_year <- rep(2017)
class(xd12_2017$survey_year)

xd12_2017$oslosp <- as.numeric(NA)
xd12_2017$oslobl <- as.numeric(NA)
xd12_2017$peacesp <- as.numeric(NA)
xd12_2017$peacebl <- as.numeric(NA)
class(xd12_2017$oslosp)

xd12_2017$date1 <- as.numeric(281)
  
xd12_2017 <- xd12_2017 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)







-----------------------------------------------------------------
  
  
#  D X04_2018 -----
xx04_2018 <- X04_2018 %>%
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age =Q32,
         educ =Q37,
         political_spectrum =Q30 ,
         relig =Q3c ,
         incom =Q36 )

xx04_2018$age_range <- Peace_Index_April_2018_weighted_jews$Q2c

xx04_2018$month <- 04

xx04_2018$date <- "2018-04-26"
class(xx04_2018$date)
xx04_2018$date <-  as.Date(xx04_2018$date, "%Y-%m-%d")


xx04_2018$survey_year <- rep(2018)
class(xx04_2018$survey_year)

xx04_2018$oslosp <- as.numeric(NA)
xx04_2018$oslobl <- as.numeric(NA)
xx04_2018$peacesp <- as.numeric(NA)
xx04_2018$peacebl <- as.numeric(NA)
class(xx04_2018$oslosp)

xx04_2018$date1 <- as.numeric(285)

xx04_2018 <- xx04_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# D  X06_2018 ----
xx06_2018 <- X06_2018 %>%filter(!is.na(Q3c)) %>%
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range =Q2c ,
         educ =Q32 ,
         political_spectrum =Q25 ,
         relig = Q3c,
         incom =Q31 )

xx06_2018$age <- as.numeric(NA)
xx06_2018$month <-as.numeric(06)
xx06_2018$survey_year <- rep(2018)
class(xx06_2018$survey_year)

xx06_2018$oslosp <- as.numeric(NA)
xx06_2018$oslobl <- as.numeric(NA)
xx06_2018$peacesp <- as.numeric(NA)
xx06_2018$peacebl <- as.numeric(NA)
class(xx06_2018$oslosp)

xx06_2018$date1 <- as.numeric(287)

xx06_2018$date <- "2018-06-28"
class(xx06_2018$date)
xx06_2018$date <-  as.Date(xx06_2018$date, "%Y-%m-%d")


xx06_2018 <- xx06_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# D  X07_2018 ----
xx07_2018 <- X07_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range =Q2c ,
         educ =Q35 ,
         political_spectrum =Q28 ,
         relig = Q3c,
         incom =Q34 )


xx07_2018$month <- 07

xx07_2018$age <- as.numeric(NA)

xx07_2018$survey_year <- rep(2018)
class(xx04_2018$survey_year)

xx07_2018$oslosp <- as.numeric(NA)
xx07_2018$oslobl <- as.numeric(NA)
xx07_2018$peacesp <- as.numeric(NA)
xx07_2018$peacebl <- as.numeric(NA)
class(xx07_2018$oslosp)

xx07_2018$date1 <- as.numeric(288)

xx07_2018$date <- "2018-07-26"
class(xx07_2018$date)
xx07_2018$date <-  as.Date(xx07_2018$date, "%Y-%m-%d")

xx07_2018 <- xx07_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)



# D  X10_2018 ----

xx10_2018 <- X10_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range =Q2c ,
         educ =Q32 ,
         political_spectrum =Q25 ,
         relig = Q3c,
         incom =Q31 )

xx10_2018$age <- as.numeric(NA) 

xx10_2018$month <- 10

xx10_2018$survey_year <- rep(2018)
class(xx10_2018$survey_year)

xx10_2018$oslosp <- as.numeric(NA)
xx10_2018$oslobl <- as.numeric(NA)
xx10_2018$peacesp <- as.numeric(NA)
xx10_2018$peacebl <- as.numeric(NA)
class(xx10_2018$oslosp)

xx10_2018$date1 <- as.numeric(291)

xx10_2018$date <- "2018-10-18"
class(xx10_2018$date)
xx10_2018$date <-  as.Date(xx10_2018$date, "%Y-%m-%d")


xx10_2018 <- xx10_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

# D X11_2018 ----
xd11_2018 <- X11_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
       negot_bl = Q6,
       gender = Q1c,
       age_range =Q2c ,
       educ =Q33 ,
       political_spectrum =Q26 ,
       relig = Q3c,
       incom =Q32 )

xd11_2018$age <- as.numeric(NA) 

xd11_2018$date1 <- as.numeric(292)

xd11_2018$survey_year <- rep(2018)
class(xd11_2018$survey_year)

xd11_2018$oslosp <- as.numeric(NA)
xd11_2018$oslobl <- as.numeric(NA)
xd11_2018$peacesp <- as.numeric(NA)
xd11_2018$peacebl <- as.numeric(NA)
class(xd11_2018$oslosp)

xd11_2018$date <- "2018-11-28"
class(xd11_2018$date)
xd11_2018$date <-  as.Date(xd11_2018$date, "%Y-%m-%d")

xd11_2018 <- xd11_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)






#  D X05_2018 ----
xx05_2018 <- X05_2018 %>%
  filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range= Q2c,
         educ =Q34,
         political_spectrum =Q27 ,
         relig =Q3c ,
         incom =Q33 )

xx05_2018$age <- as.numeric(NA)

xx05_2018$survey_year <- rep(2018)
class(xx05_2018$survey_year)

xx05_2018$oslosp <- as.numeric(NA)
xx05_2018$oslobl <- as.numeric(NA)
xx05_2018$peacesp <- as.numeric(NA)
xx05_2018$peacebl <- as.numeric(NA)
class(xx05_2018$oslosp)

xx05_2018$date1 <- as.numeric(286)

xx05_2018$date <- "2018-05-31"
class(xx05_2018$date)
xx05_2018$date <-  as.Date(xx05_2018$date, "%Y-%m-%d")


xx05_2018 <- xx05_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)




# D X03_2018 ----

xd03_2018 <- X03_2018 %>%
  filter(!is.na(Q3c)) %>%
  rename(negot_sp = Q5 ,
         negot_bl =Q6 ,
         gender =Q1c ,
         age_range =Q2c ,
         educ =Q37 ,
         political_spectrum =Q30 ,
         relig =Q3c ,
         incom =Q36)

xd03_2018$age <- as.numeric(NA)

xd03_2018$date <- "2018-03-15"
class(xd03_2018$date)
xd03_2018$date <-  as.Date(xd03_2018$date, "%Y-%m-%d")

xd03_2018$date1 <- as.numeric(284)

xd03_2018$survey_year <- rep(2018)
class(xd03_2018$survey_year)

xd03_2018$oslosp <- as.numeric(NA)
xd03_2018$oslobl <- as.numeric(NA)
xd03_2018$peacesp <- as.numeric(NA)
xd03_2018$peacebl <- as.numeric(NA)
class(xd03_2018$oslosp)

xd03_2018 <- xd03_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

#  D X08_2018 ----

xx08_2018 <- X08_2018 %>%
  filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range= Q2c, age = Q27,
         educ =Q32,
         political_spectrum =Q25 ,
         relig =Q3c ,
         incom =Q31 )

xx08_2018$survey_year <- rep(2018)
class(xx08_2018$survey_year)

xx08_2018$oslosp <- as.numeric(NA)
xx08_2018$oslobl <- as.numeric(NA)
xx08_2018$peacesp <- as.numeric(NA)
xx08_2018$peacebl <- as.numeric(NA)
class(xx08_2018$oslosp)

xx08_2018$date1 <- as.numeric(289)

xx08_2018$date <- "2018-08-30"
class(xx08_2018$date)
xx08_2018$date <-  as.Date(xx08_2018$date, "%Y-%m-%d")


xx08_2018 <- xx08_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


#  D X01_2018 ----
xx01_2018 <- X01_2018 %>% 
  filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range= Q2c,
         educ =Q30,
         political_spectrum =Q23 ,
         relig =Q3c ,
         incom =Q29 )

xx01_2018$age <- as.numeric(NA)

xx01_2018$survey_year <- rep(2018)
class(xx01_2018$survey_year)

xx01_2018$oslosp <- as.numeric(NA)
xx01_2018$oslobl <- as.numeric(NA)
xx01_2018$peacesp <- as.numeric(NA)
xx01_2018$peacebl <- as.numeric(NA)
class(xx01_2018$oslosp)

xx01_2018$date1 <- as.numeric(282)

xx01_2018$date <- "2018-01-30"
class(xx01_2018$date)
xx01_2018$date <-  as.Date(xx01_2018$date, "%Y-%m-%d")


xx01_2018 <- xx01_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

#  D X12_2018 ----
xx12_2018 <- X12_2018 %>% 
  filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range= Q2c,
         educ =Q29,
         political_spectrum =Q22 ,
         relig =Q3c ,
         incom =Q28 )

xx12_2018$age <- as.numeric(NA)

xx12_2018$survey_year <- rep(2018)
class(xx12_2018$survey_year)

xx12_2018$oslosp <- as.numeric(NA)
xx12_2018$oslobl <- as.numeric(NA)
xx12_2018$peacesp <- as.numeric(NA)
xx12_2018$peacebl <- as.numeric(NA)
class(xx12_2018$oslosp)

xx12_2018$date1 <- as.numeric(293)

xx12_2018$date <- "2019-01-06"
class(xx12_2018$date)
xx12_2018$date <-  as.Date(xx12_2018$date, "%Y-%m-%d")


xx12_2018 <- xx12_2018 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)





# collaps ----
xd_17_18 <- rbind(xd03_2018, xd05_2017, xd06_2017, xd07_2017,
                  xd08_2017, xd10_2017,xd12_2017,xd11_2018)

xx_17_18 <- rbind(xx01_2018,xx04_2018,xx05_2018,xx06_2018,xx07_2018,
                  xx08_2018, xx09_2017, xx10_2018, xx11_2017, xx12_2018)
# peace_index_17_18----
peace_index_17_18 <- rbind(xd_17_18,xx01_2018,xx04_2018,xx05_2018,xx06_2018,xx07_2018,
                  xx08_2018, xx09_2017, xx10_2018, xx11_2017, xx12_2018) %>% arrange(date)


peace_index_17_18 <- peace_index_17_18 %>% rename(party=political_spectrum)

peace_index_17_18 <- peace_index_17_18 %>%
  mutate(political_spectrum=case_when(
    party %in% c(2,3,4,5,7,9,10)~1,
    party %in% c(1,6,11,13,25)~2,
    party %in% c(8,12,26)~3,
    party %in% c(14:24,27:30)~4))




write.csv(peace_Q16b,"C:/Users/Dan/Documents/R/heat and peace/data2/peace_Q16b.csv", row.names = FALSE)



peace_index_17_18$political_spectrum <- as.numeric(peace_index_17_18$political_spectrum)


