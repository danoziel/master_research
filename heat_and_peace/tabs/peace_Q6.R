write.csv(xx17_5_11,"C:/Users/Dan/Documents/R/heat and peace/xx17_5_11.csv", row.names = FALSE)

# variables select                    ----
peace_Q6 <- peace_index %>%
  select(date,date1,survey_year,
         oslosp,oslobl,negot_sp,negot_bl,peacesp,peacebl,
         gender,age,educ,relig,inc,party,pvote99,pvote03,pvote06,pvote09)

# creating "political_spectrum" column----
peace_Q6 <- peace_Q6 %>%
  rename(incom=inc) %>% 
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
# arrang dataset                      ----
peace_Q6 <- peace_Q6 %>%
  select(date, date1, survey_year,
         oslosp, oslobl, negot_sp, negot_bl, peacesp,peacebl,
         gender, age, educ, relig, incom, political_spectrum )

# replac date - NA to "2005-11-29"
x142 <- peace_Q6 %>% filter(date1==142)
x142$date <- "2005-11-29"
x142$date <-  as.Date(x142$date, "%Y-%m-%d")

peace_Q6 <- peace_Q6 %>% filter(!is.na(date)) %>% rbind(x142)%>% arrange(date)

# add: "xx29_11_12" & "xx17_5_11" to "peace_Q6"
peace_Q6 <- rbind (peace_Q6,xx29_11_12,xx17_5_11) %>% arrange(date)

# creating age_range                  ----

#creating age_range in peace_Q6 
peace_Q6 <- peace_Q6 %>%
  mutate(age_range=case_when(
    age >= 18 & age <= 24 | age == 1 ~1,
    age >= 25 & age <= 34 | age == 2 ~2,
    age >= 35 & age <= 44 | age == 3 ~3,
    age >= 45 & age <= 54 | age == 4 ~4,
    age >= 55 & age <= 64 | age == 5 ~5,
    age >= 65 | age == 6 ~6))

#creating age_range in peace_index_17_18
peace_index_17_18$age_range[peace_index_17_18$age_range == 2] <- 1
peace_index_17_18$age_range[peace_index_17_18$age_range == 3] <- 2
peace_index_17_18$age_range[peace_index_17_18$age_range == 4] <- 3
peace_index_17_18$age_range[peace_index_17_18$age_range == 5] <- 4
peace_index_17_18$age_range[peace_index_17_18$age_range == 6] <- 5
peace_index_17_18$age_range[peace_index_17_18$age_range == 7] <- 6

# -------  bind peace_Q6 & peace_index_17_18 ------ to -------  peace_and_war ----
 
x_Q6 <- select(peace_Q6,date, date1, survey_year,
       oslosp, oslobl, negot_sp, negot_bl, peacesp,peacebl,
       gender, age_range, educ, relig, incom, political_spectrum )

x_17_18 <- select(peace_index_17_18,date, date1, survey_year,
       oslosp, oslobl, negot_sp, negot_bl, peacesp,peacebl,
       gender, age_range, educ, relig, incom, political_spectrum )

peace_and_war <- rbind(x_Q6,x_17_18)

# replace NA to 0                     ----
peace_and_war$oslosp [is.na(peace_and_war$oslosp)] <- 0 
peace_and_war$oslobl [is.na(peace_and_war$oslobl)] <- 0

peace_and_war$negot_sp [is.na(peace_and_war$negot_sp)] <- 0
peace_and_war$negot_bl [is.na(peace_and_war$negot_bl)] <- 0

peace_and_war$peacesp [is.na(peace_and_war$peacesp)] <- 0
peace_and_war$peacebl [is.na(peace_and_war$peacebl)] <- 0





# variables normaization              ----

# incom
peace_and_war$incom[peace_and_war$incom %in% c(8,9,999)] <- 7

#relig
peace_and_war <- peace_and_war %>% mutate(relig_scale=case_when(
  
        date1 >= 39 & date1 <= 143 & relig %in% c(5,6,7,8)~5,
        
        date1 >= 144 & date1 <= 273 & relig %in% c(3,4)~ 3,
        date1 >= 144 & date1 <= 273 & relig == 5~4,
        date1 >= 144 & date1 <= 273 & relig %in% c(6,7,8) ~5,
        
        date1 >= 274 & date1 <= 293 & relig == 1~4,
        date1 >= 274 & date1 <= 293 & relig == 2~3,
        date1 >= 274 & date1 <= 293 & relig == 3~2,
        date1 >= 274 & date1 <= 293 & relig == 4~1,
        
        TRUE ~ relig)) %>% select(-relig)

#educ

peace_and_war <- peace_and_war %>% mutate(education=case_when(
  date1 >= 1 & date1 <= 293 & educ %in% c(5,6)~ 5,
  date1 >= 1 & date1 <= 293 & educ %in% c(7,8,9)~ 6,
  TRUE ~ educ)) %>% select(- educ)



#----------------peace_and_terror-----------------------
#                peace_and_terror
#--------------------------------

# This code also appears in GDT tab
peace_and_war$date <- as.IDate(peace_and_war$date)
class(peace_and_war$date)

dt_terror <- terrorismdb
dt_peace <- peace_and_war

# add ID
dt_peace$id <- seq(1,146272)
dt_peace <- select(dt_peace,1:3,id,everything())



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
  select(1,4:26,2,3)






# thing to fix ----

peace_and_terror$education[peace_and_terror$education == 22] <- NA 
peace_and_terror$education[peace_and_terror$education == 23] <- NA 
peace_and_terror$education[peace_and_terror$education == 24] <- NA 
peace_and_terror$education[peace_and_terror$education == 25 ]<- NA 
peace_and_terror$education[peace_and_terror$education == 999] <- NA 
peace_and_terror$education[peace_and_terror$education == 0] <- NA 


table(peace_and_terror$education)

library(foreign)
write.dta(peace_and_terror, "c:/Users/Dan/Documents/R/heat and peace/peace_and_terror.dta")

library(haven)
write_sav(peace_and_terror, "peace_and_terror.sav")

library(writexl)
write_xlsx(peace_and_terror, "c:/Users/Dan/Documents/R/heat and peace/peace_and_terror.xlsx")
