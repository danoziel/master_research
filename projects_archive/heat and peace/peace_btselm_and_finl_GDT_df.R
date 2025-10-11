# names(at_btselem) ----
"date"                                "total_killed"                       

y <- at_btselem
y [is.na(y)] <- 0 #replace NA to 0

y <- y %>% group_by(yearweek,week_range) %>%
  summarise(total_attacks=sum(total_attacks),
            attacks_located_IL=sum(total_events_located_IL),
            attacks_located_yesha=sum(total_events_located_yesha),
            attacks_target_civilian=sum(total_events_target_civilian),
            attacks_target_security_forces=sum(total_events_target_security_forces),
            sum(total_killed)) %>%filter(yearweek>="2001-W30")

x <- bind_peace10_Q3
xy <- left_join(x,y) %>% select(1,3,4,7:11,2,5,6)
 
xil <- btselem10_il %>% select(1,2)
names(xil)[2] <- "killed_located_IL"

xye <- btselem10_ye %>% select(1,2)
names(xye)[2] <- "killed_located_yesha"

xciv <- btselem10_civ %>% select(1,2) 
names(xciv)[2] <-"killed_target_civilian" 
  
xsec <- btselem10_sec %>% select(1,2)
names(xsec)[2] <- "killed_target_security_forces"
 
x4 <- left_join(xil,xye,by = "yearweek")
x4 <- left_join(x4,xciv)
x4 <- left_join(x4,xsec)
x4 <- x4 %>%  filter(yearweek!="2012-W32",yearweek!="2013-W00",yearweek!="2015-W52")

zzz <- left_join(xy,x4,by="yearweek")

zq <- left_join(zzz,peace_Q1Q2[,1:3],by="yearweek")
zq <- zq[-587,]

zq$q2[zq$q2==0] <- NA
z <- left_join(zq,mete)

names(zqm)
z$total_attacks [is.na(z$total_attacks)] <- 0 #replace NA to 0
z$attacks_located_IL [is.na(z$attacks_located_IL)] <- 0 #replace NA to 0
z$attacks_located_yesha [is.na(z$attacks_located_yesha)] <- 0 #replace NA to 0
z$attacks_target_civilian [is.na(z$attacks_target_civilian)] <- 0 #replace NA to 0
z$attacks_target_security_forces [is.na(z$attacks_target_security_forces)] <- 0 #replace NA to 0

z$total_killed [is.na(z$total_killed)] <- 0 #replace NA to 0
z$killed_located_IL [is.na(z$killed_located_IL)] <- 0 #replace NA to 0
z$killed_located_yesha [is.na(z$killed_located_yesha)] <- 0 #replace NA to 0
z$killed_target_civilian [is.na(z$killed_target_civilian)] <- 0 #replace NA to 0
z$killed_target_security_forces [is.na(z$killed_target_security_forces)] <- 0 #replace NA to 0

z <- z %>%  select(1,2,16,17,3,4:9,12:15,10,18,19,11)

peace_btselm <- z


# meterologic----
ims_data_1 <- ims_data_1_ %>% filter(!is.na(humidity))
ims_data_3 <- ims_data_3_ %>% filter(!is.na(humidity))

class(ims_data_1$date)

ims_data_3$date <- as.IDate(ims_data_3$date)
ims_data_1$temperature <- as.numeric(ims_data_1_$temperature)
ims_data_1$humidity <- as.numeric(ims_data_1_$humidity)

mete <- meter %>% filter(temperature != "-",humidity!= "-"   )
mete$yearweek <-  strftime(mete$date, format = "%Y-W%U") 



meter <- rbind(ims_data_1,ims_data_3)
mete <- mete %>% group_by(yearweek) %>% summarise(mean(temperature),mean(humidity))


ims_data_1 <- ims_data_1[1:28274,]


# ------

library(foreign)
library(haven)		

write.dta(peace_and_attackes, "C:/Users/Dan/Documents/R/heat and peace/peace_and_attackes.dta")

write.table(peace_and_attackes, file="peace_and_attackes.csv",sep=",",row.names=F)

save(peace_btselm, file ='peace_btselm.RData')

# ----

https://www.start.umd.edu/gtd/

library(data.table)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")

# ------------------  days data.table
library(data.table)
date <- seq(as.IDate("1994-01-01"), as.IDate("2018-12-28"), 1)
day_df <- data.table(date=date)  # 9128 days


library(readxl)
peace_and_terror <- read_excel("C:/Users/Dan/Downloads/peace_and_terror.xlsx")

names(peace_and_terror)

# xpeace ----
xpeace= peace_and_terror[,c(27,1:16)] %>% distinct()

# xterror ----
date_terror= peace_and_terror [,1] %>% 
  distinct() %>% rename(date_POSIX=date) %>% 
  bind_cols(day_df) %>%  rename(date_IDate=date)
  
## stage x ----
xterror= peace_and_terror %>% 
  select("date","country_txt","targtype1_txt_new","incidents","killd",
          "total_incidents_IL","total_incidents_yesha",
          "targt_civilians","targt_security_force",
          "security_situation_txt", "security_situation_num" )%>% 
  rename(date_POSIX=date) %>% 
  left_join(date_terror) %>% 
  select(date_POSIX,date_IDate,everything())

# xincidents= xterror TAKE 1 ----
xincidents= xterror  %>% # digits represents AMOUNT
  select(date_POSIX,date_IDate,country_txt,"targtype1_txt_new","incidents" ) %>% 
  
  mutate(incidents_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                                   country_txt == "West Bank and Gaza Strip",incidents,NA)) %>% 
  mutate(incidents_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                    country_txt == "West Bank and Gaza Strip",incidents,NA)) %>% 
  mutate(incidents_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                                country_txt == "Israel",incidents,NA)) %>% 
  mutate(incidents_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                                 country_txt == "Israel",incidents,NA)) %>% 
  select(date_POSIX,date_IDate,
         incidents_security_yesha_total,incidents_civilians_yesha_total,
         incidents_security_il_total,incidents_civilians_il_total ) %>% 
  distinct()

# xincidents= xterror TAKE 2 ----
filter(date_POSIX %in% c(12606,12654, 12448,  15372, 19918 )) %>% 
  
xz_incidents= 
  xterror  %>% # digits represents AMOUNT
  select(date_POSIX,date_IDate,country_txt,"targtype1_txt_new","incidents" ) %>% 
  filter(!is.na(incidents)) %>% distinct()  %>% 
  
  mutate(incidents_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                                   country_txt == "West Bank and Gaza Strip",incidents,NA)) %>% 
  mutate(incidents_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                    country_txt == "West Bank and Gaza Strip",incidents,NA)) %>% 
  mutate(incidents_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                                country_txt == "Israel",incidents,NA)) %>% 
  mutate(incidents_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                                 country_txt == "Israel",incidents,NA)) %>%
  group_by(date_POSIX) %>% mutate(total_incidents_day=sum(incidents)) %>% # incidents_total_day VAR
  select(-c(incidents,country_txt,targtype1_txt_new))

xz2_incidents=
  xz_incidents %>%  
  group_by(date_IDate) %>% fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% select(-for_delete_same_date_rows)

xz2_incidents$incidents_security_yesha_total [is.na(xz2_incidents$incidents_security_yesha_total)] <- "" 
xz2_incidents$incidents_civilians_yesha_total [is.na(xz2_incidents$incidents_civilians_yesha_total)] <- "" 
xz2_incidents$incidents_security_il_total [is.na(xz2_incidents$incidents_security_il_total)] <- "" 
xz2_incidents$incidents_civilians_il_total [is.na(xz2_incidents$incidents_civilians_il_total)] <- "" 


# xkilld=xterror TAKE 1 ----
xkilld=xterror %>% # digits represents AMOUNT
  mutate(killd_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                  country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                         country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                         country_txt == "Israel",killd,NA)) %>% 
  mutate(killd_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                          country_txt == "Israel",killd,NA)) %>% 
  select(date_POSIX,date_IDate,
         killd_security_yesha_total,killd_civilians_yesha_total,
         killd_security_il_total,killd_civilians_il_total ) %>% 
  distinct()


# xkilld=xterror TAKE 2 ----

filter(date_POSIX %in% c(12606,12654,12882, 12448,  15372, 19918 )) %>% 
  
xz_killd=xterror %>% # digits represents AMOUNT
  select(date_POSIX,date_IDate,"country_txt","targtype1_txt_new","killd" ) %>% 
  filter(!is.na(killd)) %>% distinct()  %>% 
  
  mutate(killd_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                               country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                            country_txt == "Israel",killd,NA)) %>% 
  mutate(killd_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                             country_txt == "Israel",killd,NA)) %>% 
  
  group_by(date_POSIX) %>% mutate(total_killd_day=sum(killd)) %>% # total_killd_day VAR
  select(-c(killd,country_txt,targtype1_txt_new))

xz2_killd = 
  xz_killd %>%
  group_by(date_IDate) %>% fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% 
  select(-for_delete_same_date_rows)

xz2_killd$killd_security_yesha_total [is.na(xz2_killd$killd_security_yesha_total)] <- "" 
xz2_killd$killd_civilians_yesha_total [is.na(xz2_killd$killd_civilians_yesha_total)] <- "" 
xz2_killd$killd_security_il_total [is.na(xz2_killd$killd_security_il_total)] <- "" 
xz2_killd$killd_civilians_il_total [is.na(xz2_killd$killd_civilians_il_total)] <- "" 



# xtargt=xterror TAKE 1----
xtargt=xterror %>% # digits represents YES=1
  mutate(targt_security_yesha_1yes = ifelse(targt_security_force == 1 & 
                                               country_txt == "West Bank and Gaza Strip",targt_security_force,NA)) %>% 
  mutate(targt_civilians_yesha_1yes = ifelse(targt_civilians == 1 & 
                                                country_txt == "West Bank and Gaza Strip",targt_civilians ,NA)) %>% 
  mutate(targt_security_il_1yes = ifelse(targt_security_force == 1 & 
                                            country_txt == "Israel",targt_security_force,NA)) %>% 
  mutate(targt_civilians_il_1yes = ifelse(targt_civilians == 1 & 
                                             country_txt == "Israel",targt_civilians,NA)) %>%
  select(date_POSIX,date_IDate,
         targt_security_yesha_1yes,targt_civilians_yesha_1yes,
         targt_security_il_1yes,targt_civilians_il_1yes)%>% 
  distinct()
#xtargt=xterror TAKE 2 ----
xz_targt=
  xterror %>% # digits represents YES=1
  select(date_POSIX,date_IDate,"country_txt","targt_security_force","targt_civilians" ) %>% 
  filter(!is.na(targt_civilians)) %>% distinct()  %>% 

  mutate(targt_security_yesha_1yes = ifelse(targt_security_force == 1 & 
                                              country_txt == "West Bank and Gaza Strip",targt_security_force,NA)) %>% 
  mutate(targt_civilians_yesha_1yes = ifelse(targt_civilians == 1 & 
                                               country_txt == "West Bank and Gaza Strip",targt_civilians ,NA)) %>% 
  mutate(targt_security_il_1yes = ifelse(targt_security_force == 1 & 
                                           country_txt == "Israel",targt_security_force,NA)) %>% 
  mutate(targt_civilians_il_1yes = ifelse(targt_civilians == 1 & 
                                            country_txt == "Israel",targt_civilians,NA)) %>%
  select(-c(country_txt,targt_security_force,targt_civilians))

xz2_targt=
  xz_targt %>% 
  group_by(date_IDate) %>% fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% select(-for_delete_same_date_rows)

xz2_targt$targt_security_yesha_1yes [is.na(xz2_targt$targt_security_yesha_1yes)] <- "" 
xz2_targt$targt_security_il_1yes [is.na(xz2_targt$targt_security_il_1yes)] <- "" 
xz2_targt$targt_civilians_yesha_1yes [is.na(xz2_targt$targt_civilians_yesha_1yes)] <- "" 
xz2_targt$targt_civilians_il_1yes [is.na(xz2_targt$targt_civilians_il_1yes)] <- "" 


#xsituation TAKE2----

xsituation= 
  security_situation %>% 
  rename(security_situation_1yes=security_situation_num,
         date_IDate=date) 

xz2_situation=xsituation

names(xz2_situation)

xz2_situation$security_situation_txt [is.na(xz2_situation$security_situation_txt)] <- "" 
xz2_situation$security_situation_1yes[is.na(xz2_situation$security_situation_1yes)] <- ""

# full_join TAKE2 ----
xz2_terror_join=
  full_join(xz2_killd,xz2_incidents) %>% 
  full_join(xz2_targt) %>% 
  right_join(xz2_situation) # add also the calendar


terror_index_16102023 = xz2_terror_join
terror_index_16102023$total_killd_day[is.na(terror_index_16102023$total_killd_day)]=""
terror_index_16102023$total_incidents_day[is.na(terror_index_16102023$total_incidents_day)]=""

terror_index_16102023 <- 
  terror_index_16102023 %>% 
  select(-date_POSIX) %>% 
  arrange(date_IDate)



write.csv(terror_index_16102023, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_df_16102023.csv", row.names=FALSE)


library(haven)
write_dta(terror_index_16102023, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_df_16102023.dta")



## stage y ----
# test
# xincidents [3216:3226,]
# xkilld   [3216:3226,]
#  xtargt [3216:3226,]
# xterror [56126:56136,1:6]
  
# y_terror=xterror %>%
#   group_by(date_POSIX,date_IDate) %>%
#   summarise(incidents_total_day=sum(incidents),killd_total_day=sum(killd))

y_incidents=xincidents %>% 
  group_by(date_IDate) %>% 
  fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% 
  select(-for_delete_same_date_rows)

y_killd = xkilld %>%
  group_by(date_IDate) %>% 
  fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% 
  select(-for_delete_same_date_rows)

y_targt=xtargt %>% 
  group_by(date_IDate) %>% fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date_IDate == lead(date_IDate), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% select(-for_delete_same_date_rows)

xsituation= 
  security_situation %>% 
  rename(security_situation_1yes=security_situation_num,
         date_IDate=date) 
  

# full_join
y_terror_join=
  full_join(y_killd,y_incidents) %>% 
  full_join(y_targt) %>% 
  full_join(xsituation) %>% 
  rename(total_killd_day = "sum(killd)" )

terror_index_16102023 = y_terror_join

>write.csv(terror_index_16102023, file ="C:/Users/Dan/Documents/terror_index_16102023.csv", row.names=FALSE)


# Load the haven package
library(haven)

# Save the data frame as a Stata file
write_dta(terror_index_16102023, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_index_16102023.dta")

# full_join ## 2 ----

z_killd <- 
  y_killd [3:6] %>% 
  mutate(killd_total_day = rowSums(.[names(.)[1:4]], na.rm = T))

selected_columns <- y_killd[, c("killd_security_yesha_total",
                             "killd_civilians_yesha_total",
                             "killd_security_il_total",
                             "killd_civilians_il_total" )]



y_killd$sum_of_selected_columns <- row_sums


z_incidents <- y_incidents

         

  mutate(killd_total_day=
           killd_security_yesha_total+
           killd_civilians_yesha_total+
           killd_security_il_total+
           killd_civilians_il_total)      

[7] "incidents_security_yesha_total"  "incidents_civilians_yesha_total"
[9] "incidents_security_il_total"     "incidents_civilians_il_total"
  full_join(y_targt) %>% 
  full_join(xsituation)


terror_df_24082023 = y_terror_join

# join  peace + terror ----

# tests
a1994=xpeace %>% filter(survey_year == 1994)

atest_dates_rang_peace= xpeace %>% select(date) %>%  distinct() # 9128 days

table(xpeace$survey_year)
class(xpeace$date)
class(xpeace$date_POSIX)


# dataset
xpeace= peace_and_terror[,c(27,1:16)] %>%
  distinct() %>% 
  rename(date_POSIX=date) %>% 
  left_join(date_terror) %>% 
  select(-date_cha) %>% 
  select(date_POSIX,date_IDate,everything())

peace_index_df_24082023 = xpeace

write.csv(peace_index_df_24082023, file ="C:/Users/Dan/Documents/peace_index_df_24082023.csv", row.names=FALSE)

write.csv(terror_df_24082023, file ="C:/Users/Dan/Documents/terror_df_24082023.csv", row.names=FALSE)


#====
#X3terror=X2terror %>% mutate(total_incidents = ifelse(date == lag(date), incidents + lag(incidents), incidents))


peaceIndex_terror_24082023



  

  
  
  


