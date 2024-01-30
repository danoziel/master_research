library(dplyr)
library(tidyr)
library(readxl)
#   From and including: Saturday, January 1, 1994
#     To and including: Thursday, June 25, 2021
#  10,038 days
#  2,700  incidents
#  1,731  day with incidents


# GTD_dist     ====

GTD_0522dist<-read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/globalterrorismdb_0522dist.xlsx")
GTD_2021Jan_June_1222dist <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/globalterrorismdb_2021Jan-June_1222dist.xlsx")

GTD_dist_12_2023 <- rbind(GTD_0522dist,GTD_2021Jan_June_1222dist)
rm(GTD_0522dist,GTD_2021Jan_June_1222dist)

# tr_01 <- GTD_dist   ====

GTD_dist <- GTD_dist_12_2023
tr_01 <- GTD_dist %>% 
  filter(iyear>=1994,country_txt=="Israel"| country_txt=="West Bank and Gaza Strip",
         natlty1_txt== "Israel") %>% # Nationality of Target/Victim
  mutate(targtype1_txt_new=
           ifelse(targtype1_txt %in% c("Military","Police"),"security force","civilians"))
    

tr_01$iday[tr_01$approxdate == "March 1-2, 2002"] <- 3 # 200203030003   
tr_01$iday[tr_01$approxdate == "01/17/2006"] <- 17 # 200601000009   
tr_01$date <- as.Date(with(tr_01, paste(iyear, imonth, iday, sep = "-")))


# data ----
# removed
#    natlty2_txt,natlty3_txt,
#    target1, target2, target3, target4,
#    targtype2_txt,targtype3_txt,targtype4_txt,weapdetail,
#    weapsubtype2,weapsubtype3,weapsubtype4,
#    city ,location,
# guncertain1,guncertain2, guncertain3 NO NEED

# NAs only
#    gsubname2, gsubname3 # gname2,gname3, 

#  116 cases are not similar between targtype1_txt and targtype2_txt
tr_01[,c(1,grep("^weap",names(tr_01)))] %>% 
  mutate(a=ifelse(weaptype1_txt==weaptype2_txt,1,0 )) %>% 
  select(weaptype1_txt,weaptype2_txt,a) %>% count(a)



# tr_02 <- tr_01 ----

tr_02 <- tr_01 %>% 
  select(eventid, date, country_txt,targtype1_txt_new,
         natlty1_txt, # Nationality of Target/Victim
         weapsubtype1,weaptype1_txt,weapsubtype1_txt,weapdetail,
         targtype1_txt,
         nkill,nwound,
         # gsubname,gname, 
         provstate,
         attacktype1_txt,suicide,
         multiple ,related ,
         summary, scite1, scite2, scite3, addnotes) %>% 
  mutate(weapon="other")

# district provstate ----

tr_02$district= tr_02$provstate

tr_02$district[tr_02$provstate %in% c( "Golan Heights", "Northern" )  ] <- "Northern"
tr_02$district[tr_02$provstate %in% c( "North Sinai", "Southern" )  ] <- "Southern"


# weapon = weapsubtype1,weaptype1_txt,weapsubtype1_txt,weapdetail ----
tr_02$weapon[tr_02$weaptype1_txt=="Melee"] <- "melee"

tr_02$weapon <-ifelse(
  grepl("knife|screwdriver|Knives|cleaver|Stabbed", tr_02$weapdetail, ignore.case = TRUE) & 
    tr_02$weaptype1_txt == "Melee","sharp",tr_02$weapon)

tr_02$weapon <- ifelse(
  grepl("Stone|Bricks|Rocks|rock", tr_02$weapdetail, ignore.case = TRUE) &
    tr_02$weaptype1_txt == "Melee","stone",tr_02$weapon)

tr_02$weapon[tr_02$weaptype1_txt=="Firearms"] <- "firearms"

tr_02$weapon[tr_02$weaptype1_txt=="Explosives"] <- "explosives"
tr_02$weapon [tr_02$weapsubtype1 ==11] <- "projectile"
tr_02$weapon [tr_02$weapsubtype1 ==13] <- "suicide"

# tr_02$weapon[tr_02$weaptype1_txt == "Incendiary" ] <- "incendiary" ; only 2 obs so better "other"
tr_02$weapon [tr_02$weapsubtype1 %in% c(18,20)] <- "arson"   #[18 Arson/Fire] [20 Gasoline or Alcohol]
tr_02$weapon [tr_02$weapsubtype1 ==19] <- "molotov_cocktail" #[19 Molotov Cocktail/Petrol Bomb]


# district & weapon ----

# targt_new <- tr_01 %>% select("eventid","date", "targtype1_txt_new")
# DW <- tr_02 %>% full_join(targt_new)
# DW$date_cha <- as.character(DW$date)

tr_02$date_cha <- as.character(tr_02$date)

weapon_n_district <- 
  # DW %>% 
#  tr_02 %>% 
  select("eventid","date","date_cha","country_txt","targtype1_txt_new","natlty1_txt",
         "weapon","district","nkill","nwound",
         "related","summary","scite1","scite2","scite3","addnotes")

sapply(weapon_n_district, class)
weapon_n_district$nkill <- as.character(weapon_n_district$nkill)
weapon_n_district$nwound <- as.character(weapon_n_district$nwound)
weapon_n_district[is.na(weapon_n_district)] <- ""

terror_04122023_weapon_n_district
library(haven)
library(foreign)
write_dta(weapon_n_district, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_04122023_weapon_n_district.dta")
write.csv(weapon_n_district, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_04122023_weapon_n_district.csv", row.names=FALSE)

######## tr_03 ## targtype1_txt_new "security_force" "civilians"  (targtype1_txt) ----

#  xz2_incidents xz_incidents ----
xz_incidents= tr_03 %>% select(date,country_txt, targtype1_txt_new)  %>% 
  mutate(incidents_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                                   country_txt == "West Bank and Gaza Strip",1,NA)) %>% 
  mutate(incidents_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                    country_txt == "West Bank and Gaza Strip",1,NA)) %>% 
  mutate(incidents_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                                country_txt == "Israel",1,NA)) %>% 
  mutate(incidents_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                                 country_txt == "Israel",1,NA)) %>%
  group_by(date) %>% mutate(incidents_security_yesha_total=sum(incidents_security_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(incidents_civilians_yesha_total=sum(incidents_civilians_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(incidents_security_il_total=sum(incidents_security_il_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(incidents_civilians_il_total=sum(incidents_civilians_il_total,na.rm = T)) %>% 
  select(date,
         incidents_security_yesha_total,incidents_civilians_yesha_total,
         incidents_security_il_total,incidents_civilians_il_total)

xz2_incidents <-  xz_incidents %>% distinct()
xz2_incidents$incidents_day <- rowSums(xz2_incidents[, c("incidents_security_yesha_total","incidents_civilians_yesha_total","incidents_security_il_total","incidents_civilians_il_total")])

xz2_incidents[] <- lapply(xz2_incidents, function(col) replace(col, col == 0, ""))



#  xz2_killd     xz_killd     ----

xz_killd = 
  tr_03 %>% select(date,country_txt, targtype1_txt_new,nkill) %>% # digits represents AMOUNT
  rename(killd=nkill) %>%  # digits represents AMOUNT
  mutate(killd_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                               country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                country_txt == "West Bank and Gaza Strip",killd,NA)) %>% 
  mutate(killd_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                            country_txt == "Israel",killd,NA)) %>% 
  mutate(killd_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                             country_txt == "Israel",killd,NA)) %>% 
  
  group_by(date) %>% mutate(killd_security_yesha_total=sum(killd_security_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(killd_civilians_yesha_total=sum(killd_civilians_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(killd_security_il_total=sum(killd_security_il_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(killd_civilians_il_total=sum(killd_civilians_il_total,na.rm = T)) %>% 
  select(date,
            killd_security_yesha_total,killd_civilians_yesha_total, 
         killd_security_il_total,killd_civilians_il_total)

xz2_killd <- xz_killd %>% distinct()
xz2_killd$killd_a_day <- rowSums(xz2_killd[, c("killd_security_yesha_total","killd_civilians_yesha_total","killd_security_il_total","killd_civilians_il_total")])
xz2_killd[] <- lapply(xz2_killd, function(col) replace(col, col == 0, ""))

#  xz2_injured   xz_injured   ----

xz_injured = 
  tr_03 %>% select(date,country_txt, targtype1_txt_new,nwound) %>% # digits represents AMOUNT
  rename(injured=nwound) %>%  # digits represents AMOUNT
  mutate(injured_security_yesha_total = ifelse(targtype1_txt_new =="security force" & 
                                               country_txt == "West Bank and Gaza Strip",injured,NA)) %>% 
  mutate(injured_civilians_yesha_total = ifelse(targtype1_txt_new =="civilians" & 
                                                country_txt == "West Bank and Gaza Strip",injured,NA)) %>% 
  mutate(injured_security_il_total = ifelse(targtype1_txt_new =="security force" & 
                                            country_txt == "Israel",injured,NA)) %>% 
  mutate(injured_civilians_il_total = ifelse(targtype1_txt_new =="civilians" & 
                                             country_txt == "Israel",injured,NA)) %>% 
  
  group_by(date) %>% mutate(injured_security_yesha_total=sum(injured_security_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(injured_civilians_yesha_total=sum(injured_civilians_yesha_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(injured_security_il_total=sum(injured_security_il_total,na.rm = T)) %>% 
  group_by(date) %>% mutate(injured_civilians_il_total=sum(injured_civilians_il_total,na.rm = T)) %>% 
  select(date,
         injured_security_yesha_total,injured_civilians_yesha_total, 
         injured_security_il_total,injured_civilians_il_total)

xz2_injured <- xz_injured %>% distinct()
xz2_injured$injured_day <- rowSums(xz2_injured[, c("injured_security_yesha_total","injured_civilians_yesha_total", "injured_security_il_total","injured_civilians_il_total")])
xz2_injured[] <- lapply(xz2_injured, function(col) replace(col, col == 0, ""))




#  xz2_targt     xz_targt     ----
xz_targt=xterror %>% # digits represents YES=1
  mutate(targt_security_yesha_1yes = ifelse(targtype1_txt_new == "security force" & 
                                              country_txt == "West Bank and Gaza Strip",1,NA) )%>% 
  mutate(targt_civilians_yesha_1yes = ifelse(targtype1_txt_new == "civilians" & 
                                               country_txt == "West Bank and Gaza Strip",1 ,NA)) %>% 
  mutate(targt_security_il_1yes = ifelse(targtype1_txt_new == "security force" & country_txt == "Israel",1,NA)) %>% 
  mutate(targt_civilians_il_1yes= ifelse(targtype1_txt_new == "civilians" & country_txt == "Israel",1,NA)) %>%
  select(date,
         targt_security_yesha_1yes,targt_civilians_yesha_1yes,
         targt_security_il_1yes,targt_civilians_il_1yes)

library("tidyr")
xz2_targt=
  xz_targt %>% 
  group_by(date) %>% fill(names(.),.direction = "down") %>%  
  mutate(for_delete_same_date_rows = ifelse(date == lead(date), "TOP", NA)) %>% 
  filter(is.na( for_delete_same_date_rows)) %>% select(-for_delete_same_date_rows)

xz2_targt$targt_security_yesha_1yes [is.na(xz2_targt$targt_security_yesha_1yes)] <- "" 
xz2_targt$targt_security_il_1yes [is.na(xz2_targt$targt_security_il_1yes)] <- "" 
xz2_targt$targt_civilians_yesha_1yes [is.na(xz2_targt$targt_civilians_yesha_1yes)] <- "" 
xz2_targt$targt_civilians_il_1yes [is.na(xz2_targt$targt_civilians_il_1yes)] <- "" 

# security_situation ----

# ------------------  days data.table
library(data.table)
date <- seq(as.Date("1994-01-01"), as.Date("2021-06-25"), 1)
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

x7 <-  day_df %>% filter(date>="2019-05-03",date<"2019-05-07") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x7$n[x7$n== 1] <- "gan_sagur"

x8 <-  day_df %>% filter(date>="2019-11-12",date<"2019-11-15") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x8$n[x8$n== 1] <- "chagora_shchora"

x9 <-  day_df %>% filter(date>="2021-05-10",date<"2021-05-22") %>%
  group_by(date) %>% count()%>% mutate( security_situation = n )
x9$n[x9$n== 1] <- "shomer_hachomot"


security_situation <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
security_situation <- day_df %>% left_join(security_situation) %>% 
  rename(security_situation_num=security_situation ) %>% 
  rename(security_situation_txt=n)

security_situation$security_situation_num [is.na(security_situation$security_situation_num)] <- "" 
security_situation$security_situation_txt [is.na(security_situation$security_situation_txt)] <- "" 


# full_join terror_df_04122023=xz2_terror_join5vars ----
xz2_terror_join5vars= 
  xz2_killd %>% 
  full_join(xz2_injured) %>% 
  full_join(xz2_incidents) %>% 
  full_join(xz2_targt) %>% 
  full_join(security_situation) %>% # this one has the calendar
  arrange(date)
xz2_terror_join5vars[is.na(xz2_terror_join5vars)] <- ""
xz2_terror_join5vars$date_cha <- as.character(xz2_terror_join5vars$date)


names(xz2_terror_join5vars)

terror_04122023=xz2_terror_join5vars %>% select(date,date_cha,everything())

# write.csv write_dta read_dta ----

library(haven)
library(foreign)
write_dta(terror_04122023, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_04122023.dta")
write.csv(terror_04122023, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_04122023.csv", row.names=FALSE)


# library(haven)
terror <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_04122023.dta")

