library(dplyr)
library(readxl)
# From and including: Saturday, January 1, 1994
# To and including: Thursday, June 17, 2021
# Result: 10,030 days

# GTD_dist     ====

GTD_0522dist<-read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/globalterrorismdb_0522dist.xlsx")
GTD_2021Jan_June_1222dist <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/globalterrorismdb_2021Jan-June_1222dist.xlsx")

GTD_dist_12_2023 <- rbind(GTD_0522dist,GTD_2021Jan_June_1222dist)
rm(GTD_0522dist,GTD_2021Jan_June_1222dist)

# agragete DF ====

GTD_dist <- GTD_dist_12_2023
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


# date country_txt ----
tr_01 <- GTD_dist %>% 
  filter(iyear>=1994,country_txt=="Israel"| country_txt=="West Bank and Gaza Strip",
         natlty1_txt== "Israel") # Nationality of Target/Victim

tr_01$iday[tr_01$approxdate == "March 1-2, 2002"] <- 3 # 200203030003   
tr_01$iday[tr_01$approxdate == "01/17/2006"] <- 17 # 200601000009   
tr_01$date <- as.Date(with(tr_01, paste(iyear, imonth, iday, sep = "-")))



tr_02 <- tr_01 %>% 
  select(eventid, date, country_txt,
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

tr_03=tr_02

## tr_03 ## targtype1_txt_new "security_force" "civilians"  (targtype1_txt) ----

nkill,   # generate 4 cols: security_yesha | civilians_yesha | security_il | civilians_il
nwound , # generate 4 cols: security_yesha | civilians_yesha | security_il | civilians_il


tr_03 <- tr_02 %>% 
  mutate(targtype1_txt_new=
           ifelse(targtype1_txt %in% c("Military","Police"),"security force","civilians")) %>% 

xterror=tr_03  %>% select(date,country_txt, targtype1_txt_new)


# xz_targt ----
xz_targt=xterror %>% # digits represents YES=1
  mutate(targt_security_yesha_1yes = ifelse(targtype1_txt_new == "security force" & 
                                              country_txt == "West Bank and Gaza Strip",1,NA) )%>% 
  mutate(targt_civilians_yesha_1yes = ifelse(targtype1_txt_new == "civilians" & 
                                               country_txt == "West Bank and Gaza Strip",1 ,NA)) %>% 
  mutate(targt_security_il_1yes = ifelse(targtype1_txt_new == "security force" & country_txt == "Israel",1,NA)) %>% 
  mutate(targt_civilians_il_1yes= ifelse(targtype1_txt_new == "civilians" & country_txt == "Israel",1,NA)) %>%
  
  select(eventid, date,
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









# write.csv write_dta read_dta ----
# library(haven)
# peace_index_17_18 <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/peace_index_17_18.dta")
# library(foreign)
# write_dta(peace_index, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/peace_index.dta")
# write.csv(peace_and_terror, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/peace_and_terror.csv", row.names=FALSE)