library(dplyr)
library(tidyr)
library(readxl)
#   From and including: Saturday, January 1, 1994
#     To and including: Thursday, June 25, 2021
#  10,038 days
#  2,700  incidents
#  1,731  day with incidents
         

#### tr_01 IN "trror_12_23.R"
#### ### ###

# TR0 <- tr_01  ----

TR0 <- tr_01 %>% 
  select(eventid, date, country_txt,targtype1_txt_new,
         natlty1_txt, # Nationality of Target/Victim
         weapsubtype1,weaptype1_txt,weapsubtype1_txt,weapdetail,
         targtype1_txt,
         nkill,nwound,
         gsubname,
         gname, 
         provstate,
         attacktype1_txt,suicide,
         multiple ,related ,
         summary, scite1, scite2, scite3, addnotes) 

# TR1 <- TR0  ----
# gname               ----

TR0 # A tibble: 2,700 × 24
TR1 # A tibble: 2,668 × 25

gname <- TR0 %>% select( gsubname, gname)
gname %>% count(gname) %>% print(n = Inf)

TR1 <- TR0 %>%
  mutate(gname2=ifelse(gname %in% c(
    "Israeli extremists",
    "Israeli settlers",
    "Jaish Al-Umma (Army of the Nation)",
    "Jerusalem Groups Hebrew (Qvutzot Yerushalayim)",
    "Jewish Extremists",
    "Jewish Fighting Organization (Eyal)",
    "Kach",
    "Likud Political Party"
    ),"isrli",
                  ifelse(gname=="Unknown","Unknown",
                         "arb" )))%>% 
  # filter(gname2
  filter(gname2 != "isrli" )

TR1 %>% count(gname) %>% print(n = Inf)
TR1 %>% count(gname2) %>% print(n = Inf)

TR1 <- TR1 %>% filter(gname2 != "isrli" )

# district provstate                                              ----
TR1 %>% count( provstate)

TR1$district= TR1$provstate
TR1$district[TR1$provstate %in% c("Golan Heights","Northern","North Sinai","Southern")] <- "district_north_south"
TR1$district[TR1$provstate %in% c("Central","Haifa","Jerusalem","Tel Aviv","Unknown")] <- "district_else"
TR1$district[TR1$provstate %in% c("Gaza Strip", "West Bank")] <- "district_yesha"

TR1 %>% count( district)


# TR2<-TR1  ----
# weapon = weapsubtype1,weaptype1_txt,weapsubtype1_txt,weapdetail ----

TR2 <- TR1 %>% mutate(weapon="other")

TR2$weapon[TR2$weaptype1_txt=="Melee"] <- "melee"

TR2$weapon <-ifelse(
  grepl("knife|screwdriver|Knives|cleaver|Stabbed", TR2$weapdetail, ignore.case = TRUE) & 
    TR2$weaptype1_txt == "Melee","sharp",TR2$weapon)

TR2$weapon <- ifelse(
  grepl("Stone|Bricks|Rocks|rock", TR2$weapdetail, ignore.case = TRUE) &
    TR2$weaptype1_txt == "Melee","stone",TR2$weapon)

TR2$weapon[TR2$weaptype1_txt=="Firearms"] <- "firearms"

TR2$weapon[TR2$weaptype1_txt=="Explosives"] <- "explosives"
TR2$weapon [TR2$weapsubtype1 ==11] <- "projectile"
TR2$weapon [TR2$weapsubtype1 ==13] <- "suicide"

# TR2$weapon[TR2$weaptype1_txt == "Incendiary" ] <- "incendiary" ; only 2 obs so better "other"
TR2$weapon [TR2$weapsubtype1 %in% c(18,20)] <- "arson"   #[18 Arson/Fire] [20 Gasoline or Alcohol]
TR2$weapon [TR2$weapsubtype1 ==19] <- "molotov_cocktail" #[19 Molotov Cocktail/Petrol Bomb]

TR2 %>% count(weapon)



# 10/01/2024 addition  weapon: firearms  | cold_weapon | projectile
# TR3 <- TR2   ----  
TR3 <- TR2     
TR3$weapon [TR3$weapon %in% c("arson","explosives","firearms","molotov_cocktail","suicide" )] <- "firearms" 
# [19 Molotov Cocktail/Petrol Bomb]
# "arson" [18 Arson/Fire] [20 Gasoline or Alcohol]
TR3$weapon [TR3$weapon %in% c("melee","sharp","stone" )] <- "cold_weapon" # Unarmed Assault
TR3$weapon [TR3$weapon %in% c("projectile")] <- "projectile"

TR3 %>% filter(weapon=="other") %>% count(attacktype1_txt)

TR3$weapon [
  TR3$attacktype1_txt %in% c("Bombing/Explosion","Facility/Infrastructure Attack") &
    TR3$weapon == "other"] <- "firearms"

TR3$weapon [TR3$weapon == "other"] <- "cold_weapon"

TR3 %>% count(weapon)

#  names(TR3)                                                     ----

# > names(TR3)
# [1] "eventid"           "date"              "country_txt"       "targtype1_txt_new" "natlty1_txt"      
# [6] "weapsubtype1"      "weaptype1_txt"     "weapsubtype1_txt"  "weapdetail"        "targtype1_txt"    
# [11] "nkill"             "nwound"            "gname"             "provstate"         "attacktype1_txt"  
# [16] "suicide"           "multiple"          "related"           "summary"           "scite1"           
# [21] "scite2"            "scite3"            "addnotes"          "weapon"            "date_cha"         
# [26] "district" 


 #### a_incidents                  ----

a_inc_district= TR3 %>% count(date,district)%>% pivot_wider(names_from = district, values_from = n)
a_inc_weap= TR3 %>% count(date,weapon) %>% pivot_wider(names_from = weapon, values_from = n)
a_inc_trgt= TR3 %>% count(date,targtype1_txt_new) %>%  pivot_wider(names_from = targtype1_txt_new, values_from = n)

a_incidents= TR3 %>% count(date)%>% rename(inc_day= n) %>% 
  full_join(a_inc_district) %>% full_join(a_inc_weap) %>% full_join(a_inc_trgt)

names(a_incidents)
names(a_incidents) <- c("date","inc_day",
                        "inc_district_yesha","inc_district_else","inc_district_north_south",
                        "inc_weapon_cold","inc_weapon_firearms","inc_weapon_projectile",
                        "inc_trgt_security_force","inc_trgt_civilians")

rm(a_inc_district,a_inc_weap,a_inc_trgt)
security_force civilians trgt  
----------------------------------------------------------------------------------------------------------

 #### a_kill                       ----

killd_district = TR3 %>% select(date,district,nkill) %>% 
  group_by(date,district) %>% summarise(killed=sum(nkill,na.rm = T)) %>% 
  pivot_wider(names_from = district, values_from = killed)

killd_weap= TR3 %>% select(date,weapon,nkill) %>% 
  group_by(date,weapon) %>% summarise(killed=sum(nkill,na.rm = T)) %>% 
  pivot_wider(names_from = weapon, values_from = killed)

killd_trgt= TR3 %>% select(date,targtype1_txt_new,nkill) %>%  
  group_by(date,targtype1_txt_new) %>% summarise(killed=sum(nkill,na.rm = T)) %>% 
  pivot_wider(names_from = targtype1_txt_new, values_from = killed)

a_kill= 
  TR3 %>% select(date,nkill) %>% 
  group_by(date)%>% summarise(killd_day=sum(nkill,na.rm = T)) %>% 
  full_join(killd_district)%>% 
  full_join(killd_weap)%>% 
  full_join(killd_trgt)

names(a_kill)
names(a_kill) <- c("date","kill_day",
                        "kill_district_yesha","kill_district_else","kill_district_north_south",
                        "kill_weapon_cold","kill_weapon_firearms","kill_weapon_projectile",
                        "kill_trgt_security_force",  "kill_trgt_civilians")

rm(killd_district,killd_weap,killd_trgt)


-------------------------------------------------------------------------------------------------------
 #### a_wound                      ----

wound_district = TR3 %>% select(date,district,nwound) %>% 
  group_by(date,district) %>% summarise(wounded=sum(nwound,na.rm = T)) %>% 
  pivot_wider(names_from = district, values_from = wounded)

wound_weap= TR3 %>% select(date,weapon,nwound) %>% 
  group_by(date,weapon) %>% summarise(wounded=sum(nwound,na.rm = T)) %>% 
  pivot_wider(names_from = weapon, values_from = wounded)

wound_trgt= TR3 %>% select(date,targtype1_txt_new,nwound) %>%  
  group_by(date,targtype1_txt_new) %>% summarise(wounded=sum(nwound,na.rm = T)) %>% 
  pivot_wider(names_from = targtype1_txt_new, values_from = wounded)

a_wound= 
  TR3 %>% select(date,nwound) %>% 
  group_by(date)%>% summarise(wound_day=sum(nwound,na.rm = T)) %>% 
  full_join(wound_district) %>% 
  full_join(wound_weap) %>% 
  full_join(wound_trgt)

names(a_wound)
names(a_wound) <- c("date","wound_day",
                    "wound_district_yesha","wound_district_else","wound_district_north_south",
                    "wound_weapon_cold","wound_weapon_firearms","wound_weapon_projectile",
                    "wound_trgt_security_force","wound_trgt_civilians")


rm(wound_district,wound_weap,wound_trgt)

### A_inc_kill_wound            ----
names(a_incidents)
names(a_kill)
names(a_wound)


A_inc_kill_wound= 
  full_join(a_incidents,a_kill) %>% full_join(a_wound) %>% # A tibble: 1,712 × 28
  full_join(security_situation) %>% rename(security_situation_yes1_no0=security_situation_num) #  10,038

A_inc_kill_wound$date_cha <- as.character(A_inc_kill_wound$date)

A_inc_kill_wound[is.na(A_inc_kill_wound)] <- 0

terror_11012024 =A_inc_kill_wound  %>% select(date,date_cha,everything())

# write.csv write_dta read_dta ----

library(haven)
library(foreign)
write_dta(terror_11012024, "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_11012024.dta")
write.csv(terror_11012024, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_11012024.csv", row.names=FALSE)










