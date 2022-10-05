library(tidyverse)
library(haven)
library(stringr)
library(readxl)
#Irrigation_Midline_Clean ----

Irrigation_Midline_Clean_20180713 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180713.dta")
Irrigation_Midline_Clean_20180619 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180619.dta")
H <- Irrigation_Midline_Clean_20180619 %>% select(id,starts_with("c"))
# ----

Irrigation_Midline_Clean_20180709 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180709.dta")
H1557 <- Irrigation_Midline_Clean_20180709 %>% select(id,starts_with("c"))

YR_Ramthal_Data_Entry <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry.dta")
Sample_rmtl <- YR_Ramthal_Data_Entry %>% select(id, in_out)





#DATABASES -----

shape_code <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/shape_code.dta")

village_list4 <- read_excel("~/master_research/DATAs/ramthal_data/village_list4.xlsx")
village_list <- village_list4 %>% select(village,a6) %>% rename(village_code=a6)

ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_mid_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")

#YR_Ramthal_Data_Entry ----
YR_Ramthal_Data_Entry <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry.dta")

#mid 2018 - in&out layer distance_km 
#           around_boundary south1_north0
YR_Ramthal_Data_Entry_2 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2.dta")

# לא ברור מה זה 126 תצפיות
YR_Ramthal_Data_Entry_2_stata12 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata12.dta")
YR_stata12 <- YR_Ramthal_Data_Entry_2_stata12 %>% select(id,starts_with("c2"))
# ==YR_Ramthal_Data_Entry_2
YR_Ramthal_Data_Entry_2_stata13 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata13.dta")
YR_stata13 <- YR_Ramthal_Data_Entry_2_stata13 %>% select(id,starts_with("c2"))

rm(YR_Ramthal_Data_Entry,YR_Ramthal_Data_Entry_2,
   YR_Ramthal_Data_Entry_2_stata12,YR_Ramthal_Data_Entry_2_stata13)
# -----

crop <- ifmr_base_2016 %>% filter(Id=="100014") %>% 
  select(Id,matches ("D4_"),matches ("D24"))
crop <- crop[,c(1:5,36:50)]

crop1 <- crop[,c(1:3,8,16)] 
names(crop1) <- c('ID', 'svy', 'hisa', 'crop1','crop2')

crop2 <- crop[,c(1,4:5,10,18)]
names(crop2) <- c('ID', 'svy', 'hisa', 'crop1','crop2')

crop0 <- crop[,c(1,6:7,12,20)]
names(crop0) <- c('ID', 'svy', 'hisa', 'crop1','crop2')
crop <- rbind(crop0,crop1,crop2)

HH100014 <-YR_Ramthal_Data_Entry %>%  filter(id=="100014") %>% select(147:279)
HH100014 <- HH100014[,-c(6,13,15:111,116:125,127)]


# HH roster_2016 ----
HH_roster_2016 <- ifmr_base_2016 %>% 
  select("Id","A17","A9","C1",matches ("C2_"), matches ("C3_"), matches ("C4_"),matches ("C5_"))
  
rstr_id <-   ifmr_base_2016 %>% select("Id","A17","A9","C1") %>%  rename(HH_name = A17,village= A9)

rstr_member_name <- ifmr_base_2016 %>% select("Id",matches ("C2_")) %>% 
  gather( "key", "member_name",2:31) %>%mutate( key = str_sub(key, 3, 5))

rstr_gender <- ifmr_base_2016  %>% select("Id",matches ("C3_")) %>% 
  gather( "key", "gender", 2:31) %>% mutate( key = str_sub(key, 3, 5))

rstr_age <- ifmr_base_2016  %>% select("Id",matches ("C4_")) %>% 
  gather( "key", "age", 2:31) %>% mutate( key = str_sub(key, 3, 5))

rstr_relationship  <- ifmr_base_2016 %>% select("Id",matches ("C5_"),-contains("os")) %>% 
  gather( "key", "relationship", 2:31) %>%mutate( key = str_sub(key, 3, 5))


roster_2016 <- left_join(rstr_id,rstr_member_name) %>%
  left_join(rstr_gender) %>% left_join(rstr_age) %>% left_join(rstr_relationship) %>% 
  filter(member_name!= "") %>% mutate(id_m=str_c(Id,key)) %>% select(id_m,everything(),-c(key,C1)) %>%
  mutate(relationship_code = relationship ) 

rm(HH_roster_2016,rstr_id,rstr_member_name,rstr_gender,rstr_age,rstr_relationship)

roster_2016$relationship[roster_2016$relationship == 1] <- "self"
roster_2016$relationship[roster_2016$relationship == 2] <- "wife/husband"
roster_2016$relationship[roster_2016$relationship == 3] <- "daughter/son"
roster_2016$relationship[roster_2016$relationship == 4] <- "mother/father"
roster_2016$relationship[roster_2016$relationship == 5] <- "sister/brother"
roster_2016$relationship[roster_2016$relationship == 6] <- "grandparent"
roster_2016$relationship[roster_2016$relationship == 7] <- "mother/father_in_law"
  roster_2016$relationship[roster_2016$relationship == 8] <- "sister/brother_in_law"
  roster_2016$relationship[roster_2016$relationship == 9] <- "daughter/son_in_law"
  roster_2016$relationship[roster_2016$relationship == 10] <- "aunt/uncle"
  roster_2016$relationship[roster_2016$relationship == 11] <- "cousin"
  roster_2016$relationship[roster_2016$relationship == 12] <- "niece/nephew"
  roster_2016$relationship[roster_2016$relationship == 13] <- "grandchild"

write.csv(roster_2016, file = "~/master_research/DATAs/ramthal_data/baseline_survey_2016/roster_2016.csv", row.names=FALSE)

library("writexl")
write_xlsx(roster_2016,"~/master_research/DATAs/ramthal_data/iroster_2016.xlsx")


# HH plots_2016 ----
HH_plots_2016 <- ifmr_base_2016 %>% 
  select("Id","A17", "D3", matches ("D4_"),matches ("D5_"))

names(roster_2016)

# HH roster_2018 ----

# a13_1 == Interviewed/Not Interviewed in the baseline survey

H1557 <- Irrigation_Midline_Clean_20180709 %>% select(id,starts_with("c"))
roster_2018A <- ifmr_mid_2018 %>% select("id","a13","a13_1","a5","c1","c1_exist") %>% 
  rename(Id=id)

m1 <- H1557 %>%   select("id", 7:8,11:13) +15+3
names(m1) <- c('Id', 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m2 <- H1557 %>%   select("id", 22:23,26:28)
names(m2) <- c('Id', 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m3 <- H1557 %>%   select("id", 34:35,38:40)
names(m3) <- c('Id', 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m4 <- H1557 %>%   select("id", 47:48,51:53)
names(m4) <- c('Id', 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m5 <- H1557 %>%   select("id", 59:60,63:65)
names(m5) <- c('Id', 'member_name', 'previously_been_HH',"gender","age","relationship_code")

roster_2018B <- rbind(m1,m2,m3,m4,m5) %>% 
  full_join(roster_2018A) # %>% select(1) %>% distinct()

