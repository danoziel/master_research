library(tidyverse)
library(haven)
library(stringr)
library(readxl)


# Databases created in this Rscript ----

#roster 6/10/2022
rmtl_roster_2016_2018 <- read.csv("~/master_research/DATAs/ramthal_data/FILE.csv")

# farmer own  6 -19 plots in midline 2018
H1557_2018_m6_m19 <- read.csv("~/master_research/DATAs/ramthal_data/FILE.csv")

#Irrigation_Midline_Clean ----

Irrigation_Midline_Clean_20180709 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180709.dta")
Irrigation_Midline_Clean_20180619 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180619.dta")
# ----

#List of household members added in 2018
Irrigation_Midline_Clean_20180709 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180709.dta")
H1557 <- Irrigation_Midline_Clean_20180709 %>% select(id,starts_with("c"))

#List of household IN and OUT the project (2016)
YR_Ramthal_Data_Entry <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry.dta")
Sample_rmtl <- YR_Ramthal_Data_Entry %>% select(id, in_out)


#DATABASES -----

# IDs Matching- survey and GIS
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


# HH roster_2018 ----

# a13_1 == Interviewed/Not Interviewed in the baseline survey

#List of household members added in 2018
H1557 <- Irrigation_Midline_Clean_20180709 %>% select(id,starts_with("c"))
roster_2018A <- ifmr_mid_2018 %>% select("id","a13","a13_1","a5","c1","c1_exist") %>% 
  rename(Id=id)

#Lists: id, name, village- 2016 2018
idNname_2016 <- roster_2016 %>% select(Id,HH_name,village) %>% distinct() %>% 
  rename(HH_name_2016=HH_name) 
idNname_2018 <- roster_2018A %>% select("Id","a13") %>% distinct() %>% 
  rename(HH_name_2018=a13) 
Id_names_vil_16_18 <- full_join(idNname_2016,idNname_2018)

# HH members 1-5 (2018) 
m1 <- H1557 %>%   select("id","C1_exist_","C1_", 7:8,11:13)
names(m1) <- c('Id',"total_HH_2016","total_HH_2018", 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m2 <- H1557 %>%   select("id","C1_exist_","C1_", 22:23,26:28)
names(m2) <- c('Id',"total_HH_2016","total_HH_2018", 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m3 <- H1557 %>%   select("id","C1_exist_","C1_", 34:35,38:40)
names(m3) <- c('Id',"total_HH_2016","total_HH_2018", 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m4 <- H1557 %>%   select("id","C1_exist_","C1_", 47:48,51:53)
names(m4) <- c('Id',"total_HH_2016","total_HH_2018", 'member_name', 'previously_been_HH',"gender","age","relationship_code")

m5 <- H1557 %>%   select("id","C1_exist_","C1_", 59:60,63:65)
names(m5) <- c('Id',"total_HH_2016","total_HH_2018", 'member_name', 'previously_been_HH',"gender","age","relationship_code")

roster_2018_m1_m5 <- rbind(m1,m2,m3,m4,m5) 
rm(m1,m2,m3,m4,m5)

# HH members 6-19 (2018) - I edited manually in Excel
H1557_2018_m6_m19 <- H1557 [,c(1,5:6,72:216)]
write.csv(H1557_2018_m6_m19, file = "C:/Users/Dan/Documents/H1557_2018_m6_m19.csv", row.names=FALSE)

# Combine 1-5 and 6-19 (2018)
roster_2018C <-
  rbind(roster_2018_m1_m5,H1557_2018_m6_m19) %>%
  filter(member_name!="") %>% 
  left_join(Id_names_vil_16_18) %>% 
  select("Id",village,HH_name_2016,"HH_name_2018","member_name",
         "gender","age","relationship_code",previously_been_HH,total_HH_2016,total_HH_2018) %>% 
  mutate(year="midline_2018")

#Combine HHm 2018 with HHm 2016
bl_roster_2016 <- roster_2016 %>% 
  mutate(previously_been_HH="",total_HH_2016="",total_HH_2018="",HH_name_2018="") %>% 
  rename(HH_name_2016=HH_name) %>% 
  select(Id,village,"HH_name_2016",HH_name_2018,"member_name",
         "gender","age","relationship_code",previously_been_HH,total_HH_2016,total_HH_2018)%>% 
  mutate(year="baseline_2016")

#Full roster of members from 2016 and 2018
rmtl_roster_2016_2018 <-
  rbind(bl_roster_2016,roster_2018C) %>% 
  arrange(Id)

write.csv(rmtl_roster_2016_2018, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/rmtl_roster_2016_2018.csv", row.names=FALSE)






# HH plots_2016 ----
HH <- ifmr_base_2016 %>% 
  select("Id","A17", "D3", matches ("C39_"),matches ("C40_"))


table(HH$C40_1)
  
H39 <- HH[,1:14] %>% gather( "key", "INR", 4:14)
H40 <- HH[,c(1:3,15:25)] %>% gather( "key1", "frq", 4:14)
H79 <- cbind(H40,H39) %>% select(1,frq,INR)%>% filter(frq>0) #%>% filter(INR>0)

H_00 <- H79 %>% filter(frq==1,INR>0)



DSF1 <- ifmr_base_2016 %>% select(1,A18,A19,A12) 
DSF2 <- ifmr_mid_2018 %>% select (1,a14,a15,a8) %>%  
  rename(Id=id, door_Num=a8 ,spouse_name=a14,father_name=a15)	

DSF1 %>% left_join(DSF2) %>% kable() %>% kable_minimal()


write.csv(FILE,"C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/FILE.csv", row.names=FALSE)
FILE <- read.csv("~/master_research/DATAs/ramthal_data/FILE.csv")