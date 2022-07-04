library(tidyverse)
library(haven)
library(stringr)



ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_mid_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")

# HH roster_2016 ----
HH_roster_2016 <- ifmr_base_2016 %>% 
  select("Id","A17", "C1",matches ("C2_"), matches ("C3_"), matches ("C4_"),matches ("C5_"))
  
rstr_id <-   ifmr_base_2016 %>% select("Id","A17", "C1") %>%  rename(HH_name = A17)

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
  filter(member_name!= "") %>% mutate(id_m=str_c(Id,key)) %>% select(id_m,everything(),-c(key,C1))

rm(HH_roster_2016,rstr_member_name,rstr_gender,rstr_age,rstr_relationship)

write.csv(roster_2016, file = "~/master_research/DATAs/ramthal_data/baseline_survey_2016/roster_2016.csv", row.names=FALSE)


# HH plots_2016 ----
HH_plots_2016 <- ifmr_base_2016 %>% 
  select("Id","A17", "D3", matches ("D4_"),matches ("D5_"))

