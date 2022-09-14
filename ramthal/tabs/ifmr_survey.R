library(tidyverse)
library(haven)
library(stringr)



ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_mid_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")

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

