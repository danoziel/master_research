library(tidyverse)
library(haven)


bhoomi <- read_excel("~/master_research/DATAs/ramthal_data/baseline_survey_2016/20160610_irrigation_Bhoomi_entire_sample_YR_5.xlsx")
jain_2017_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx")
ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

hissa_ifmr_2016 <- ifmr_base_2016 %>% select("Id","A9","A17", "C1",matches ("D4_"))
hissa_jain <- jain_2017_2019 %>% select(farmer_name ,village ,survey_number)
hissa_bhoomi <- bhoomi %>% select(-c(7,8,9))

ifmr_D4 <- hissa_ifmr_2016 %>% select("Id","A9","A17", "C1",matches ("D4_hissa")) %>% 
  gather( "key1", "srvy_no",5:22) %>% mutate(no=1:32598)

ifmr_hissa <- hissa_ifmr_2016 %>% select("Id","A9","A17", "C1",!matches ("D4_hissa")) %>% 
  gather( "key2", "hissa",5:22) %>% mutate(no=1:32598) %>% inner_join(ifmr_D4) %>% 
  select("Id","A9","A17", "key1","hissa","srvy_no") %>% arrange(Id) %>% 
  filter(hissa!="",!is.na(hissa))
