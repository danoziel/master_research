library(tidyverse)
library(haven)
library("stringi")  

#bhoomi and jain_2017_2019 ----
bhoomi <- read_excel("~/master_research/DATAs/ramthal_data/baseline_survey_2016/20160610_irrigation_Bhoomi_entire_sample_YR_5.xlsx")
hissa_bhoomi <- bhoomi %>% select(-c(7,8,9))

jain_2017_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx")
hissa_jain <- jain_2017_2019 %>% select(farmer_name ,village ,survey_number)

# ifmr_hissa ----
ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_base_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")

# hissa_ifmr_2016 ----
hissa_ifmr_2016A <- ifmr_base_2016 %>% select("Id","A9","A17","C1" ,matches ("D4_"))

hissa_ifmr_2016B <- hissa_ifmr_2016A %>% select("Id","A9","A17", "C1",matches ("D4_hissa")) %>% 
  gather( "key1", "srvy_no",5:22) %>% mutate(no=1:32598)

hissa_ifmr_2016C <- hissa_ifmr_2016A %>% select("Id","A9","A17", "C1",!matches ("D4_hissa")) %>% 
  gather( "key2", "hissa",5:22) %>% mutate(no=1:32598) %>% inner_join(hissa_ifmr_2016B) %>% 
  select("Id","A9","A17", "C1","key2","hissa","srvy_no") %>% arrange(Id) %>% 
  filter(hissa!="",!is.na(hissa)) 

hissa_ifmr_2016D <- hissa_ifmr_2016C%>%
  unite("hissa_srvy_no", hissa:srvy_no,sep= "-",remove = FALSE) %>% 
  mutate(hissa_srvy_no=ifelse(srvy_no=="",hissa,hissa_srvy_no)) %>% 
  rename(village=A9,farmer_name=A17,total_plots=C1,key1=key2) %>% 
  select(-c(srvy_no,hissa)) %>% 
  filter(key1 != "D4_0")

rm(hissa_ifmr_2016A,hissa_ifmr_2016B,hissa_ifmr_2016C)

# hissa_ifmr_2018 ----
hissa_ifmr_2018A <- ifmr_base_2018 %>% select("id","a5","a13","d3" ,starts_with("D3_")) 

hissa_ifmr_2018B <-hissa_ifmr_2018A %>%
  select("id","a5","a13","d3",starts_with("D3_"),
         -starts_with(c("d3_1_hissa_nu_yesno_","d3_1_status_","d3_1_hissa_nu_correct_")),
         -ends_with("cb")) %>% 
  gather("key1", "hissa_srvy_no",5:14) %>% 
  rename(Id=id,village=a5,farmer_name=a13,total_plots=d3)

hissa_ifmr_2018C <-hissa_ifmr_2018A %>%
  select("id","a5","a13","d3",starts_with("d3_1_hissa_nu_correct_")) %>% 
  add_column(d3_1_hissa_nu_correct_9="correct_9",d3_1_hissa_nu_correct_10="correct_10") %>% 
  gather("key2", "hissa_srvy_correct",5:14)

hissa_ifmr_2018D <- bind_cols(hissa_ifmr_2018B,hissa_ifmr_2018C) %>% 
  select(Id,village,farmer_name,total_plots,key1,hissa_srvy_no,hissa_srvy_correct) %>% 
  filter(hissa_srvy_no!="",!is.na(hissa_srvy_no)) %>% mutate(KeyQ="m18")

rm(hissa_ifmr_2018A,hissa_ifmr_2018B,hissa_ifmr_2018C)

# ifmr_hissa_16_18 = hissa_ifmr_2016 + hissa_ifmr_2018
hissa_ifmr_2016E <- hissa_ifmr_2016D %>% mutate(hissa_srvy_correct="") %>% mutate(KeyQ="b16")

# ifmr_hissa_16_18 ----
ifmr_hissa_16_18A <- bind_rows(hissa_ifmr_2016E,hissa_ifmr_2018D) %>% 
  arrange(Id) %>% mutate(hissa_srvy_no=ifelse(hissa_srvy_no=="-999--999","-999",hissa_srvy_no) )

rm(hissa_ifmr_2016D,hissa_ifmr_2016E,hissa_ifmr_2018D)

#remove the "-"
ifmr_hissa_16_18A$hissa_srvy_no <- gsub("-$","",ifmr_hissa_16_18A$hissa_srvy_no )
ifmr_hissa_16_18A$hissa_srvy_correct <- gsub("-$","",ifmr_hissa_16_18A$hissa_srvy_correct )

ifmr_hissa_16_18B <-
  ifmr_hissa_16_18A %>%
  mutate(hissa_srvy_correct=ifelse(hissa_srvy_correct %in% c("correct_9","correct_10"),"",hissa_srvy_correct))

AA <- ifmr_hissa_16_18B 

BB <- AA %>% select(Id,hissa_srvy_no, hissa_srvy_correct) %>% 
  filter(!hissa_srvy_correct =="") 

CC <- AA %>% group_by(Id) %>%  distinct(hissa_srvy_no, .keep_all = TRUE)%>% 
  select(-hissa_srvy_correct) %>% left_join(BB)

DD <- CC %>% 
  mutate(hissa_srvy=ifelse(is.na (hissa_srvy_correct),hissa_srvy_no,hissa_srvy_correct) ) %>% 
  mutate(baseline_wrong_no=ifelse(hissa_srvy_correct==hissa_srvy,hissa_srvy_no,"") ) %>% 
  select(Id,village,farmer_name,hissa_srvy,baseline_wrong_no,KeyQ)
  
ifmr_hissa_16_18 <- DD
ifmr_hissa_16_18$baseline_wrong_no[is.na (ifmr_hissa_16_18$baseline_wrong_no)] <- ""
rm(ifmr_hissa_16_18A,ifmr_hissa_16_18B,AA,BB,CC,DD)  

farmers16 <- ifmr_base_2016 %>% select("Id","A9","A17")
farmers18 <- ifmr_base_2018 %>% select("id","a5","a13") %>% rename(Id=id)
farmers <- left_join(farmers16,farmers18) %>%
  rename(HH_name_2016=A17,HH_name_2018=a13) %>% 
  select(Id,HH_name_2016,HH_name_2018)

ifmr_hissa_2016_2018 <-left_join(ifmr_hissa_16_18,farmers) %>% 
  rename(baseline_wrong_number=baseline_wrong_no) %>% 
  select(Id,village,HH_name_2016,HH_name_2018,hissa_srvy,baseline_wrong_number,KeyQ)

rm(farmers16,farmers18,farmers)

write.csv(ifmr_hissa_2016_2018,"~/master_research/DATAs/ramthal_data/ifmr_hissa_2016_2018.csv", row.names = FALSE)











