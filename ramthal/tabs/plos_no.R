library(tidyverse)
library(haven)
library("stringi")  

#bhoomi and jain_2017_2019 -----
bhoomi <- read_excel("~/master_research/DATAs/ramthal_data/baseline_survey_2016/20160610_irrigation_Bhoomi_entire_sample_YR_5.xlsx")
hissa_bhoomi <- bhoomi %>% select(-c(7,8,9))

jain_2017_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx")
hissa_jain <- jain_2017_2019 %>% select(farmer_name ,village ,survey_number)

# ifmr_hissa ----
ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_mid_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")

ifmr_mid_2018 == ifmr_base_2018












# hissa_ifmr_2016 ----
hissa_ifmr_2016A <- ifmr_base_2016 %>% select("Id","A9","A17","C1" ,matches ("D4_"))

hissa_ifmr_2016B <- hissa_ifmr_2016A %>% select("Id","A9","A17", "C1",matches ("D4_hissa")) %>% 
  gather( "key1", "hissa_no",5:22) %>% mutate(no=1:32598)

#NEED TO FIX
hissa_ifmr_2016C <- hissa_ifmr_2016A %>% 
  select("Id","A9","A17", "C1",!matches ("D4_hissa")) %>% 
  gather( "key2", "srvy_no",5:22) %>% 
  mutate(no=1:32598) %>% 
  inner_join(hissa_ifmr_2016B) %>% 
  select("Id","A9","A17", "C1","key2","srvy_no","hissa_no") %>% arrange(Id) %>% 
  filter(hissa_no!="" & !is.na(srvy_no))  %>% 
  filter(hissa_no!="",!is.na(hissa_no | srvy_no!="",!is.na(srvy_no)) )

rm(hissa_ifmr_2016A,hissa_ifmr_2016B,hissa_ifmr_2016C)




# hissa_ifmr_2018 ----
hissa_ifmr_2018A <- ifmr_mid_2018 %>% select("id","a5","a13","d3" ,starts_with("D3_")) 

hissa_ifmr_2018B <-hissa_ifmr_2018A %>%
  select("id","a5","a13","d3",starts_with("D3_"),
         -starts_with(c("d3_1_hissa_nu_yesno_","d3_1_status_","d3_1_hissa_nu_correct_")),
         -ends_with("cb")) %>% 
  gather("key", "hissa_srvy_no",5:14) %>% 
  rename(Id=id,village=a5,farmer_name=a13,total_plots18=d3)

hissa_ifmr_2018B[c('SRVY1', 'HISSA1')] <- str_split_fixed(hissa_ifmr_2018B$hissa_srvy_no, '-', 2)
hissa_ifmr_2018B$HISSA2 <- str_replace(hissa_ifmr_2018B$HISSA1,"999-","")
hissa_ifmr_2018B$hissa <- str_replace(hissa_ifmr_2018B$HISSA2,"666-","")
hissa_ifmr_2018B <- hissa_ifmr_2018B[,c( 1:7,10)]
hissa_ifmr_2018B$nin <- str_replace_all(hissa_ifmr_2018B$hissa_srvy_no, "-999", NA_character_)
hissa_ifmr_2018B <- hissa_ifmr_2018B %>% mutate(NIN=ifelse(is.na(nin),"-999","")) 

hissa_ifmr_2018c <- hissa_ifmr_2018B %>% 
  mutate(NIN=ifelse(is.na(nin),"-999","")) %>% 
  mutate(srvy=ifelse(NIN=="-999","-999",SRVY1)) %>% 
  mutate(hissa=ifelse(hissa=="999","-999",hissa)) %>% 
  mutate(hissa=ifelse(hissa=="666","-666",hissa)) %>% 
  filter(hissa_srvy_no !="" | srvy !="" | hissa !="") %>% 
  select(1:3,srvy,hissa)

ifmr_srvy_hissa <- 
  ifmr_base_2016 %>% select("Id","A17") %>% 
  rename(farmer_name_16=A17) %>% 
  inner_join(hissa_ifmr_2018c) %>% 
  rename(farmer_name_18=farmer_name) %>% 
  select(Id,village,farmer_name_16,farmer_name_18,srvy,hissa)

# Plots_list ----
Plots_list<- read_excel("~/master_research/DATAs/ramthal_data/ifmr_ml_2022_01112022.xlsx",
                        sheet = "Plots_list")

Plots <- Plots_list[,1:11]

names(Plots)

Plots[c('SRVY1', 'HISSA1')] <- str_split_fixed(Plots$srvy_hissa, '-', 2)
Plots$HISSA2 <- str_replace(Plots$HISSA1,"999-","")
Plots$hissa <- str_replace(Plots$HISSA2,"666-","")
Plots <- Plots[,-c(13:14 )]
Plots$nin <- str_replace_all(Plots$srvy_hissa, "-999", NA_character_)
Plots <- Plots %>% mutate(NIN=ifelse(is.na(nin),"-999","")) 

PlotsA <- Plots %>% 
  mutate(NIN=ifelse(is.na(nin),"-999","")) %>% 
  mutate(srvy=ifelse(NIN=="-999","-999",SRVY1)) %>% 
  mutate(hissa=ifelse(hissa=="999","-999",hissa)) %>% 
  mutate(hissa=ifelse(hissa=="666","-666",hissa)) %>% 
  filter(!is.na(plot)) %>% 
  filter(farmer_name != "NA")

plots_list1 <- PlotsA 

ifmr_srvy_hissa_crop_size <- 
  ifmr_base_2016 %>% select("Id","A17") %>% 
  rename(farmer_name_16=A17) %>% 
  inner_join(plots_list1) %>% 
  rename(farmer_name_18=farmer_name) %>% 
  select( Id:farmer_name_16 ,farmer_name_18,village,
          total_plots , plot,
          srvy, hissa,rabi_crop:plot_size_guntas) %>% 
  kable() %>% kable_minimal()








# -----
hissa_ifmr_2018C <-hissa_ifmr_2018A %>%
  select("id","a5","a13","d3",starts_with("d3_1_hissa_nu_correct_")) %>% 
  add_column(d3_1_hissa_nu_correct_9="correct_9",d3_1_hissa_nu_correct_10="correct_10") %>% 
  gather("key2", "hissa_srvy_correct",5:14)

hissa_ifmr_2018D <- bind_cols(Plots,hissa_ifmr_2018C) %>% 
  select(Id,village,farmer_name,total_plots,key1,hissa_srvy_no,hissa_srvy_correct) %>% 
  filter(hissa_srvy_no!="",!is.na(hissa_srvy_no)) %>% mutate(KeyQ="m18")

rm(hissa_ifmr_2018A,Plots,hissa_ifmr_2018C)

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

library("writexl")
write_xlsx(ifmr_hissa_2016_2018,"~/master_research/DATAs/ramthal_data/ifmr_hissa_2016_2018.xlsx")


# Plots list by crop/size

ifmr_base_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")
ifmr_base_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")








# NEW LIST: PLOTS-CROP-HISSA ----

# D3	How many plots of land does your household currently own?
#   FOR EACH CURRENTLY OWNED SURVEY NUMBER:
# D4	Survey/hissa number
# D5	Village in which survey plot is located
# D6	Area of Plot (acres/gunta)




  
  
  



write.csv(x104349, file = "C:/Users/Dan/Documents/x104349.csv", row.names=FALSE)

xx103216 <- plot_ifmr_2016A %>% filter(Id== "103216") %>% select(1:6,39:40,73,167:174) %>% 
  unite("S2_crop", D24_2_Crop_1_1:D24_2_Crop_4_os_1, na.rm = TRUE, remove = FALSE)

write.csv(FILE,"C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/FILE.csv", row.names=FALSE)
FILE <- read.csv("~/master_research/DATAs/ramthal_data/FILE.csv")