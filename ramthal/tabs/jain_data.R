library(tidyverse)
library(readxl)
library(dplyr)
library(kableExtra)

jain_789A
jain_2017_2018_2019
Jain_2020_2021
jain_171819

# databases ----
library(readxl)
library(readr)
village_code <- village_list4 %>% select(village,a6) %>% rename(village_code=a6) 
ifmr_16 <- ifmr_base_2016 %>% select(A9,Id) %>% arrange(A9) %>% rename(village=A9)
ifmr_18 <-ifmr_mid_2018 %>% select(a5,id) %>% arrange(a5) %>% rename(village=a5)
jain_missing_villages <- data.frame(x=budihal,gadisunkapur,gorabal,hachanur,hulgera,hullalli,jalakamaladini,konnur,nagur)

jain_kharif_2017 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "kharif_2017")
jain_kharif_2018 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "kharif_2018")
jain_kharif_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "kharif_2019")
jain_rabbi_2017 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "rabbi_2017")
jain_rabbi_2018 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "rabbi_2018")
jain_rabbi_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_data/jain_february_2020.xlsx",sheet = "rabbi_2019")
Jain_2020_2021 <- read_csv("~/master_research/DATAs/ramthal_data/Jain_data/Jain_2020_2021.csv",show_col_types = FALSE)

jain_kharif_2017_kharif_2022 <- rbind(jain_kharif_2017,jain_rabbi_2017,
                             jain_kharif_2018,jain_rabbi_2018,
                             jain_kharif_2019,jain_rabbi_2019,
                             Jain_2020_2021)

rm(jain_kharif_2017,jain_kharif_2018,jain_kharif_2019,jain_rabbi_2017,jain_rabbi_2018,jain_rabbi_2019,Jain_2020_2021)


# Remove everything after backslash 
jain_17_18_19$survey_number <- gsub("(^\\d+)([\a-zA-Z0-9]*)", "\\1", jain_789A$survey_hissa)
jain_17_18_19$hissa_number <-   171819

jain_17_18_19 %>%
  select("si_number","farmer_name","zone","block","village","survey_hissa",
         "survey_number","hissa_number","area_ha","crop","sowing_date",
         "season","existing_crop_kharif_2017","newly_sown_crop_kharif","total_area")           

Jain_crop_17_22%>% mutate(area_acre=area_ha*2.4710538147)

# crops names   jain_789C ----
ji<-jain_kharif_2017_kharif_2022 %>% 
  mutate(id_num = row_number())
library(stringr)
ji$ID <- str_pad(ji$id_num, 2, pad = "0")
jain_789F$a6 <- str_pad(jain_789F$a6, 3, pad = "1")
jain_789F$survey_plot <- str_pad(jain_789F$survey_plot, 3, pad = "0")
jain_789F$id_yoav <- str_c(jain_789F$a6,jain_789F$survey_plot)

ji_aa<-ji %>% filter(crop == "Onion/Chilly") %>% 
    `azsxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`

ji_a<-ji %>% 
  select("si_number","crop","village","survey_hissa",
         "survey_number","hissa_number","area_ha","season") %>% 
  separate_rows(crop, sep = ",")


  mutate(crop2=
#Pulse
    ifelse(crop %in% c("Redgram","Greengram, Redgram","redgram","Redgram, Greengram","Redgram, Maize"), "Redgram",
           ifelse(crop %in% c("Greengram","Greengram, Redgram"),"Greengram",
                  ifelse(crop %in% c("Bengalgram","Bgm/Jwr","Bgm/jwr","BG/Maize","Groundnut/Bg"),"Bengalgram",
# Cereals                       
    ifelse(crop %in% c("Jowar","Bgm/Jowar"),"Jowar",
           ifelse(crop %in% c("Wheat","bgm/wheat","wheat","Wheat/Jowar","Bgm/Wheat",
                       "Jwr/Wheat","Wheat/Bgm","Jwr/Wheat/BG","Wheat,Bgm","Wheat/Jwr"),"Wheat",
                  ifelse(crop %in% c("Foxtail millet","Foxtailmillet","foxtail Millet","Foxtail","Bajra","Bajara"),"Millet",
                         ifelse(crop %in% c("Maize","maize","Maize-Sandoge"),"Maize",
                  
# Seeds                  
    ifelse(crop %in% c("Sesamum","Sunflower","Redgram,Sunflower","Redgram, Sunflower","Bgm/Snfwr","Jowar/Safflower",
                       "SF/BG","Safflower","Cotton","Cotton,Maize"),"Oilseeds",
           
# Vegetables                                     
    ifelse(crop %in% c("Chilli","Chilly","chilly","Chilly,Jowar","Chilly,Sugar","Redgram,Chilly",
                       "Chilly,Onion", "Chilly,Onion,Bg","Onion,Jowar","Chilli-badagi","Bgm/Chilly",
                       "Chilly,Onion,Jowar","Chilly.jowar"),"Chilly",
           ifelse(crop %in% c("Onion","onion","Onion,Chilli","Onion,Chilly","Bgm/Onion"),"Onion",
                  ifelse(crop %in% c("Vegetable","Vegetables","Cucumber","Coriandor","Corainder",
                                     "Coriendor","Corriander","Coriander","Brinjal","Garlic","BG/Veg","Gherkin","Bhendi"), "Other Vegetables",
                         "Others"))))))))))))

# crops names  cropCat ----

jain_789D<-jain_789C %>% 
  mutate(cropCat=ifelse(crop2 %in% c("Greengram","Redgram","Bengalgram","Cowpea","Blackgram"),"Pulses",
                        ifelse(crop2 %in% c("Jowar","Wheat","Millet","Maize","Soybean","Soyabean"),"Cereals",
                               ifelse(crop2 %in% c( "Oilseeds","Yallu"),"Oilseeds",
                                             ifelse(crop2 %in% c("Chilly","Onion","Other Vegetables","Papaya"),"Vegetables",
                                             crop2)))))




# add: village_list   ----

village_list <- read_excel("~/master_research/DATAs/ramthal_data/village_list.xlsx")
village_list <- village_list %>%dplyr:: select(village,a6)

jain_789E <- jain_789D %>% left_join(village_list) 

# id_yoav----

jain_789F <- 
  jain_789E %>% filter(survey_plot>0,a6>0)

# Adding Leading digits to the Elements of a Vector
library(stringr)
jain_789F$a6 <- str_pad(jain_789F$a6, 2, pad = "0")
jain_789F$a6 <- str_pad(jain_789F$a6, 3, pad = "1")
jain_789F$survey_plot <- str_pad(jain_789F$survey_plot, 3, pad = "0")
jain_789F$id_yoav <- str_c(jain_789F$a6,jain_789F$survey_plot)

rm(jain_789A,jain_789B,jain_789C,jain_789D,jain_789E)


# =========================================================================================
# =========================================================================================  


Jain_kharif_2022 <- Jain_kharif2022 %>%
  select(farmer_name,Season,survey_number,
         village_zone_1,village_zone_2,village_zone_3,village_zone_4,
         Main_Crop,Second_Crop,Date_sowing_Main_Crop,plot_size_ha_001) %>%
  mutate(village = village_zone_1) %>% 
  mutate(village = case_when(!is.na(village_zone_2) ~ village_zone_2,
                             !is.na(village_zone_3) ~ village_zone_3,
                             !is.na(village_zone_4) ~ village_zone_4,
                             TRUE ~ village))




