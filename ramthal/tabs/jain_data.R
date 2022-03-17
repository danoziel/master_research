library(tidyverse)
library(readxl)
library(dplyr)

# bind  data jain_2017_2018_2019 ==> jain_789 ----
jain_kharif_2017 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "kharif_2017")
jain_kharif_2018 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "kharif_2018")
jain_kharif_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "kharif_2019")

jain_rabbi_2017 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "rabbi_2017")
jain_rabbi_2018 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "rabbi_2018")
jain_rabbi_2019 <- read_excel("~/master_research/DATAs/ramthal_data/Jain_feb_2020/jain_february_2020.xlsx",sheet = "rabbi_2019")

jain_2017_2018_2019 <- rbind(jain_kharif_2017,jain_kharif_2018,jain_kharif_2019,jain_rabbi_2017,jain_rabbi_2018,jain_rabbi_2019)
rm(jain_kharif_2017,jain_kharif_2018,jain_kharif_2019,jain_rabbi_2017,jain_rabbi_2018,jain_rabbi_2019)

# Remove everything after backslash 
jain_2017_2018_2019$survey_plot <- gsub("(^\\d+)([\a-zA-Z0-9]*)", "\\1", jain_2017_2018_2019$survey_number)

jain_789A <- jain_2017_2018_2019
rm(jain_2017_2018_2019)

# villages names corrections----
jain_789B <- jain_789A %>% 
  mutate(village=ifelse(village == "Chinnapur S T", "Chinnapur",village)) %>% 
  mutate(village=ifelse(village == "Chinnapur ST", "Chinnapur",village)) %>% 
  mutate(village=ifelse(village == "Binjawadagi", "Binjawadgi",village)) %>% 
  mutate(village=ifelse(village == "kesarabhavi", "Kesarbhavi",village)) %>% 
  mutate(village=ifelse(village == "Hirebadawadagi", "Hirebadawadgi",village)) %>% 
  mutate(village=ifelse(village == "Kesarabhavi", "Kesarbhavi",village)) %>% 
  mutate(village=ifelse(village == "Chikkabadawadagi", "Chikkabadwadgi",village)) %>% 
  mutate(village=ifelse(village == "Chintakamldinni", "Chintakamaladinni",village)) %>% 
  mutate(village=ifelse(village == "Bekamaldinni", "Bekamaladinni",village)) %>% 
  mutate(village=ifelse(village == "Ramavadagi", "Ramawadagi",village)) %>% 
  mutate(village=ifelse(village == "Kadiwal inam", "Kadiwal",village)) %>% 
  mutate(village=ifelse(village == "Kadiwal Inam", "Kadiwal",village)) %>% 
  mutate(village=ifelse(village == "Nidasanoor", "Nidasanur",village)) %>% 
  filter(!is.na(village))


# crops names   jain_789C ----
jain_789C<-jain_789B %>% 
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
                         ifelse(crop %in% c("Maize","Cotton,Maize","maize","Maize-Sandoge"),"Maize",
                  
# Seeds                  
    ifelse(crop %in% c("Sesamum","Sunflower","Redgram,Sunflower","Redgram, Sunflower","Bgm/Snfwr","Jowar/Safflower",
                       "SF/BG","Safflower","Cotton"),"Oilseeds",
           
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
#                                      ifelse(crop2 %in% c( "Cotton"),"Cotton",
                                             ifelse(crop2 %in% c("Chilly","Onion","Other Vegetables","Papaya"),"Vegetables",
                                             crop2)))))

# freqcrop <-as.data.frame(table(jain_789C$crop2)) 
# freqcropD <-as.data.frame(table(jain_789D$cropCat)) 



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
rm(freqcrop,freqcropD)