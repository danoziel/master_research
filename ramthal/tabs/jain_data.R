library(tidyverse)
library(sp)
library(raster)
library(sf)
library(rgdal)
library(readxl)
library(dplyr)

# bind  data jain_2017_2018_2019 ----
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

jain_171819 <- jain_2017_2018_2019
# villages names ----

jain_171819 <- jain_171819 %>% 
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

# village_list ----

village_list <- read_excel("~/master_research/DATAs/ramthal_data/village_list.xlsx")
village_list <- village_list %>% select(village,a6)

jain_171819 <- jain_171819 %>% left_join(village_list) 

# id_yoav----

vec <- jain_171819 %>%
 # select("farmer_name","village","survey_plot","a6")%>%
  filter(survey_plot>0,a6>0)

# Adding Leading digits to the Elements of a Vector
library(stringr)
vec$a6 <- str_pad(vec$a6, 2, pad = "0")
vec$a6 <- str_pad(vec$a6, 3, pad = "1")
vec$survey_plot <- str_pad(vec$survey_plot, 3, pad = "0")
vec$id <- str_c(vec$a6,vec$survey_plot)

vec <- vec %>%select("id","village","crop","area_ha","season")

names(vec)


vb <- banihatti_data_id %>% left_join(vec)
