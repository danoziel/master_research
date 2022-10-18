library(tidyverse)
library(readxl)
library(haven)

# DBs----

shape_code <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/shape_code.dta")


IDs_18 <- ifmr_mid_2018 %>% select(id,a5,a6,a10,a13)

Jain_crop_17_22 %>% group_by(season) %>% count()

village_list4 <- read_excel("~/master_research/DATAs/ramthal_data/village_list4.xlsx")
village_list <- village_list4[,c(1,2,5)]
  
village_list <- village_list %>% 
  mutate(village=ifelse(a5 == "binjawadagi", "Binjawadagi",village)) %>% 
  mutate(village=ifelse(village == "Ghattignur", "Ghattignur",village)) #%>% 

IDs_ji <- Jain_crop_17_22 %>%
  filter(season=="rabi_20_21") %>% 
  select (farmer_name,village,survey_hissa,survey_number) %>%
  full_join(village_list)

  
IDs_jiV <- IDs_ji%>% select (village,survey_number) %>% distinct()
IDs_jiV2 <- IDs_ji%>% select (village) %>% distinct()

Ghattignur
Ghattignur


IDs_ji$survey_number <- gsub("(^\\d+)([\a-zA-Z0-9]*)", "\\1", IDs_ji$survey_number)





Malakajappa Tumbagi S/O Shivarudrappa - Ashok S Hosamani


# id_yoav----

Jain_crop_17_22

jain_789F <- 
  jain_789E %>% filter(survey_plot>0,a6>0)

# Adding Leading digits to the Elements of a Vector
library(stringr)
jain_789F$a6 <- str_pad(jain_789F$a6, 2, pad = "0")
jain_789F$a6 <- str_pad(jain_789F$a6, 3, pad = "1")
jain_789F$survey_plot <- str_pad(jain_789F$survey_plot, 3, pad = "0")
jain_789F$id_yoav <- str_c(jain_789F$a6,jain_789F$survey_plot)

rm(jain_789A,jain_789B,jain_789C,jain_789D,jain_789E)





Jain_kharif_2022 <- Jain_kharif2022 %>%
  select(farmer_name,Season,survey_number,
         village_zone_1,village_zone_2,village_zone_3,village_zone_4,
         Main_Crop,Second_Crop,Date_sowing_Main_Crop,plot_size_ha_001) %>% 
  mutate(village= case_when(village_zone_1== as.character()~village_zone_1)
           )






