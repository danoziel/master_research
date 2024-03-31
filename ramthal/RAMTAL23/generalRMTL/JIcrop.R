
library(readr)
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

ji=Jain_2020_2021
ji$survey_number <- gsub("\\*1B", "", ji$survey_number)
ji$survey_number <- gsub("\\*", "", ji$survey_number)
ji$survey_number <- gsub("\\/3,4", "", ji$survey_number)
ji$survey_number <- gsub("B", "", ji$survey_number)
ji$survey_number <- gsub("A", "", ji$survey_number)

# ji %>% filter(si_number %in% c(1440,362,676,425,2967 ))
# # ji$survey_number <-as.numeric(ji$survey_number)
# ji %>% count(survey_number) %>% as.data.frame()

#| ji=ji %>% rename(a5=village) %>% mutate(village_code=ifelse(a5 %in% ...
                                                              
library(stringr)
ji$id_village_code <- str_pad(ji$village_code, 3, pad = "1")
ji$id_survey_number <- str_pad(ji$survey_number, 3, pad = "0")
ji$id <- str_c(ji$id_village_code,ji$id_survey_number)

zone_block_JI= # A tibble:127 
  ji %>% select(zone,block )%>%  distinct()

block_JI= # A tibble:1,928 | id:1,827
  ji %>% select(id,block ) %>% filter(!is.na(id)) %>%  distinct() %>% 
  group_by(id) %>% mutate(id_is_twice=n())

irrigation_HH %>% left_join(shp_index22) %>% mutate(id=as.character(id)) %>%
  select(id,hh_id ,hh_irrigated) %>% 
  group_by(id) %>% 
  summarise(N=n( ),sum_irri=sum(hh_irrigated)) %>% left_join(block_JI) %>% 
  group_by(block) %>% 
  summarise(N=sum(N),sum_irri=sum(sum_irri)) %>% 
  filter(!is.na(block)) %>% 
  left_join(zone_block_JI) %>% 
  group_by(zone) %>% 
  summarise(N=sum(N),sum_irri=sum(sum_irri)) %>% 
  mutate(prt=sum_irri/N )
#| I    19.4% 
#| II   17.5% 
#| III   8.1%
#| IV   14.1%
#| 

The present Project is designed only for Kharif and Rabi
Water Allocation and Release
Physical Area Ha 12300
                                               Kharif  Rabi
Area to be Irrigated as per agreement ( 95%) Ha 9,348   5,842 As Tender Condition Area to be Irrigated
Peak Water Requirement mm/day                     2.4    4
Approx. Water Allocation Mm³                    14.84    20.47
cubic meters/hectare                             1,588    3,504
14.84 cubic megameters (Mm³) is equivalent to 14.84 million cubic meters. 14,840,000 cubic meters.

approximately 1587.59 Kharif/ 3503.94 Rabi cubic meters of water would be used for irrigation per hectare.



library("chirps")
library("terra")

lonlat <- data.frame(lon =76.06, lat = 16.06 )
dates <- c("2017-05-01","2023-01-31")
r1 <- get_chirps(lonlat, dates, server = "CHC")

