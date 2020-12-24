library(tidyverse)
library(kableExtra)
library(formattable)
library(knitr)
library(MatchIt)
library(tableone)
library(gridExtra)

irrigate_hr_annual <- Agriculture_17_18_19%>% 
  group_by(TreatmentControl,year, household_questionnaire_id) %>% 
  summarise(irrigate_hr=sum(irri_for_season)) %>%
  filter(!is.na(irrigate_hr)) %>% 
  summarise(irrigate_hr=mean(irrigate_hr)) %>% 
  rename(Group=TreatmentControl)

ggplot(irrigate_hr_annual, aes(year, irrigate_hr, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="irrigate_hr") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=16))

# irrigation_hr_per_ha= irrigate_hr/irrigated_out_of_tot_land_cult
B <- Agriculture_17_18_19%>% 
  select(TreatmentControl,year,season_of_crop, household_questionnaire_id,irri_for_season) %>% 
  rename(season = season_of_crop) %>% 
  group_by(TreatmentControl,year,season, household_questionnaire_id) %>% 
  summarise(irrigate_hr=sum(irri_for_season)) %>%
  inner_join(land_17_18_19) %>% 
  mutate(irrigation_hr_per_ha= irrigate_hr/irrigated_out_of_tot_land_cult) %>% 
  
B$irrigation_hr_per_ha[B$irrigation_hr_per_ha==Inf] <- 0
B <- B %>% 
  filter(!is.na(irrigation_hr_per_ha)) %>% 
  summarise(n(),total_irrigation=mean (irrigate_hr,na.rm = T) ,irrigation_hr_per_ha=mean(irrigation_hr_per_ha,na.rm = T))  %>%  
  mutate(across(is.numeric, round))


x <- Agriculture_17_18_19%>% 
  group_by(TreatmentControl,year,season_of_crop, household_questionnaire_id) %>% 
  summarise(irrigate_hr=sum(irri_for_season)) %>%
  summarise(n(),total_irrigation=mean (irrigate_hr,na.rm = T))  %>%  
  mutate(across(is.numeric, round))






