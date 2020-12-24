library(tidyverse)
library(kableExtra)
library(MatchIt)
library(tableone)

Dietary_17_18_19 <-
  rbind(Dietary_diversity_Baseline_2017_,
        Dietary_diversity_Midline_2018_,
        Dietary_Diversity_Endline_2019_Saptari) %>%
  inner_join(Control_and_treatment_4_districts, by="household_questionnaire_id")

times_vegeaten <- Dietary_17_18_19 %>% 
  select(1,times_vegeaten_summer,times_vegeaten_monsoon,times_vegeaten_winter,year,TreatmentControl) %>% 
  mutate(times_vegeaten=times_vegeaten_monsoon+times_vegeaten_summer+times_vegeaten_winter)
  
x <-
  Dietary_17_18_19 %>% 
  mutate(times_vegeaten=times_vegeaten_monsoon+times_vegeaten_summer+times_vegeaten_winter) %>%
  group_by(TreatmentControl,year) %>% rename(Group=TreatmentControl) %>% 
  summarise(Mean=mean(times_vegeaten,na.rm = T))

x <- Dietary_17_18_19 %>% 
  group_by(TreatmentControl,year) %>% rename(Group=TreatmentControl) %>% 
  summarise(Mean=mean(times_vegeaten_summer,na.rm = T))

ggplot(x) + geom_line(aes(y = Mean, x = year, colour = Group), size=1.5,stat="identity")+
  theme_minimal() + ggtitle("summer")
