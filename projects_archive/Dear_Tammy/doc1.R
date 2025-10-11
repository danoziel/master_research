library(tidyverse)
library(kableExtra)
a <- 
  agricultural_survey_data %>% filter(new_skills %in% c(4,5)) %>%
  count(applied_skills) %>% mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))


aa <- 
  agricultural_survey_data_1_  %>% select(`share 1`:share2_5) %>%  drop_na() %>% 
  mutate(sh=ifelse(share2_3 == 1 & share2_4 == 1 ,1,0)) %>% 
  count(sh)%>% mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

water_theme <- 
  agricultural_survey_data %>% select(theme1_1,theme1_2,theme1_3,theme1_4,most_important1) %>%
  mutate(water=ifelse(theme1_1==1 | theme1_2==1,1,0))%>% 
  group_by(water) %>% 
  count(most_important1)%>% mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  filter(water==1) %>% kable() %>% kable_minimal()

water_MASHAV <- 
  agricultural_survey_data %>% select(`participation3 MASHAV`,most_important1) %>%
  filter(`participation3 MASHAV`==1) %>% 
  count(most_important1)%>% mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) 



agricultural_survey_data %>% select(theme1_6,most_important1) %>%
  filter(theme1_6==1) %>% 
  count(most_important1)%>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) %>%
  kable() %>% kable_minimal()

