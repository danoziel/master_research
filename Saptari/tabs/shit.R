library(tidyverse)
library(kableExtra)
library(formattable)
library(knitr)
library(MatchIt)
library(tableone)
library(gridExtra)



# match.it-----
lsay <- Agriculture_17_18_19 %>% filter(season_of_crop=="Summer") %>%
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(irrigate_hr=sum(irri_for_season)) %>%
  filter(!is.na(irrigate_hr))

names(lsay)[names(lsay) == 'TreatmentControl'] <- 'Group'

lso <-
  lsay %>%  mutate(own_sp = Group == "Treatment")

lso_M <- filter(lso, year == 2017)
match.it <- matchit(own_sp ~ irrigation_hr_per_ha , data = lso_M, method="nearest", ratio=1)
df.match <- match.data(match.it)[4]
df.match <-
  inner_join (df.match,lso,by="household_questionnaire_id") %>% 
  select(year,Group,household_questionnaire_id,irrigation_hr_per_ha )%>%
  filter(Group=="Control")
df.match$Group <- "Match"

df <- rbind(lsay,df.match) 

ggplot(df, aes(year, irrigation_hr_per_ha, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="income per ha") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",axis.text.x = element_text(face="bold",size=14),axis.text.y = element_text(face="bold",size=16))

rm(df.match,match.it,df)
rm(lso , lso_M)
rm( lsi )


#---------------------------------------

x <- Procurement_18_19 %>%
  filter(TC==1,year==2018,!is.na(total_litres_consumed_dieselkero)) %>% 
  select(household_questionnaire_id) 

x1 <- Procurement_18_19 %>%
  filter(TC==1,year==2019,!is.na(total_litres_consumed_dieselkero)) %>% 
  inner_join(x)%>% 
  select(household_questionnaire_id) %>% 
  left_join(Procurement_18_19) %>% 
  group_by(district,year) %>%
  summarise(N=n(), Mean=mean(total_litres_consumed_dieselkero,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))
# -----------------------------------------



