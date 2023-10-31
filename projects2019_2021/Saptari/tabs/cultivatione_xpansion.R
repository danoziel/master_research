library(tidyverse)
library(kableExtra)
library(extrafont)
library(sjPlot)


# outliers HH ----
total_land_baseline 
# "A104507035","E0104705010", "E104705010", "A110402001","A0110402001"  HH pilot
  
# A110402001 (monitoring)= A0110402001(survey)
# E0104705010 (monitoring) = E104705010 (survey)
# T309900000 T309800000 (monitoring)== T304802123  (survey)

# T303007001 T300901091 T300901113  no endline in survey
# T302603034 no endline in survey only aqua

#   T300406135  returned the pump no data in monitorung
#  "E0104705010" pump broke

#HH with few observations
# T210701004  (18) |  year 2018-2019 missung | Saptari 
# E0104705010 (22) | Summer_2017_2018	         | Sapteri
# A0110402001 (39) | 2017 missing              | Sapteri

# T302603034(11) T309708020(14) T300901113(12) | monsoon 18-19 | Paddy | Bara

## total_land_holding (survey) ----   
 DATABASE: land_I

#     Sapteri total_land_holding (survey) ----
total_land_Saptari_baseline <-
  lands_I_17_18_19 %>% filter(TC==1,year ==2017) %>%
  mutate(toal_land = land_for_cultivation +land_for_aquaculture_ponds +orchard_land) %>% 
  select(district,household_questionnaire_id,toal_land) %>% 
  rename(HH=household_questionnaire_id)%>%
  mutate(toal_land=toal_land*0.0339) 

#     RBS     total_land_holding (survey) ----
total_land_rbs_baseline <-
  lands_I_18_19 %>% filter(TC==1,year ==2018) %>%
  mutate(toal_land = land_for_cultivation +land_for_aquaculture_ponds +orchard_land) %>% 
  select(district,household_questionnaire_id,toal_land) %>% 
  rename(HH=household_questionnaire_id)%>% 
  mutate(district = "Rautahat_Bara_Sarlahi")%>%
  mutate(toal_land=toal_land*0.0339) 

## irrigated land  (survey) ----
DATABASE: lands_I  & Land_

#     rbs     irrigated land  (survey) ----
total_su_aqua_rbs <- 
  lands_I_18_19 %>%rename(HH=household_questionnaire_id)%>% filter(TC==1,year==2019,land_for_aquaculture_ponds>0) %>% select(HH,land_for_aquaculture_ponds) %>% mutate(land_for_aquaculture_ponds=land_for_aquaculture_ponds*0.0339)

total_s_irri_rbs <- 
  Land_18_19 %>% filter(TC==1,year==2019) %>% rename(HH=household_questionnaire_id)%>% 
  mutate(irrigated_land_survey=total_land_cultivated*0.0339) %>% select(HH,season,irrigated_land_survey) %>% spread("season","irrigated_land_survey") %>%
  full_join(total_su_aqua_rbs)
total_s_irri_rbs [is.na(total_s_irri_rbs)] <- 0 

total_su_irri_rbs <- 
  total_s_irri_rbs %>% 
  mutate(summer_2018_2019 = Summer+Annual+land_for_aquaculture_ponds,
         monsoon_2018_2019= Monsoon+Annual+land_for_aquaculture_ponds,
         winter_2018_2019= Winter+Annual+land_for_aquaculture_ponds,
         district = "Rautahat_Bara_Sarlahi",Source ="Irrigated land \nSurvey") %>%
  select(-c(Summer,Monsoon,Annual,Winter,land_for_aquaculture_ponds))


#     Saptari irrigated land  (survey) ----
total_su_aqua_saptari <- 
  lands_I_17_18_19 %>%rename(HH=household_questionnaire_id)%>% filter(TC==1,year %in% c(2018,2019),land_for_aquaculture_ponds>0) %>%
  select(HH,land_for_aquaculture_ponds,year) %>% mutate(land_for_aquaculture_ponds=land_for_aquaculture_ponds*0.0339) %>%
  spread("year","land_for_aquaculture_ponds") %>% rename(aqua2018=`2018`,aqua2019=`2019`)

total_s_irri_sapteri <- 
  land_17_18_19 %>% filter(TC==1,year %in% c(2018,2019)) %>% rename(HH=household_questionnaire_id)%>% 
  mutate(irrigated_land_survey=total_land_cultivated*0.0339) %>% select(HH,season,irrigated_land_survey,year) %>%
  mutate(Seasons=ifelse(season=="Summer" & year== 2018,"summer_2017_2018",
                        ifelse(season=="Monsoon" & year== 2018,"monsoon_2017_2018",
                               ifelse(season=="Winter" & year== 2018,"winter_2017_2018",
                                      ifelse(season=="Summer" & year== 2019,"summer_2018_2019",
                                             ifelse(season=="Monsoon" & year== 2019,"monsoon_2018_2019",
                                                    ifelse(season=="Winter" & year== 2019,"winter_2018_2019",
                                                           ifelse(season=="Annual" & year== 2018,"annual_2017_2018",
                                                                  ifelse(season=="Annual" & year== 2019,"annual_2018_2019",year))))))))) %>% 
  select(HH,Seasons,irrigated_land_survey) %>% 
  spread("Seasons","irrigated_land_survey") %>% 
  full_join(total_su_aqua_saptari)

total_s_irri_sapteri [is.na(total_s_irri_sapteri)] <- 0 
  
total_su_irri_sapteri <- total_s_irri_sapteri %>% 
  mutate(summer_2017_2018 = summer_2017_2018+aqua2018,
         monsoon_2017_2018= monsoon_2017_2018+aqua2018,
         winter_2017_2018= winter_2017_2018+aqua2018,
         
         summer_2018_2019 = summer_2018_2019+aqua2019,
         monsoon_2018_2019= monsoon_2018_2019+aqua2019,
         winter_2018_2019= winter_2018_2019+aqua2019,
         district="Saptari",Source ="Irrigated land \nSurvey") %>% 
  select(-c(annual_2017_2018,annual_2018_2019,aqua2018,aqua2019))

## irrigated land(monitoring) RBS+Sapteri    ----
total_mo_irri <- 
  water01_SEASONs %>%
  filter(HH != "T305602003" |Seasons != "Summer 2017-2018") %>%
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>% 
  select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017",
                         "Monsoon 2019-2020","Winter 2019-2020",
                         "Annual 2019-2020") )%>% distinct() %>%
  group_by(district,HH,Seasons) %>% 
  summarise(irrigated_land_monitoring =sum(`Total Area Cultivated`)*0.0339) %>% 
  spread("Seasons","irrigated_land_monitoring") %>%
  replace(is.na(.), 0) %>% 
  mutate(summer_2017_2018 =  `Summer 2017-2018`+`Annual 2017-2018`,
         monsoon_2017_2018= `Monsoon 2017-2018`+`Annual 2017-2018`,
         winter_2017_2018= `Winter 2017-2018`+`Annual 2017-2018`,
         
         summer_2018_2019 =  `Summer 2018-2019`+`Annual 2018-2019`,
         monsoon_2018_2019= `Monsoon 2018-2019`+`Annual 2018-2019`,
         winter_2018_2019= `Winter 2018-2019`+`Annual 2018-2019`,
         Source ="Irrigated land \nMonitoring") %>% 
  select(-c(3:10)) %>%
  mutate(
    summer_2017_2018=ifelse(summer_2017_2018==0 & district=="Rautahat_Bara_Sarlahi",NA,summer_2017_2018),
    winter_2017_2018=ifelse(winter_2017_2018==0 & district=="Rautahat_Bara_Sarlahi",NA,winter_2017_2018),
    monsoon_2017_2018=ifelse(monsoon_2017_2018==0 & district=="Rautahat_Bara_Sarlahi",NA,monsoon_2017_2018)) 






# cultivation_expansion----

total_irrigated <- 
  total_su_irri_rbs %>%
  mutate(summer_2017_2018 = NA,monsoon_2017_2018= NA,winter_2017_2018=NA) %>% 
  select(5:6,1,8,9,7,3,4,2) %>% 
  rbind(total_su_irri_sapteri,total_mo_irri) %>% 
  gather("x","y" ,4:9)

total_land_baseline <- 
  rbind(total_land_Saptari_baseline,total_land_rbs_baseline) %>% 
  mutate(Source ="Total land holding \n(Survey)",x="Total land holding") %>% rename(y=toal_land)       


cultivation_expansion <- 
  bind_rows(total_irrigated,total_land_baseline) %>% 
  mutate(y=ifelse(y==0,NA,y)) %>% 
  filter(!HH %in% c("A104507035","E0104705010", "E104705010","A110402001","A0110402001")) %>%  # HH pilot
  filter(!HH %in% c("T303007001", "T300901091","T210701004","T300901113")) %>%    ## missing a year
  filter(!HH %in% c("T300608033","T308707002"))#Removed due to conflicting pond size data -lands_I_18_19/aquaculture
view(cultivation_expansion)

cultivation_expansion <- 
  cultivation_expansion %>% 
  group_by(district,x,Source) %>% summarise(y=mean(y,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2)) 

cultivation_expansion <- 
  cultivation_expansion %>% 
  mutate(y=ifelse(district=="Rautahat_Bara_Sarlahi" & x=="monsoon_2018_2019" & Source=="Irrigated land \nSurvey",4.03,y)) %>% 
  mutate(y=ifelse(district=="Rautahat_Bara_Sarlahi" & x=="winter_2018_2019" & Source=="Irrigated land \nSurvey",3.95,y))
  

total_land_holding		

# PLOT 
cultivation_expansion$district[cultivation_expansion$district=="Rautahat_Bara_Sarlahi"] <- "Rautahat, Bara & Sarlahi"

# 

cultivation_expansion %>%
  ggplot(aes(x,y, fill=Source))+
  geom_col(position="dodge",width= 0.75) +
  geom_text(aes(label = y), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
  facet_grid(. ~ district) +

  theme_minimal() +
  labs(x = " ",y="Area size (in ha)")+
  theme(text = element_text(size = 25),
        axis.title = element_text(size = 17.5),
        axis.text = element_text(size = 10)) +
  theme(panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
  
  scale_x_discrete(limits = c("Total land holding", "monsoon_2017_2018", "winter_2017_2018","summer_2017_2018",
                              "monsoon_2018_2019", "winter_2018_2019","summer_2018_2019"))+
  scale_fill_manual(values=c("steelblue3", "steelblue1","steelblue4"))

scale_fill_manual(values=c("#468189", "#9DBEBB","cyan4"))

  
  





# 1200/750 ----
ggplot(data = cultivation_expansion,aes(x=x)) +
  geom_bar(aes(y=y,fill=Source),stat="identity",position="dodge",alpha=.7) +
  labs(x = " ",y="Area size (in ha)")+
  geom_text(aes( y = y,label = y), vjust= 1.6, color= "black", size = 4)+
  facet_grid(. ~ district) + 
  theme_minimal() +
  theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits = c("total own land", "monsoon_2017_2018", "winter_2017_2018",
                              "summer_2017_2018",
                              "monsoon_2018_2019", "winter_2018_2019",
                              "summer_2018_2019"))+
  scale_fill_manual(values=c("#468189", "#9DBEBB","cyan4"))
# TABLE ----
cultivation_expansion %>% rename(season=x) %>% 
  replace(is.na(.),0) %>% 
  group_by(district,Source) %>% 
  summarise(mean(y)) 








cultivation_expansion %>% 
  group_by(district,x,Source) %>% summarise(y=mean(y,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2)) 

--------
  cultivation_expansion %>% 
  group_by(district,Source) %>% 
  filter(x %in% c("summer_2018_2019","summer_2017_2018","Total land holding")) %>% 
  filter(!Source %in% c("Irrigated land \nMonitoring")) %>% 
  summarise(y=mean(y,na.rm = T))

2.88/4.57 
#[1] 0.6301969  rbs

0.705/2.74
#[1] 0.2572993  saptari

------

  cultivation_expansion %>% 
  group_by(Source) %>% 
  filter(x %in% c("winter_2018_2019","winter_2017_2018","Total land holding")) %>% 
  filter(!Source %in% c("Irrigated land \nMonitoring")) %>% 
  summarise(y=mean(y,na.rm = T))  

4.00/4.57
  
  
------  
  cultivation_expansion %>% 
  group_by(district,Source) %>% 
  filter(!Source %in% c("Irrigated land \nMonitoring")) %>% 
  summarise(y=mean(y,na.rm = T))
  
  
3.66/4.57
# [1] 0.8008753 rbs
  
1.41/2.74
# [1] 0.5145985 saptari

cultivation_expansion %>% group_by(Source) %>% filter(!Source %in% c("Irrigated land \nMonitoring")) %>% 
  summarise(y=mean(y,na.rm = T))

2.29/3.78
#[1] 0.6058201 all 4



farmers_ID2 <- farmers_ID %>%
  select(1,4) %>% arrange(household_questionnaire_id)

farmers_ID1 <- water01 %>% select(1,3) %>% distinct() %>% arrange(HH)
farmers_ID12 <- bind_cols(farmers_ID1,farmers_ID2)


#Irrigated land baseline ----

#  Baseline   rbs     irrigated land  (survey) ----
total_su_aqua_rbs18 <- 
  lands_I_18_19 %>%rename(HH=household_questionnaire_id)%>% filter(TC==1,year==2018,land_for_aquaculture_ponds>0) %>% select(HH,land_for_aquaculture_ponds) %>% mutate(land_for_aquaculture_ponds=land_for_aquaculture_ponds*0.0339)

total_s_irri_rbs18 <- 
  Land_18_19 %>% filter(TC==1,year==2018) %>% rename(HH=household_questionnaire_id)%>% 
  mutate(irrigated_land_survey=total_land_cultivated*0.0339) %>% select(HH,season,irrigated_land_survey) %>% spread("season","irrigated_land_survey") %>%
  full_join(total_su_aqua_rbs)
total_s_irri_rbs18 [is.na(total_s_irri_rbs18)] <- 0 

total_su_irri_rbs18 <- 
  total_s_irri_rbs18 %>% 
  mutate(summer_base = Summer+Annual+land_for_aquaculture_ponds,
         monsoon_base= Monsoon+Annual+land_for_aquaculture_ponds,
         winter_base= Winter+Annual+land_for_aquaculture_ponds,
         district = "Rautahat_Bara_Sarlahi",Source ="Irrigated land \nSurvey") %>%
  select(-c(Summer,Monsoon,Annual,Winter,land_for_aquaculture_ponds))


# Baseline    Saptari irrigated land  (survey) ----
total_su_aqua_saptari17 <- 
  lands_I_17_18_19 %>%rename(HH=household_questionnaire_id)%>% filter(TC==1,year==2017,land_for_aquaculture_ponds>0) %>%
  select(HH,land_for_aquaculture_ponds,year) %>% mutate(land_for_aquaculture_ponds=land_for_aquaculture_ponds*0.0339) %>%
  spread("year","land_for_aquaculture_ponds") %>% rename(aqua2017=`2017`)

total_s_irri_sapteri17 <- 
  land_17_18_19 %>% filter(TC==1,year ==2017) %>% rename(HH=household_questionnaire_id)%>% 
  mutate(irrigated_land_survey=total_land_cultivated*0.0339) %>% select(HH,season,irrigated_land_survey,year) %>%
  select(HH,season,irrigated_land_survey) %>% 
  spread("season","irrigated_land_survey") %>% 
  full_join(total_su_aqua_saptari17)

total_s_irri_sapteri17 [is.na(total_s_irri_sapteri17)] <- 0 

total_su_irri_sapteri17 <- total_s_irri_sapteri17 %>% 
  mutate(summer_base = Summer+aqua2017,
         monsoon_base= Monsoon+aqua2017,
         winter_base= Winter+aqua2017,
         
         district="Saptari",Source ="Irrigated land \nSurvey") %>% 
  select(-c(Annual,Summer,Monsoon,Winter,aqua2017))

# Baseline  total_irrigated -----
total_irrigated_baseline <- 
  total_su_irri_rbs18 %>%
  rbind(total_su_irri_sapteri17) %>% 
  gather("x","y" ,2:4)


total_irrigated_baseline %>% 
  summarise(mean(y,na.rm = T)) 
# NEW 24/10/2023 ----
cultivation_expansion <- read.csv("~/master_research/DATAs/data_master/cultivation_expansion.csv")
cultivation_expansion <- 
  cultivation_expansion %>%
  mutate(season = case_when(
    x %in% c("monsoon_2017_2018", "monsoon_2018_2019") ~ "monsoon",
    x %in% c("summer_2017_2018","summer_2018_2019") ~ "summer",
    x %in% c("winter_2017_2018","winter_2018_2019") ~ "winter",
    TRUE ~ x)) 

cultivation_expansion$HH[cultivation_expansion$HH=="E104705010"] ="E0104705010"
cultivation_expansion$HH[cultivation_expansion$HH=="A0104507035"] ="A104507035"
cultivation_expansion$HH[cultivation_expansion$HH=="A0110402001"] ="A110402001"

cultivation_expansion$HH[cultivation_expansion$HH=="T309800000"] ="T304802123"
cultivation_expansion$HH[cultivation_expansion$HH=="T309900000"] ="T304802123"

cultivation_expansion <- 
  cultivation_expansion %>%
  filter(!HH %in% c("A104507035","E0104705010", "E104705010","A110402001","A0110402001")) %>%  # HH pilot
  filter(!HH %in% c("T303007001", "T300901091","T210701004","T300901113")) %>%    ## missing a year
  filter(!HH %in% c("T300608033","T308707002"))#Removed due to conflicting pond size data -lands_I_18_19/aquaculture


days_use_hh$HH[days_use_hh$HH=="T309800000"] ="T304802123"
days_use_hh$HH[days_use_hh$HH=="T309900000"] ="T304802123"


# A110402001 (monitoring)= A0110402001(survey)
# E0104705010 (monitoring) = E104705010 (survey)
# T309900000 T309800000 (monitoring)== T304802123  (survey)



days_use_hh <- read.csv("~/master_research/DATAs/data_master/data_saptari/days_use_hh.csv")

mean(days_use_hh$percentage)
days_use_hh$use_high_low <- ifelse(days_use_hh$percentage>0.37,"high","low")
days_use_hh$use_high_low <- ifelse(days_use_hh$percentage>0.4,"high","low")
days_use_hh$y_d <-   days_use_hh$percentage*365
days_use_hh %>% summarise(mean(y_d))
days_use_hh %>% filter(y_d<41) %>% summarise(mean(y_d)) # btm 10%
days_use_hh %>% filter(y_d>283) %>% summarise(mean(y_d)) #top 10%
days_use_hh %>% group_by(use_high_low) %>% summarise(mean(y_d)) #top 10%

aa=
inner_join(cultivation_expansion,days_use_hh,relationship = "many-to-many") %>% 
  group_by(use_high_low,x,Source) %>% 
  summarise(M1=mean(y,na.rm = T)) %>%
  mutate(season = case_when(
    x %in% c("monsoon_2017_2018", "monsoon_2018_2019") ~ "monsoon",
    x %in% c("summer_2017_2018","summer_2018_2019") ~ "summer",
    x %in% c("winter_2017_2018","winter_2018_2019") ~ "winter",
    TRUE ~ x)) %>% 
  group_by(use_high_low,season,Source) %>% 
  summarise(M2=mean(M1)) 
  

fc=
  inner_join(cultivation_expansion,days_use_hh,relationship = "many-to-many") %>% 
  select(HH,use_high_low) %>% distinct()

crop=
  water01 %>%  select(HH,crop,season) %>% 
  filter(season %in% c(
    "Monsoon_2017_2018","Monsoon_2018_2019","Summer_2017_2018",
    "Summer_2018_2019","Winter_2017_2018","Winter_2018_2019")) %>% distinct()

crop$HH[crop$HH=="T309800000"] ="T304802123"
crop$HH[crop$HH=="T309900000"] ="T304802123"

fc1=
days_use_hh[1:49,c(1,5)] %>% inner_join(crop) %>%  
  filter(!crop %in% c("Barren", "Barren Land","System testing","System Testing")) %>% 
  filter(HH != "A104507035") %>% 
  filter(HH != "E0104705010") %>% 
  mutate(seasons = case_when(
    season %in% c("Monsoon_2017_2018", "Monsoon_2018_2019") ~ "monsoon",
    season %in% c("Summer_2017_2018","Summer_2018_2019") ~ "summer",
    TRUE ~ "winter"))
  
fc2%>% count(use_high_low,crops)

fc1 %>% 
  select(HH, use_high_low,crop) %>%  
  mutate(crops = case_when(
    crop %in% c("Summer Paddy", "Paddy") ~ "paddy ",
    crop %in% c("Beans", "Black Eyed Beans","Lentil",
                "Horse Gram","Long Yard Beans","Split Red Lentil") ~ "pulses",
    crop %in% c("Linseed","Oil", "Mustard", "Sesame Seeds") ~ "oilseeds",
    crop %in% c("Brinjal","Cauliflower","Chilli","Coriander","Sunflower","Sugarcane",
                "Cucumber","Tomato","Onion","Potato","Mango Plant") ~ "high-value veg/fruit",
    crop %in% c("Cabbage", "Ridge Gourd","Pumpkin","Sponge Gourd",
                "Garlic","Grass","Green Leafy Vegetables",
                "Lady's Finger", "Radish","Bitter Gourd","Bottle Gourd") ~ "low-value veg/fruit",
    TRUE ~ crop)) %>% select(HH,use_high_low,crops) %>% distinct() %>% 
  count(use_high_low,crops)
  
high         Fish Farming (Xlow) oilseeds
low         paddy(Xhi) veg/fruit pulses
Maize Wheat




# Irrigated land Monitoring-Irrigated land Survey- Total land holding(Survey) 
# monsoon_2017_2018  monsoon_2018_2019   summer_2017_2018   summer_2018_2019 Total land holding 
# winter_2017_2018   winter_2018_2019







