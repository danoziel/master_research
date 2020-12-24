# frequency HH grow type of crop----
# year
R_Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(no.HH=n(),no.HH/133)

# crop
x <- R_Agriculture_Baseline_2018_ %>%   
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop ,name_of_crop) %>% 
  summarise(no.HH=n(),no.HH/133)



#[Ram] farmers how do not cultivate anything in the summer- add a row for "cultivating any crop".
R_Agriculture_Baseline_2018_ %>% select(1,4,18) %>%
  group_by(household_questionnaire_id) %>%
  filter(season_of_crop==3) # Groups:   household_questionnaire_id [40]
# do not cultivate anything in the summer: 133-40= 93
# 93/133 = 0.6992481


# How much of the cultivated area was under this crop: mean & %----

#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 
as <- R_Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id) %>%
  summarise(sc=sum(cult_area_under_crop)) %>% summarise(mean(sc))

tcb <- R_Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,TC) %>%
  summarise(sc=sum(cult_area_under_crop)) %>%group_by(TC) %>%  summarise(mean(sc))

tca <- R_Agriculture_Endline_EPC_2019_ %>%  
  group_by(household_questionnaire_id,TC) %>%
  summarise(sc=sum(cult_area_under_crop)) %>%group_by(TC) %>%  summarise(mean(sc))

#season
R_Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop) %>% summarise(n=n(), mean = mean(sum_cult_area))
# %>% mutate(freq = prop.table(n))
# %>% per=sum(sum_cult_area)/sum(R_Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T))

tcb <- R_Agriculture_Baseline_2018_ %>%  
  group_by(TC,household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(TC,season_of_crop) %>% summarise(n=n(), mean = mean(sum_cult_area))

tca <- R_Agriculture_Endline_EPC_2019_ %>%  
  group_by(TC,household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(TC,season_of_crop) %>% summarise(n=n(), mean = mean(sum_cult_area))


# crop
x <- R_Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(mean = mean(sum_cult_area))

# area crop out of area seasom----
#season_of_crop / name_of_crop 

ses <- R_Agriculture_Baseline_2018_ %>%
  select(1,3,4,7) %>% 
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum1=sum(cult_area_under_crop)) %>% 
  group_by(season_of_crop,name_of_crop) %>% 
  summarise(n(),mean(sum1))

# sum of cult_area_under_crop in a season_of_crop
R_Agriculture_Baseline_2018_ %>%  
  group_by(season_of_crop) %>%
  summarise(sum(cult_area_under_crop))

#Monsoon
R_Agriculture_Baseline_2018_ %>% filter(season_of_crop=="1") %>% 
  group_by(name_of_crop)%>%summarise(pr=sum(cult_area_under_crop/7564))

#Winter
R_Agriculture_Baseline_2018_ %>%filter(season_of_crop=="2") %>% 
  group_by(name_of_crop)%>%summarise(pr=sum(cult_area_under_crop/7005))

#Summer
R_Agriculture_Baseline_2018_ %>%filter(season_of_crop=="3") %>% 
  group_by(name_of_crop)%>%summarise(p=sum(cult_area_under_crop/1061))

# Annual
R_Agriculture_Baseline_2018_ %>%filter(season_of_crop=="4") %>% 
  group_by(name_of_crop)%>%summarise(p=sum(cult_area_under_crop/526))

#time taken to irrigate the cultivated area- IN HOURS: n=HH ----

# HH level in a year
as<- R_Agriculture_Baseline_2018_ %>%
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(household_questionnaire_id) %>% 
  summarise(count = n(),mean_hr=mean(hrs_irr_1katha),sum_ir=sum(irri_for_season)) %>% 
  summarise(n.HH=n(),mean(sum_ir),mean(mean_hr),mints= mean(mean_hr)*60)

tcb<- R_Agriculture_Baseline_2018_ %>%
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(TC,household_questionnaire_id) %>% 
  summarise(count = n(),mean_hr=mean(hrs_irr_1katha),sum_ir=sum(irri_for_season)) %>% 
  summarise(n.HH=n(),mean(sum_ir),mean(mean_hr))

tca<- R_Agriculture_Endline_EPC_2019_ %>%
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(TC,household_questionnaire_id) %>% 
  summarise(count = n(),mean_hr=mean(hrs_irr_1katha),sum_ir=sum(irri_for_season)) %>% 
  summarise(n.HH=n(),mean(sum_ir),mean(mean_hr))


# season
aas <- R_Agriculture_Baseline_2018_ %>% 
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(household_questionnaire_id,season_of_crop) %>% 
  summarise(sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
  group_by(season_of_crop) %>%
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),mints=mean(mean_hr)*60,p_HH=n.HH/133)

tcb <- R_Agriculture_Baseline_2018_ %>% filter(TC==0) %>% 
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(household_questionnaire_id,season_of_crop) %>% 
  summarise(sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
  group_by(season_of_crop) %>%
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),p_HH=n.HH/107)
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),p_HH=n.HH/26)

tca <- R_Agriculture_Endline_EPC_2019_ %>% filter(TC==0) %>%   
    drop_na(irri_for_season,season_of_crop)%>% 
    group_by(household_questionnaire_id,season_of_crop) %>% 
    summarise(sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
    group_by(season_of_crop) %>%
    summarise( mean(sum_ir),mean(mean_hr),n.HH=n(),p_HH=n.HH/107)
      
#crop (year)
as <- R_Agriculture_Baseline_2018_ %>%
  filter(!is.na(irri_for_season))%>% 
  group_by(household_questionnaire_id,name_of_crop) %>% 
  summarise(n(),sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
  group_by(name_of_crop) %>%
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),mints=mean(mean_hr)*60 ,p_HH=n.HH/133)

#season/crop
as <- R_Agriculture_Baseline_2018_ %>%
  filter(!is.na(irri_for_season))%>%
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(n(),sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
  group_by(season_of_crop,name_of_crop) %>%
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),mints=mean(mean_hr)*60,p_HH=n.HH/133)

#--------------irrigation IN HOURS------TC---

#[5.6] most_imp_source--------

#Rain    Groundwater   River   Canal 
# 1          2           3       4    

aa <- R_Agriculture_Baseline_2018_ %>% 
  select(1,8,17) %>% 
  filter(most_imp_source>0) %>% 
  group_by(household_questionnaire_id,most_imp_source) %>% 
  summarise(n()) %>% 
  group_by(most_imp_source) %>% 
  summarise(n() ,p_HH=n()/133)



#when_is_crop_sowed/harvested----not fixed----
#when_is_crop_sowed
ses <- R_Agriculture_Baseline_2018_ %>% 
  select(when_is_crop_sowed, cult_area_under_crop) %>% 
  group_by(when_is_crop_sowed) %>% 
  summarise(sum=sum(cult_area_under_crop),
            per=sum(cult_area_under_crop)/
              sum(R_Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )

#when_is_crop_harvested
ses <- R_Agriculture_Baseline_2018_ %>% 
  select(cult_area_under_crop,when_is_crop_harvested) %>% 
  group_by(when_is_crop_harvested) %>% 
  summarise(sum=sum(cult_area_under_crop),
            per=sum(cult_area_under_crop)/
              sum(R_Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )









