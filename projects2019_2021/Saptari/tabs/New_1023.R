library(sjPlot)
library(dplyr)
# back to data 10/2023 


prc = usage_percent
hh_usage_percent[,c(1,4)]
#     update season  ----
# 10/2013  main season (July-October); winter (November-February); and summer (March-June).

diary_spip_terai=
  water01%>%
  mutate(date=as.Date (date)) %>% 
  mutate(season=case_when(
    date >= "2017-06-02" & date <= "2017-10-31" ~ "monsoon_2017_2018",
    date >= "2017-11-01" & date <= "2018-02-28" ~ "winter_2017_2018",
    date >= "2018-03-01" & date <= "2018-06-30" ~ "summer_2017_2018",
    
    date >= "2018-07-01" & date <= "2018-10-31" ~ "monsoon_2018_2019",
    date >= "2018-11-01" & date <= "2019-02-28" ~ "winter_2018_2019",
    date >= "2019-03-01" & date <= "2019-06-30" ~ "summer_2018_2019",
    
    date >= "2019-07-01" & date <= "2019-10-31" ~ "monsoon_2019_2020",
    date >= "2019-11-01" & date <= "2019-12-16" ~ "winter_2019_2020"))

diary_spip_terai=
  diary_spip_terai %>%
  mutate(SEASONs=case_when(
    season %in% c("monsoon_2017_2018","monsoon_2018_2019","monsoon_2019_2020" ) ~ "monsoon",
    season %in% c("summer_2017_2018","summer_2018_2019" ) ~ "summer",
    season %in% c("winter_2017_2018","winter_2018_2019","winter_2019_2020" ) ~ "winter"))


#     update HH      ----


diary_spip_terai
diary_spip_terai$crop[diary_spip_terai$crop=="Summer Paddy"] <- "Paddy"
diary_spip_terai$HH[diary_spip_terai$HH == "A0110402001"] <- "A110402001"   
diary_spip_terai$HH[diary_spip_terai$HH == "E0104705010"] <- "E104705010" 

# HH diary Vs. HH survey ----
# Search for non-overlapping HH

# The use of "Procurement" is only for the  survey HH list
Procurement <- read.csv("~/master_research/DATAs/data_master/data_saptari/Procurement.csv")
Procurement %>% select(household_questionnaire_id,district) %>% distinct( )

# HH diary list:  
water01 %>% select(HH,district,District) %>% distinct()

HH_srvey=Procurement %>% rename(HH=household_questionnaire_id) %>% select(HH) %>% distinct()%>% mutate(survey=1)
HH_diary=water01 %>% select(HH) %>% distinct()%>% mutate(diary=1)

HH_full=full_join(HH_srvey,HH_diary)
HH_full %>% filter(is.na(survey) | is.na(diary))

# HH cases               ----

# Survey data only,  no diary 
# T304802123   LAL BABU MAHATO  Rautahat

# diary data only, Survey not done
# T309900000      Kanti Devi       Bara
# T309800000      Nirmala Devi     Bara

# diary data👍  , Survey only endline
# T109902002 (10 obs) | winter_2019_2020 | Mamta Yadav | Saptari

# returned the pump | REMOVED from "water01" df
# T300406135 (7 obs)  # the data in the survey isnt SIP usage

# pump broke
# E0104705010 (22 obs) | Summer_2017_2018 | Sapteri

#HH with few unreliable observations REMOVED from "hh_usage_percent" df
# T210701004  (18 obs) | 2018,2019  missing | Saptari
# A0110402001 (39 obs) | 2017 missing       | Sapteri

# Low usage
# T302603034(11/264 obs) | monsoon 18-19 | Paddy | Bara
# T309708020(14/18 obs) | monsoon 18-19 | Paddy | Bara
# T300901113(12/283 obs) | monsoon 18-19 | Paddy | Bara





# DF hh_usage_percent    ----
HUP1 <- #Hh Usage Percent
  diary_spip_terai %>% mutate(date=as.Date(date)) %>%
  group_by(HH) %>% 
  mutate(start=min(date),end=max(date)) %>% 
  mutate(total_days=end-start,
         total_days = as.numeric(total_days)) %>%
  select(date,HH,total_days) %>% distinct() %>% 
  group_by(HH,date) %>% mutate(n=n()) %>% 
  group_by(HH,total_days) %>% summarise(irrigation_days=sum(n)) %>% 
  mutate(usage_percent = irrigation_days / total_days) 

HUP2 <- HUP1 %>% filter(!HH %in% c( "A110402001", "T210701004"))


mean(HUP2$usage_percent)  # 0.4
median (HUP2$usage_percent)  # 0.38

# "high_usage","low_usage"
HUP2$usage_high_low <- ifelse(HUP2$usage_percent>.38,"high_usage","low_usage")
HUP2$intensity_high2_low1 <- ifelse(HUP2$usage_high_low=="high_usage",2,1)
HUP2$intensity_high1_low0 <- ifelse(HUP2$usage_high_low=="high_usage",1,0)

# "high_usage", "Av.", "low_usage"
HUP2$usage_hi_av_lw=ifelse(HUP2$usage_percent<.10,"low_usage",
                           ifelse(HUP2$usage_percent>.77,"high_usage","Av."))
HUP2$scale_3_2_1 <- 
  ifelse(HUP2$usage_percent<.10,1,ifelse(HUP2$usage_percent>.77,3,2))

hh_usage_percent <- HUP2


# essential code ----
library(sjPlot)
tab_model(df,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c(" "))

# colors         ----

"Aquaculture";     "dodgerblue" 
"Cultivated Land"; "#a1d99b"
  
"Saptari";                "darkolivegreen4"
"Rautahat\nBara Sarlahi"; "lightsalmon4" 

"Total Area Cultivated" :"steelblue2"
"Area Irrigated" :       "steelblue"

"Winter" : "dodgerblue4"
"Monsoon" :"dimgrey"
"Summer" : "darkolivegreen4"



# total land holding 

total_land_Saptari_baseline # TAB cultivation_expansion
total_land_rbs_baseline # TAB cultivation_expansion



# LCC per hr                                                      ----
#total_days # percentage = percentage / total_days 
w1_102023 <- diary_spip_terai 
w1_102023 <-  water01

w2_102023 <- 
  w1_102023 %>% mutate(date=as.Date(date)) %>% 
  filter(!HH %in% c("A0110402001", "T210701004" ))

w3_102023 <- w2_102023 %>% 
  select(HH,date) %>% 
  group_by(HH) %>% 
  mutate(start=min(date),end=max(date)) %>% 
  mutate(total_days=end-start,total_days = as.numeric(total_days)) %>%
  select(date,HH,total_days) %>% distinct() %>% 
  group_by(HH,date) %>% mutate(n=n()) %>% 
  group_by(HH,total_days) %>% 
  summarise(irrigation_days=sum(n))

w_prc_102023 <- w3_102023 %>% 
  mutate(prc = irrigation_days / total_days,days_a_year=365*prc)
# mutate(days_a_year=(365/total_days) *irrigation_days))
w_prc_102023 %>% arrange(days_a_year )
w_prc_102023 %>% arrange(desc(days_a_year))


w_prc_102023 <- w_prc_102023 %>% 
  mutate(intensity=ifelse(days_a_year<50,"low",ifelse(days_a_year>283,"high","Av.") ))
w_prc_102023 %>% group_by(intensity) %>%  summarise(mean(days_a_year))

w2_102023 %>% select(HH,date,Hours,season) %>% 
  group_by(HH) %>% summarise(hours=mean(Hours)) %>% 
  inner_join(w_prc_102023) %>% group_by(intensity) %>% 
  summarise(hours=mean(hours))

w2_102023 %>% select(HH,date,Hours,season) %>% 
  group_by(HH) %>% summarise(hours=mean(Hours)) %>% 
  inner_join(w_prc_102023) %>% mutate(hours_a_year=hours*days_a_year) %>% 
  group_by(intensity ) %>% 
  summarise(hours_a_year=mean(hours_a_year))%>% 
  mutate(hours_a_year=3478((hours_a_year)*20)    )


w_hr_102023 <- w2_102023 %>% select(HH,date,Hours,season) %>% 
  group_by(HH) %>% summarise(hours=sum(Hours))

w =w2_102023[,c(1,31)] %>% distinct() %>% 
  inner_join(w_prc_102023) %>%inner_join(w_hr_102023) %>% 
  mutate((total_days/365)) %>% 
  mutate(hours/(total_days/365)) %>% 
  mutate((hours/(total_days/365))*20) %>% 
  mutate(3478 / ((hours/(total_days/365))*20) ) %>%  
  mutate( hr_cost_20y = 3478 / ((hours/(total_days/365))*20))  %>%  
  select(total_days,irrigation_days,HH,prc,hr_cost_20y ,intensity)%>% 
  mutate(hr.intensity=ifelse(hr_cost_20y>0.55,"low use",ifelse(hr_cost_20y<0.10,"high use","Av. use") ))

tibble <- 
  w %>% 
  group_by(hr.intensity) %>% 
  summarise(hr_cost=mean(hr_cost_20y))



# ________________________ Appendix G __________________________- ----
      # DF total_land_holding (survey)      ----
total_land_Saptari_baseline # TAB cultivatione_xpantion.R
total_land_rbs_baseline # TAB cultivatione_xpantion.R
# C:\Users\Dan\Documents\master_research\DATAs\data_master\data_saptari

land_holding= 
  rbind(total_land_Saptari_baseline,total_land_rbs_baseline) %>% 
  rename(total_land_holding=toal_land) %>% 
  inner_join(hh_usage_percent)

      # graph land_holding (survey)----
land_holding %>%
  ggplot(aes(x =total_land  , y =  usage_percent   ))+
  geom_point(color = "darkolivegreen3", size = 2)+
  geom_smooth(method = "lm", color = "salmon4") +  # Add the regression line
  labs(title= "", x="total_land_holding (in Ha)", y= "SIP usage (in %)")+
  theme_minimal() + theme(text = element_text(family = "serif"))






# mils Max Irri Land Season ## DF ##     ----
mils <-diary_spip_terai %>% 
  select(HH,season,SEASONs,crop,Total.Area.Cultivated) %>%
  distinct() %>%
  group_by(HH,season,SEASONs) %>%
  summarise(total_irrigated_land =sum(Total.Area.Cultivated)) %>% 
  group_by(HH,SEASONs) %>%
  summarise(irrigated_land =max(total_irrigated_land,na.rm = T)*0.0339)%>% 
  left_join(hh_usage_percent) %>%
  filter(!is.na(usage_percent ) )

      # DF mils_GENERAL ---- 
mils_GENERAL <- mils %>% group_by(HH) %>%
  summarise(irrigated_land_ha =max(irrigated_land,na.rm = T))
  
land_holding_mils=inner_join(land_holding,mils_GENERAL)

      # REG land_holding + mils_GENERAL   ----

model <- lm(usage_percent ~ irrigated_land_ha+total_land_holding, data = land_holding_mils)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent"))

model <- lm(intensity_high1_low0 ~ irrigated_land_ha+total_land_holding, data = land_holding_mils)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("intensity_high1_low0"))


      # graph mils_GENERAL----

land_holding_mils %>% 
  ggplot(aes(x =irrigated_land_ha , y =  usage_percent    ))+
  geom_point(color = "darkolivegreen3", size = 2)+
  geom_smooth(method = "lm", color = "salmon3") +
  labs(x="irrigated_land by SIP (in Ha)", y= "SIP usage (in %)",title = "HH largest plot")+
  theme_minimal() + theme(text = element_text(family = "serif"))

            # REG mils SESONS ----
# usage_high_low | usage_2_1 | usage_hi_av_lw | usage_3_2_1

mils_SEASONS <-mils %>% 
  tidyr::pivot_wider(
  names_from = SEASONs ,
  values_from = irrigated_land ) 

model1 <- lm(usage_percent ~ monsoon, data = mils_SEASONS)
model2 <- lm(usage_percent ~ winter, data = mils_SEASONS)
model3 <- lm(usage_percent ~ summer, data = mils_SEASONS)

summary(model)
tab_model(model3,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent"))

plot_summs(model1,model2,model3,scale = T,
           colors = c( "dimgrey", "dodgerblue4", "darkolivegreen4"),
           point.shape = F)+ theme(legend.position="none")




model <- lm(intensity_high1_low0 ~ monsoon+summer+winter, data = mils_SEASONS)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("intensity_high1_low0"))



            # graph mils SESONS----
mils %>% 
  ggplot(aes( y =  prc  ,x = irrigated_land   ,color= SEASONs   ))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE) +
  labs(x="irrigated_land by SIP (in Ha)", y= "SIP usage (in %)")+
  theme_light()+
  scale_color_manual(values=c("dimgrey", "darkolivegreen3","dodgerblue3"))

                   # REG  irrigation potential (gap) ----
#A104507035  IIImonsoon2.26 \Isummer2.90  \IIwinter2.31: I-II=GAP 2.90-2.31=0.59

milsGAP <- 
  mils %>% 
  # group_by(HH) %>%mutate(rank_irrigated = rank(desc(irrigated_land_monitoring)))
  mutate(mn=min(irrigated_land),mx=max(irrigated_land)) %>%
  mutate(gap=mx-mn) %>%
  select(HH,prc,gap) %>% distinct()
milsGAP$intensity_high1_low0 <- ifelse(milsGAP$prc>.38,1,0)



model <- lm(prc ~ gap, data = milsGAP)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent"))

model <- lm(intensity_high1_low0 ~ gap, data = milsGAP)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("intensity_high1_low0"))


                   # graph irrigation potential (gap) ----
ggplot(milsGAP, aes(x =gap, y =  prc   ))+
  geom_point(color = "#00AFBB", size = 2)+
  geom_smooth(method = "lm", color = "darkblue") +  # Add the regression line
  labs(x="Gap (in Ha)", y= "SIP usage (in %)",
       title="Cultivated area expansion potential",
       subtitle = "Potential per farmer=the interval between the maximum irrigated area (in Ha) \nand the minimum irrigated area between 2017 and 2020. Compared to the \npercentage of daily use during the entire SIP holding period.")+
  theme_minimal() + theme(text = element_text(family = "serif"))









# Demography #  DF characters ----

# age_sex
library(haven)
DM11<- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Demography_Baseline(2017).dta")
DM12 <- read_dta("~/Nepal Data/REWSSPC/Baseline/Demography_Baseline(2018).dta")
age_sexSAP=DM11 %>% filter(relation_with_headhh ==1) %>% select(household_questionnaire_id ,age,sex,district  )
age_sexRBS=DM12 %>% filter(relation_with_headhh ==1) %>% select(household_questionnaire_id ,age,sex,district  )
age_sex= rbind(age_sexSAP ,age_sexRBS) %>%
  rename(HH=household_questionnaire_id,age_hh_head=age, sex_hh_head=sex) %>% mutate(survey="baseline")
rm(DM11,DM12,age_sexSAP,age_sexRBS )

# DM2 caste livestock
DM21 <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Demography_2_Baseline(2017).dta")
DM23 <- read_dta("~/Nepal Data/REWSSPC/Baseline/Demography_2_Baseline(2018).dta")
DM2=rbind(DM21,DM23)%>%
  rename(HH=household_questionnaire_id) %>% mutate(survey="baseline",caste=NA) 
rm(DM21,DM23)

DM2$caste[DM2$which_is_the_caste_of_you_househ ==1] <- 3
DM2$caste[DM2$which_is_the_caste_of_you_househ ==2] <- 1
DM2$caste[DM2$which_is_the_caste_of_you_househ %in% c(5,6,8,20,21 ) ] <- 2
DM2$caste[DM2$which_is_the_caste_of_you_househ ==28] <- 1

# code         name            grade              rate (for reg)
# 1       Brahmin/Chhetri    higt_caste           1
# 2                 Tharu    scheduled_tribe      3
# 5                Yadavs    middle_caste         2
# 6        Kori/Khushwaha    middle_caste         2
# 8                  Teli    middle_caste         2
# 20         Hajam/Thakur    middle_caste         2
# 21              Haluwai    middle_caste         2
# 28 T305001120   KALAWAR    low_caste            3
DM2=DM2 %>% 
  select(HH,caste,# bullock,cow,calf,buffalo,goat,pigeon,chicken,duck,pig,sheep,
         district,survey)

# rec education_of_hh_head 
recSAP <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Household questionnaires_rec_Baseline(2017).dta")
recRBS <- read_dta("~/Nepal Data/REWSSPC/Baseline/Household questionnaire_rec_Baseline(2018).dta")
rec=rbind(recSAP,recRBS)%>%  
  select(household_questionnaire_id,last_education_of_hh_head )%>%
  rename(HH=household_questionnaire_id, hh_head_edu=last_education_of_hh_head) %>% mutate(survey="baseline")
rm(recSAP,recRBS)


# age_sex # DM2:caste,livestock # rec:education_of_hh_head
             # REG characters ----
demography <- 
  left_join(hh_usage_percent,age_sex) %>% 
  left_join(DM2) %>% 
  left_join(rec)
demography$intensity_high1_low0
rm(age_sex,DM2,rec)
model1 <- lm(usage_percent ~ age_hh_head, data = demography)
model2 <- lm(usage_percent ~ hh_head_edu, data = demography)
model3 <- lm(usage_percent ~ caste, data = demography)

model1 <- lm(intensity_high1_low0  ~ age_hh_head, data = demography)
model2 <- lm(intensity_high1_low0  ~ hh_head_edu, data = demography)
model3 <- lm(intensity_high1_low0  ~ caste, data = demography)

summary(model1)

library(jtools)
summ(model1)

tab_model(model1,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          pred.labels = c("Constant", "Household head's age"),dv.labels = c("Usage percent"))

tab_model(model2,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          pred.labels = c("Constant", "Household head's education level"),dv.labels = c("Usage percent"))

tab_model(model3,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          pred.labels = c("Constant", "Household caste"),dv.labels = c("Usage percent"))




# CROP | SURVEY # DF cropping pattern ----

# agri DF build in TAB Agriculture_arrange.R 
agri_crops= agri %>% 
  select(HH,usage_percent ,name_of_crop,season_of_crop,crop,year,district,survey)

 # Fish Farming

aquaculture <- read_dta("~/master_research/DATAs/data_master/data_saptari/aquaculture.dta")
aquaculture %>% filter(TC==1) %>% group_by(district,year) %>% count()

aqua=
  aquaculture%>% filter(TC==1) %>% 
  select(year,household_questionnaire_id,district) %>% 
  mutate(survey=ifelse(year==2017 & district=="Saptari","baseline","endline" )) %>% 
  mutate(survey=ifelse(year==2018 & district %in% c(1,2,3),"baseline",survey )) %>% 
  mutate(year=ifelse(year==2018 & district=="Saptari",NA,year )) %>% 
  filter(!is.na(year)) %>% 
  rename(HH=household_questionnaire_id) %>% 
  mutate(crop="Fish_Farming",name_of_crop="Fish_Farming",season_of_crop="Annual") 
aqua$district[aqua$district== "SAPTARI" ] <- "Saptari"

aqua_crop=aqua %>%
  inner_join (hh_prc) %>% 
  select(HH,percentage,name_of_crop,season_of_crop,crop,year,district,survey)

       # REG cropping pattern per crop # crop_be1----
crop_be1 <- read.csv("~/master_research/DATAs/data_master/data_saptari/crop_be1.csv")
crop_be1= rbind(agri_crops,aqua_crop) %>% 
  select(HH,percentage,crop,survey) %>% 
  distinct() %>% 
  mutate(unit=1)

crop_be=crop_be1 %>% pivot_wider(names_from =crop , values_from= unit )
crop_be[is.na(crop_be)] <- 0
crop_be$intensity_high1_low0 <- ifelse(crop_be$percentage>.38,1,0)



crop_base= crop_be %>% filter(survey=="baseline")
crop_end= crop_be %>% filter(survey=="endline")
names(crop_be)

# reg crop_base ----

# v v v
model <- lm(percentage ~ Paddy+Wheat+Vegetables+Oilseeds+Pulses+Maize+Fish_Farming,data = crop_base)
model <- lm(percentage ~ Vegetables+Oilseeds+Fish_Farming,data = crop_base)
model <- lm(percentage ~ Paddy+Wheat+Pulses+Maize,data = crop_base)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent | baseline"))

# v x x
model <- lm(intensity_high1_low0 ~ Paddy+Wheat+Vegetables+Oilseeds+Pulses+Maize+Fish_Farming,data = crop_base)
model <- lm(intensity_high1_low0 ~ Vegetables+Oilseeds+Fish_Farming,data = crop_base)
model <- lm(intensity_high1_low0 ~ Paddy+Wheat+Pulses+Maize,data = crop_base)

tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("intensity_high1_low0| baseline"))


# reg crop_end ----

# v v v
model <- lm(percentage ~ Paddy+Wheat+Vegetables+Oilseeds+Pulses+Maize+Fish_Farming,data = crop_end)
model <- lm(percentage ~ Vegetables+Oilseeds+Fish_Farming,data = crop_end)
model <- lm(percentage ~ Paddy+Wheat+Pulses+Maize,data = crop_end)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent | endline"))

model <- lm(intensity_high1_low0 ~ Paddy+Wheat+Vegetables+Oilseeds+Pulses+Maize+Fish_Farming,data = crop_end)
model <- lm(intensity_high1_low0 ~ Vegetables+Oilseeds+Fish_Farming,data = crop_end)
model <- lm(intensity_high1_low0 ~ Paddy+Wheat+Pulses+Maize,data = crop_end)

tab_model(model,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("intensity_high1_low0"))



       # REG cropping pattern  crop_be_cat ----
crop_be2=crop_be1
crop_be2= rbind(agri_crops,aqua_crop) %>% mutate(cat4_crop=NA,cat2_crop=NA)
crop_be2$cat4_crop[crop_be2$crop %in% c("Paddy","Wheat", "Maize","Others" )] <- "traditional_cereal"
crop_be2$cat4_crop[crop_be2$crop %in% c( "Pulses" )] <- "traditional_pulses"
crop_be2$cat4_crop[crop_be2$crop %in% c("Sugarcane", "Fish_Farming" )] <- "water_crop_annual"
crop_be2$cat4_crop[crop_be2$crop %in% c("Vegetables","Oilseeds" )] <- "water_crop_seasonal"

crop_be2$cat2_crop[crop_be2$crop %in% c("Paddy","Wheat", "Maize","Pulses","Others" )] <- "traditional"
crop_be2$cat2_crop[crop_be2$crop %in% c("Vegetables","Sugarcane", "Fish_Farming","Oilseeds" )] <- "water_required"

cat_2_crop= crop_be2 %>% 
  select(HH,percentage,cat2_crop,survey) %>% 
  distinct() %>% 
  mutate(unit=1)

cat_4_crop= crop_be2 %>% 
  select(HH,percentage,cat4_crop,survey) %>% 
  distinct() %>% 
  mutate(unit=1)

cat_2_crop=cat_2_crop %>% pivot_wider(names_from =cat2_crop , values_from= unit )
cat_2_crop[is.na(cat_2_crop)] <- 0
cat_2_crop$intensity_high1_low0 <- ifelse(cat_2_crop$percentage>.38,1,0)

cat_4_crop=cat_4_crop %>% pivot_wider(names_from =cat4_crop , values_from= unit )
cat_4_crop[is.na(cat_4_crop)] <- 0
cat_4_crop$intensity_high1_low0 <- ifelse(cat_4_crop$percentage>.38,1,0)


cat_2_crop_base= cat_2_crop %>% filter(survey=="baseline")
cat_2_crop_end= cat_2_crop %>% filter(survey=="endline")

cat_4_crop_base= cat_4_crop %>% filter(survey=="baseline")
cat_4_crop_end= cat_4_crop %>% filter(survey=="endline")

# v v
model <- lm(percentage ~ traditional+water_required,data = cat_2_crop_base)

model <- lm(percentage ~ traditional_cereal+water_crop_seasonal+
              traditional_pulses+water_crop_annual,data = cat_4_crop_base)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | baseline"))

# v v
model <- lm(percentage ~ traditional+water_required,data = cat_2_crop_end)

model <- lm(percentage ~ traditional_cereal+water_crop_seasonal+
              traditional_pulses+water_crop_annual,data = cat_4_crop_end)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | endline"))

# CROP | DIARY # DF cropping pattern ----

names(diary_spip_terai)
table(diary_spip_terai$crop)

     diary_spip_terai$crop_cat =diary_spip_terai$crops_category
diary_spip_terai$crop_cat [diary_spip_terai$crop %in% c("Paddy", "Summer Paddy","paddy")] <- "Paddy"
diary_spip_terai$crop_cat [diary_spip_terai$crop =="Wheat" ] <- "Wheat"
diary_spip_terai$crop_cat [diary_spip_terai$crop =="Maize" ] <- "Maize"
  
     diary_spip_terai$cat2_crop =diary_spip_terai$crop_cat
     diary_spip_terai$crop_cat[diary_spip_terai$crop_cat=="Vegetables_Fruits"] <- "Vegetables"
diary_spip_terai$cat2_crop[diary_spip_terai$crop_cat %in% c("Paddy","Wheat", "Maize","Pulses" )] <- "traditional"
diary_spip_terai$cat2_crop[diary_spip_terai$crop_cat %in% c("Vegetables","Sugarcane", "Fish_Farming","Oilseeds" )] <- "water_required"

     diary_spip_terai$cat4_crop =diary_spip_terai$crop_cat
diary_spip_terai$cat4_crop[diary_spip_terai$crop_cat %in% c("Paddy","Wheat", "Maize" )] <- "traditional_cereal"
diary_spip_terai$cat4_crop[diary_spip_terai$crop_cat %in% c( "Pulses" )] <- "traditional_pulses"
diary_spip_terai$cat4_crop[diary_spip_terai$crop_cat %in% c("Sugarcane", "Fish_Farming" )] <- "water_crop_annual"
diary_spip_terai$cat4_crop[diary_spip_terai$crop_cat %in% c("Vegetables","Oilseeds" )] <- "water_crop_seasonal"

#  crops_diary----
crops_diary <- 
  diary_spip_terai %>% select(HH,crop_cat) %>% 
  filter(!is.na(crop_cat)) %>% distinct() %>% mutate(unit=1) %>% 
  pivot_wider(names_from = crop_cat,values_from =unit ) %>% 
  right_join(hh_usage_percent)
crops_diary[is.na(crops_diary)] <- 0

# reg crops_diary----

model <- lm(usage_percent ~ Paddy+Wheat+Vegetables+Oilseeds+Pulses+Maize+Fish_Farming,data = crops_diary)
model <- lm(usage_percent ~ Vegetables+Oilseeds+Fish_Farming,data = crops_diary)
model <- lm(usage_percent ~ Paddy+Wheat+Pulses+Maize,data = crops_diary)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | diary"))


crops_2_diary <- 
  diary_spip_terai %>% select(HH,cat2_crop) %>% 
  filter(!is.na(cat2_crop)) %>% distinct() %>% mutate(unit=1) %>% 
  pivot_wider(names_from = cat2_crop,values_from =unit )%>% 
  right_join(hh_usage_percent)
crops_2_diary[is.na(crops_2_diary)] <- 0

model <- lm(usage_percent ~ traditional+water_required,data = crops_2_diary)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | diary"))


crops_4_diary <- 
  diary_spip_terai %>% select(HH,cat4_crop) %>% 
  filter(!is.na(cat4_crop)) %>% distinct() %>% mutate(unit=1) %>% 
  pivot_wider(names_from = cat4_crop,values_from =unit )%>% 
  right_join(hh_usage_percent)
crops_4_diary[is.na(crops_4_diary)] <- 0

model <- lm(usage_percent ~ traditional_cereal+water_crop_seasonal+
              traditional_pulses+water_crop_annual,data = crops_4_diary)
summary(model)
tab_model(model,digits=3,p.style="numeric",show.se = TRUE,
          string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | diary"))

# graph----


fit1 <- lm(usage_percent ~ Paddy ,crops_diary)
fit2 <- lm(percentage ~    Paddy ,crop_base)
fit3 <- lm(percentage ~    Paddy ,crop_end)

fit11 <- lm(usage_percent ~ Wheat ,crops_diary)
fit22 <- lm(percentage ~    Wheat ,crop_base)
fit33 <- lm(percentage ~    Wheat ,crop_end)

fit111 <- lm(usage_percent ~ Vegetables ,crops_diary)
fit222 <- lm(percentage ~    Vegetables ,crop_base)
fit333 <- lm(percentage ~    Vegetables ,crop_end)

fit101 <- lm(usage_percent ~ Oilseeds ,crops_diary)
fit202 <- lm(percentage ~    Oilseeds ,crop_base)
fit303 <- lm(percentage ~    Oilseeds ,crop_end)

fit01 <- lm(usage_percent ~ Pulses ,crops_diary)
fit02 <- lm(percentage ~    Pulses ,crop_base)
fit03 <- lm(percentage ~    Pulses ,crop_end)

fit011 <- lm(usage_percent ~ Maize ,crops_diary)
fit022 <- lm(percentage ~    Maize ,crop_base)
fit033 <- lm(percentage ~    Maize ,crop_end)

fit0111 <- lm(usage_percent ~ Fish_Farming,crops_diary)
fit0222 <- lm(percentage ~    Fish_Farming,crop_base)
fit0333 <- lm(percentage ~    Fish_Farming,crop_end)


tab_model(fit11,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent | diary"))

tab_model(fit22,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent | baseline"))

tab_model(fit33,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("usage_percent | endline"))

library("jtools")
plot_summs(fit1,fit2,fit3,
           fit11,fit22,fit33,
           fit111,fit222,fit333,
           fit101,fit202,fit303,
           fit01,fit02,fit03,
           fit011,fit022,fit033,
           fit0111,fit0222,fit0333,
           scale = T,
           colors = c( "orange3","deepskyblue3", "deepskyblue4", "orange3","deepskyblue3", "deepskyblue4",
                       "orange3","deepskyblue3", "deepskyblue4", "orange3","deepskyblue3", "deepskyblue4",
                       "orange3","deepskyblue3", "deepskyblue4", "orange3","deepskyblue3", "deepskyblue4",
                       "orange3","deepskyblue3", "deepskyblue4"),
           point.shape = F)+ theme(legend.position="none")

# plot for legend
plot_summs(fit1,fit2,fit3,scale = T,colors = c( "orange3","deepskyblue3", "deepskyblue4"),model.names = c("diary", "Baseline (survey)", "Endline (survey)"),point.shape = F)+ coord_cartesian(xlim = c(-.25, .25))


# CROP irrigated land | diary  # DF # ----


# crops_land_size_Seasons irrigated by SIP
crops_land_size_Seasons <- 
  diary_spip_terai %>% 
  select(HH,Seasons,crop_cat,crop,Total.Area.Irrigated)%>%
  #filter(HH %in% c("A104507035","T110505001"),Seasons =="Winter 2017-2018")%>% 
  filter(!is.na(Total.Area.Irrigated),!is.na(Seasons),Total.Area.Irrigated>0) %>%
  group_by(HH,Seasons,crop) %>% 
  mutate(TotalAreaIrrigated=sum(Total.Area.Irrigated*0.0339)) %>% 
  select(-Total.Area.Irrigated) %>% 
  distinct() %>% 
  group_by(HH,Seasons,crop_cat) %>% 
  summarise(TotalAreaIrri=sum(TotalAreaIrrigated))


# crop irrigated area = usage_high_low
crops_land_size_Seasons %>%   
  group_by(HH,crop_cat) %>% summarise(AreaIrri=max(TotalAreaIrri)) %>% 
  inner_join(hh_usage_percent[,c(1,5)]) %>% 
  group_by(usage_high_low,crop_cat) %>% summarise(AreaIrri =mean(AreaIrri ),n=n()) %>% 
  mutate(prt= ifelse(usage_high_low=="high_usage", n/23,n/25 )) %>% 
  filter(!crop_cat %in% c("Sugarcane","Oilseeds", "Maize" ))


# WITH 0s
# crop irrigated area = usage_high_low <- 
crops_land_size_Seasons %>%   
  group_by(HH,crop_cat) %>% summarise(AreaIrri=max(TotalAreaIrri)) %>% 
  pivot_wider(names_from = crop_cat,values_from = AreaIrri) %>%   
  pivot_longer(!HH, names_to = "crop_cat", values_to = "AreaIrri") %>% 
  mutate(AreaIrri= ifelse(is.na(AreaIrri),0,AreaIrri)) %>% 
  filter(!crop_cat %in% c("Sugarcane","Oilseeds", "Maize" )) %>% 
  inner_join(hh_usage_percent[,c(1,5)]) %>% 
  group_by(usage_high_low,crop_cat) %>% summarise(AreaIrri =mean(AreaIrri ))



# crops_land_size_per_hh irrigated by SIP
crops_land_size_per_hh <- 
  crops_land_size_Seasons %>%   
  group_by(HH,crop_cat) %>% summarise(AreaIrri=max(TotalAreaIrri)) %>% 
  pivot_wider(names_from = crop_cat,values_from = AreaIrri) %>%   
  inner_join(hh_usage_percent[,c(1,4)])


fit1 <- lm(usage_percent ~ Paddy ,crops_land_size_per_hh)
fit7 <- lm(usage_percent ~ Fish_Farming,crops_land_size_per_hh)

library("jtools")
plot_summs(fit1,fit2,fit3,fit4,fit5,fit6,fit7,
           scale = T,colors = c( "orange3", "orange3","orange3", "orange3","orange3", "orange3","orange3"),
           point.shape = F)+ theme(legend.position="none")

tab_model(fit1,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | diary"))
tab_model(fit7,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",dv.labels = c("usage_percent | diary"))










# land holding + fish ponds====
Agriculture_17_18_19 <- read.csv("~/master_research/DATAs/data_master/data_saptari/Agriculture_17_18_19.csv")
Agriculture_18_19 <- read.csv("~/master_research/DATAs/data_master/data_saptari/Agriculture_18_19.csv")

A1= Agriculture_17_18_19 %>% select(season_of_crop,household_questionnaire_id,cult_area_under_crop,year,TreatmentControl) %>% filter(year==2017)
A2= Agriculture_18_19  %>% select(season_of_crop,household_questionnaire_id,cult_area_under_crop,year,TreatmentControl)%>% filter(year==2018)

aa=rbind(A1,A2) %>% filter(TreatmentControl=="Treatment") %>% 
  group_by(season_of_crop,household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(season_of_crop) %>% 
  summarise(cult_area=mean(cult_area))
  
aquaculture <- read_dta("~/master_research/DATAs/data_master/data_saptari/aquaculture.dta")

Aq1= aquaculture %>% filter(year==2017,TreatmentControl=="Treatment",district=="Saptari") %>% select(2,5)
Aq2= aquaculture %>% filter(year==2018,TreatmentControl=="Treatment",district %in% c("1","2","3")) %>% select(2,5)

rbind(Aq1,Aq2) %>% 
  filter(total_area_of_pond<59) %>% 
  mutate(pond=total_area_of_pond*0.0339) %>% 
  summarise(mean(pond))




total_area_of_pond


# ____________________  NEXT ________________________- ----
# Hours, Total.Area.Cultivated,Area.Irrigated,-----
diary <- 
  diary_spip_terai %>% 
  select(date,HH,Hours,
         Total.Area.Cultivated,Area.Irrigated,
         crop,crop_cat,cat4_crop,cat2_crop)
diary$crop_cat[diary$crop_cat=="NA"] <- NA

diary %>%group_by(crop_cat) %>%  
  summarise(hr=mean(Hours,na.rm = T),
            irri=mean(Area.Irrigated,na.rm = T)*0.0339,
            tot=max(Total.Area.Cultivated,na.rm = T)*0.0339) %>%arrange(irri)

diary %>%group_by(date,crop_cat) %>%  
  filter(!is.na(crop_cat)) %>% 
  summarise(hr=mean(Hours,na.rm = T),
            irri=mean(Area.Irrigated,na.rm = T)*0.0339,
            tot=max(Total.Area.Cultivated,na.rm = T)*0.0339)



# TABLE TO PAPER -----
# crop irrigated area = usage_high_low
crops_land_size_Seasons %>%   
  group_by(HH,crop_cat) %>% summarise(AreaIrri=max(TotalAreaIrri)) %>% 
  inner_join(hh_usage_percent[,c(1,5)]) %>% 
  group_by(usage_high_low,crop_cat) %>% summarise(AreaIrri =mean(AreaIrri ),n=n()) %>% 
  mutate(prt= ifelse(usage_high_low=="high_usage", n/23,n/25 )) %>% 
  filter(!crop_cat %in% c("Sugarcane","Oilseeds", "Maize" ))

# WITH 0s
# crop irrigated area = usage_high_low <- 
crops_land_size_Seasons %>%   
  group_by(HH,crop_cat) %>% summarise(AreaIrri=max(TotalAreaIrri)) %>% 
  pivot_wider(names_from = crop_cat,values_from = AreaIrri) %>%   
  pivot_longer(!HH, names_to = "crop_cat", values_to = "AreaIrri") %>% 
  mutate(AreaIrri= ifelse(is.na(AreaIrri),0,AreaIrri)) %>% 
  filter(!crop_cat %in% c("Sugarcane","Oilseeds", "Maize" )) %>% 
  inner_join(hh_usage_percent[,c(1,5)]) %>% 
  group_by(usage_high_low,crop_cat) %>% summarise(AreaIrri =mean(AreaIrri ))

### SURVEYS CP croping pattern###

####### Field crop # agri_crops: 45 HH

# agri DF build in TAB Agriculture_arrange.R 
surveyCP_field= 
  agri %>%
  select(HH,cult_area_under_crop ,season_of_crop ,name_of_crop,crop,survey) %>% 
  group_by(survey,HH,season_of_crop,crop) %>% summarise(crop_area=sum(cult_area_under_crop) ) %>% 
  group_by(survey,HH,crop) %>% summarise(crop_area=max(crop_area) )
  
####### Fish Farming # aqua: 22 HH

aquaculture <- read_dta("~/master_research/DATAs/data_master/data_saptari/aquaculture.dta")

Aq1= aquaculture %>% filter(year==2017,TreatmentControl=="Treatment",district=="Saptari") %>% select(2,5) %>% mutate(survey="baseline" )
Aq2= aquaculture %>% filter(year==2018,TreatmentControl=="Treatment",district %in% c("1","2","3")) %>% select(2,5)%>% mutate(survey="baseline" )

Aq1end= aquaculture %>% filter(year==2019,TreatmentControl=="Treatment",district=="SAPTARI") %>% select(2,5)%>% mutate(survey="endline" )
Aq2end= aquaculture %>% filter(year==2019,TreatmentControl=="Treatment",district %in% c("1","2","3")) %>% select(2,5)%>% mutate(survey="endline" )

surveyCP_fish <-
  rbind(Aq1,Aq2,Aq1end,Aq2end) %>% 
  mutate(crop_area= ifelse(total_area_of_pond<121,total_area_of_pond,-888)) %>% 
  rename(HH=household_questionnaire_id ) %>% select(-total_area_of_pond) %>% 
  mutate(crop="Fish_Farming") %>% select(survey,HH,crop,crop_area )


######### total_sample_baseline <- 
Aq=rbind(Aq1,Aq2)%>%  
  mutate(pond_area=total_area_of_pond *0.0339) %>% 
  select(household_questionnaire_id ,pond_area) %>% 
  rename(HH=household_questionnaire_id)

agri %>%   filter(survey=="baseline") %>% 
  select(HH,cult_area_under_crop ,season_of_crop ,name_of_crop,crop) %>% 
  mutate(crop_area=cult_area_under_crop*0.0339) %>% 
  group_by(HH,season_of_crop) %>% summarise(crop_area=sum(crop_area) ) %>% 
  left_join(Aq) %>% 
  mutate(pond_area=ifelse(is.na(pond_area),0,pond_area  ) ) %>% 
  group_by(season_of_crop) %>% summarise(crop_area=mean(crop_area) ) %>% 
  
#  mutate(ha_area=crop_area+pond_area) %>% 

  group_by(season_of_crop) %>% summarise(ha_area=mean(ha_area) ) %>% 
  mutate(total_crop_area=ifelse(season_of_crop !="Annual",ha_area+0.827,NA ))


  
# diary
crops_land_size_Seasons %>%   
   filter(!Seasons %in% c("Summer 2016-2017","Annual 2019-2020","Monsoon 2019-2020","Summer 2019-2020","Winter 2019-2020"  ) ) %>% 
   mutate(Season= ifelse(Seasons %in% c("Monsoon 2017-2018","Monsoon 2018-2019"),"Monsoon",
                         ifelse(Seasons %in% c("Summer 2017-2018","Summer 2018-2019"),"Summer",
                                ifelse(Seasons %in% c("Winter 2017-2018","Winter 2018-2019"),"Winter" ,"Annual" ) ))) %>% 
   group_by(HH,Season) %>% summarise(AreaIrri=sum(TotalAreaIrri)) %>% 
#  inner_join(hh_usage_percent[,c(1,5)]) %>% 
#  group_by(usage_high_low,Seasons) %>%
   group_by(Season) %>%
  summarise(AreaIrri =mean(AreaIrri ),n=n()) 


stat=
  rbind(surveyCP_field,surveyCP_fish)%>% 
  mutate(crop_area=ifelse(crop_area>0,crop_area*0.0339,crop_area)) %>% 
  pivot_wider(names_from = crop,values_from = crop_area) %>%   # A tibble: 98 × 7
  pivot_longer(-c(survey,HH), names_to = "crop_cat", values_to = "area_irri") %>%    # A tibble: 490 × 4
  mutate(area_irri=ifelse(is.na(area_irri),0, ifelse(area_irri==-888,NA,area_irri ) )) %>%
  filter(survey=="baseline")
  
st=stat %>% filter(area_irri!=0) %>%group_by(HH) %>%  summarise(area_irri=mean(area_irri)) %>% 
  group_by(mean(area_irri))

num=stat %>% filter(area_irri!=0) %>% group_by(crop_cat) %>% count() %>% mutate(prt=  n/51 ) %>% arrange(desc(n))
size=stat %>% filter(area_irri!=0) %>% group_by(crop_cat) %>% summarise(area_irri=mean(area_irri))
# stat %>% group_by(crop_cat) %>% summarise(area_irri=mean(area_irri,na.rm = T)) 

inner_join(num,size) %>% kable() %>% kable_minimal()


####### Field crop + Fish Farming + hh_usage_percent[,c(1,5)]

surveyCP=
  rbind(surveyCP_field,surveyCP_fish)%>% 
  mutate(crop_area=ifelse(crop_area>0,crop_area*0.0339,crop_area)) %>% 
  filter( crop %in% c( "Paddy", "Wheat","Vegetables","Pulses","Fish_Farming" )) %>% # A tibble: 303 × 4
  pivot_wider(names_from = crop,values_from = crop_area) %>%   # A tibble: 98 × 7
  pivot_longer(-c(survey,HH), names_to = "crop_cat", values_to = "area_irri") %>%    # A tibble: 490 × 4
  mutate(area_irri=ifelse(is.na(area_irri),0, ifelse(area_irri==-888,NA,area_irri ) )) %>% 
  inner_join(hh_usage_percent[,c(1,5)] )

# total sample baseline
surveyCP %>%filter(area_irri!=0,survey=="baseline") %>%  
  group_by(survey,crop_cat) %>% count() %>% 
  mutate(prt= ifelse(usage_high_low=="high_usage", n/22,n/25 ))

# HH total
surveyCP %>%select(survey,HH,usage_high_low) %>%distinct() %>%
  group_by(survey,usage_high_low) %>% count()

#prt baseline
surveyCP %>%filter(area_irri!=0,survey=="baseline") %>%  
  group_by(survey,crop_cat,usage_high_low) %>% count() %>% 
  mutate(prt= ifelse(usage_high_low=="high_usage", n/22,n/25 ))
  
# size baseline
surveyCP %>%filter(survey=="baseline") %>%  
  group_by(survey,crop_cat,usage_high_low) %>%
  summarise(mean_area_irri=mean(area_irri,na.rm = T))

#prt
  surveyCP %>%filter(area_irri!=0,survey=="endline") %>%  
  group_by(survey,crop_cat,usage_high_low) %>% count() %>% 
  mutate(prt= ifelse(usage_high_low=="high_usage", n/22,n/23 ))

# size
surveyCP %>%filter(survey=="endline") %>%  
  group_by(survey,crop_cat,usage_high_low) %>%
  summarise(mean(area_irri,na.rm = T))



