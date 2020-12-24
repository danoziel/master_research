b2018 <- Procurement_Baseline_2018_ %>%
  filter(total_litres_consumed_dieselkero>0,total_litres_consumed_dieselkero<10000) %>% # rm T300901113 T305001120
  summarise(mean(total_litres_consumed_dieselkero),n())

tc2018 <- Procurement_Baseline_2018_ %>% inner_join(Control_and_treatment_4_districts) %>% 
  filter(total_litres_consumed_dieselkero>0,total_litres_consumed_dieselkero<10000) %>% 
  group_by(TreatmentControl)%>%summarise(mean(total_litres_consumed_dieselkero),n())
# filter remove T300901113 T305001120 

#extracting 'Endeline control HH' so I have it for the Matching----

Control_Endline <- Procurement_Endline_EPC_2019_ %>%
  inner_join(Water_extraction_mechanism_Endline_EPC_2019_) %>% 
  inner_join(Control_and_treatment_4_districts) %>% 
  filter(TC==0)  %>% select(household_questionnaire_id,163)

treatment_Baseline <- Procurement_Baseline_2018_ %>%
  inner_join(Water_extraction_mechanism_Baseline_2018_) %>%
  inner_join(Control_and_treatment_4_districts) %>%filter(TC==1) %>% 
  select(household_questionnaire_id,166)
  
bind <- bind_rows(Control_Endline,treatment_Baseline)

#bild maching for total_litres_consumed_dieselkero
pp <-  bind%>% 
  inner_join(Water_extraction_mechanism_Baseline_2018_[,c(1,2,54)]) %>% 
  inner_join(Procurement_Baseline_2018_[,c(1,19:20,26:27)]) %>%
  filter(total_litres_consumed_dieselkero<10000)

x <- R_Lands_Baseline_2018_ [,c(1,7)]%>% group_by(household_questionnaire_id) %>%
  summarise(total=sum(total_land_cultivated))

pp <- inner_join(pp,x)#[,c(1:6,8:9)]%>% arrange(desc(total_irri_land))
pp%>% group_by(TreatmentControl) %>% count()

# total_land_cultivated
matchig <-pp%>%filter(TreatmentControl=="Treatment"|
                        household_questionnaire_id %in% c("T308601105","T305104033","T304905099",
                                                 "T309306011","T307006116","T305404026",
                                                 "T302004124","T305404025","T309504024",
                                                 "T302609002","T304706131","T304706131",
                                                 "T304807087","T306708132","T304808083",
                                                 "T303505004","T303608010","T304702127",
                                                 "T301808002","T302608050","T307408001",
                                                 "T304705090","T301503134","T309904032"))
matchig <- matchig[,1:2]

# TreatmentControl----
# [7.16]Total litres of diesel/kerosene consumed for agriculture pumps----

# base-matchig
pb <- inner_join(Procurement_Baseline_2018_,matchig) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(total_litres_consumed_dieselkero),n())

# end-matchig
pe <- inner_join(Procurement_Endline_EPC_2019_,matchig) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(total_litres_consumed_dieselkero,na.rm = T),n())

# how many liters of fuel in a season per HH (df: R.WEM_liter_hr_season)----

ses <- R_WEM_liter_hr_season %>% inner_join(matchig) %>%
  group_by(TreatmentControl) %>% 
  summarise(HH=n(),monsoon=mean(p123_m), summer=mean(p123_s),
            winter=mean(p123_w),Year=mean(p123_year))





# Editing the Endline for :"total liter for a season"----

R_WEM_liter_hr_season_Endline <- Water_extraction_mechanism_Endline_EPC_2019_ %>% 
  select(1,61:63,100:105,112:117,124:129)

# rm liters [6.18]: higt amount & NA                      
R_WEM_liter_hr_season_Endline <- R_WEM_liter_hr_season_Endline %>%
  filter(!is.na(liters_of_fuels_p_hour_p_1) | !is.na(liters_of_fuels_p_hour_p_2))

# "total liter for a season" - New VARs
R_WEM_liter_hr_season_Endline <- R_WEM_liter_hr_season_Endline %>% 
  mutate(p1_s=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_s_p1*hours_per_day_use_s__p_1,
         p2_s=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_s_p2*hours_per_day_use_s__p_2,
         p3_s=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_s_p3*hours_per_day_use_s__p_3) %>%
  mutate(p1_m=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_m_p1*hours_per_day_use_m__p_1,
         p2_m=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_m_p2*hours_per_day_use_m__p_2,
         p3_m=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_m_p3*hours_per_day_use_m__p_3) %>% 
  mutate(p1_w=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_w_p1*hours_per_day_use_w__p_1,
         p2_w=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_w_p2*hours_per_day_use_w__p_2,
         p3_w=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_w_p3*hours_per_day_use_w__p_3)

R_WEM_liter_hr_season_Endline[,23:31][is.na(R_WEM_liter_hr_season_Endline[,23:31])] <- 0 # Replace  NA values to 0 on new VARs

R_WEM_liter_hr_season_Endline <- R_WEM_liter_hr_season_Endline %>%
  mutate(p123_s=p1_s+p2_s+p3_s, p123_m=p1_m+p2_m+p3_m, p123_w=p1_w+p2_w+p3_w,
         p123_year=p1_s+p2_s+p3_s+p1_m+p2_m+p3_m+p1_w+p2_w+p3_w)

R_WEM_liter_hr_season_Endline[,32:35][R_WEM_liter_hr_season_Endline[,32:35] == 0] <- NA
# -----
x <- R_WEM_liter_hr_season_Endline %>%filter(p123_m>0|p123_s>0|p123_w>0|p123_year>0) %>%
  inner_join(matchig) %>% 
  inner_join(Control_and_treatment_4_districts) %>% group_by(TreatmentControl) %>%
  summarise(HH=n(), monsoon=mean(p123_m), summer=mean(p123_s),
            winter=mean(p123_w),Year=mean(p123_year))

ses <- R_WEM_liter_hr_season %>% filter(p123_m>0 |p123_s>0|p123_w>0|p123_year>0) %>%
  inner_join(matchig) %>%
  group_by(TreatmentControl) %>% 
  summarise(HH=n(),monsoon=mean(p123_m), summer=mean(p123_s),
            winter=mean(p123_w),Year=mean(p123_year))
