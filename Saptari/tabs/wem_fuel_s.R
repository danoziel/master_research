Water_extraction_mechanism_Baseline_2017_$days_in_a_season_pump_use_m_p1
Water_extraction_mechanism_Midline_2018_$days_in_a_season_pump_use_m_p1
Water_extraction_mechanisms_Endline_2019_Saptari$time_to_irrigate_1_katha__p_1

# "total liter for a season" - New VARs
# wem_liter_fuel_17----
wem_liter_fuel_17 <- Water_extraction_mechanism_Baseline_2017_ %>% 
  mutate(p1_s=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_s_p1*hours_per_day_use_s__p_1,
         p2_s=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_s_p2*hours_per_day_use_s__p_2,
         p3_s=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_s_p3*hours_per_day_use_s__p_3) %>%
  mutate(p1_m=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_m_p1*hours_per_day_use_m__p_1,
         p2_m=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_m_p2*hours_per_day_use_m__p_2,
         p3_m=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_m_p3*hours_per_day_use_m__p_3) %>% 
  mutate(p1_w=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_w_p1*hours_per_day_use_w__p_1,
         p2_w=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_w_p2*hours_per_day_use_w__p_2,
         p3_w=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_w_p3*hours_per_day_use_w__p_3)

wem_liter_fuel_17 <- wem_liter_fuel_17 %>% select(1,70,136:146) %>% replace(is.na(.), 0)
  
wem_liter_fuel_17 <- wem_liter_fuel_17 %>%
  mutate(p123_s=p1_s+p2_s+p3_s, p123_m=p1_m+p2_m+p3_m, p123_w=p1_w+p2_w+p3_w,
         p123_year=p1_s+p2_s+p3_s+p1_m+p2_m+p3_m+p1_w+p2_w+p3_w)

# wem_liter_fuel_18----
wem_liter_fuel_18 <- Water_extraction_mechanism_Midline_2018_ %>% 
  mutate(p1_s=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_s_p1*hours_per_day_use_s__p_1,
         p2_s=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_s_p2*hours_per_day_use_s__p_2,
         p3_s=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_s_p3*hours_per_day_use_s__p_3) %>%
  mutate(p1_m=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_m_p1*hours_per_day_use_m__p_1,
         p2_m=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_m_p2*hours_per_day_use_m__p_2,
         p3_m=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_m_p3*hours_per_day_use_m__p_3) %>% 
  mutate(p1_w=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_w_p1*hours_per_day_use_w__p_1,
         p2_w=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_w_p2*hours_per_day_use_w__p_2,
         p3_w=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_w_p3*hours_per_day_use_w__p_3)

wem_liter_fuel_18 <- wem_liter_fuel_18 %>% select(1,70,136:146) %>% replace(is.na(.), 0)

wem_liter_fuel_18 <- wem_liter_fuel_18 %>%
  mutate(p123_s=p1_s+p2_s+p3_s, p123_m=p1_m+p2_m+p3_m, p123_w=p1_w+p2_w+p3_w,
         p123_year=p1_s+p2_s+p3_s+p1_m+p2_m+p3_m+p1_w+p2_w+p3_w)

# wem_liter_fuel_19----
wem_liter_fuel_19 <- Water_extraction_mechanisms_Endline_2019_Saptari %>% 
  mutate(p1_s=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_s_p1*hours_per_day_use_s__p_1,
         p2_s=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_s_p2*hours_per_day_use_s__p_2,
         p3_s=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_s_p3*hours_per_day_use_s__p_3) %>%
  mutate(p1_m=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_m_p1*hours_per_day_use_m__p_1,
         p2_m=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_m_p2*hours_per_day_use_m__p_2,
         p3_m=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_m_p3*hours_per_day_use_m__p_3) %>% 
  mutate(p1_w=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_w_p1*hours_per_day_use_w__p_1,
         p2_w=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_w_p2*hours_per_day_use_w__p_2,
         p3_w=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_w_p3*hours_per_day_use_w__p_3)

wem_liter_fuel_19 <- wem_liter_fuel_19 %>% select(1,70,136:146) %>% replace(is.na(.), 0)

wem_liter_fuel_19 <- wem_liter_fuel_19 %>%
  mutate(p123_s=p1_s+p2_s+p3_s, p123_m=p1_m+p2_m+p3_m, p123_w=p1_w+p2_w+p3_w,
         p123_year=p1_s+p2_s+p3_s+p1_m+p2_m+p3_m+p1_w+p2_w+p3_w)

# wem_liter_fuel_17_18_19----
wem_liter_fuel_17_18_19 <- 
  rbind(wem_liter_fuel_17,wem_liter_fuel_18,wem_liter_fuel_19) %>% 
  inner_join(Control_and_treatment_4_districts)

wem_liter_fuel_17_18_19[wem_liter_fuel_17_18_19 == 0] <- NA

wem_liter_fuel_17_18_19%>%
  group_by(TreatmentControl,year) %>% 
  summarise(N=n(), Monsoon=mean(p123_m,na.rm = T),
            Summer=mean(p123_s,na.rm = T),Winter=mean(p123_w,na.rm = T),
            Year=mean(p123_year,na.rm = T))

write.csv(wem_liter_fuel_17_18_19, file = "C:/Users/Dan/Documents/R/Saptari/data/wem_liter_fuel_17_18_19.csv", row.names=FALSE)







