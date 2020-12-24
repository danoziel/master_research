# o-------
land_17_18_19 <- land_17_18_19 %>%
  mutate( season_detail = season )

land_17_18_19$season[land_17_18_19$season == "MONSOON 2073"] <- "Monsoon"
land_17_18_19$season[land_17_18_19$season == "MONSOON 2074"] <- "Monsoon"
land_17_18_19$season[land_17_18_19$season == "MONSOON 2075"] <- "Monsoon"

land_17_18_19$season[land_17_18_19$season == "WINTER 2073"] <- "Winter"
land_17_18_19$season[land_17_18_19$season == "WINTER 2074"] <- "Winter"
land_17_18_19$season[land_17_18_19$season == "WINTER 2075"] <- "Winter"

land_17_18_19$season[land_17_18_19$season == "ANNUAL 2073-74"] <- "Annual"
land_17_18_19$season[land_17_18_19$season == "ANNUAL 2074-75"] <- "Annual"
land_17_18_19$season[land_17_18_19$season == "ANNUAL 2075-76"] <- "Annual"

land_17_18_19$season[land_17_18_19$season == "SUMMER 2074"] <- "Summer"
land_17_18_19$season[land_17_18_19$season == "SUMMER 2076"] <- "Summer"
land_17_18_19$season[land_17_18_19$season == "SUMMER 2075"] <- "Summer"

# crop_intensity                  ----
lsci <- land_17_18_19%>% filter(total_land_cultivated_year>0) %>%
  
  mutate(NEW_total_land_cult= case_when(
    TreatmentControl=="Control" & land_for_cultivation < total_land_cultivated ~ NA_real_,
    TRUE ~ total_land_cultivated)) %>% 
  
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(gross=sum(NEW_total_land_cult,na.rm = T),net=mean(land_for_cultivation))%>%
  mutate(crop_intensity=gross/net*100) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(),Mean=mean(crop_intensity)) %>% 
  mutate(across(is.numeric, round))

--------------------------------------------------------------
  #  irri intensity -----
  
# season
xx <- land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  mutate(irrigation_intens=irrigated_out_of_tot_land_cult/total_land_cultivated*100) %>% 
  filter(irrigation_intens>= 0,irrigation_intens<=100) %>% 
  group_by(TreatmentControl,season,year) %>% 
  summarise(mean(irrigation_intens))

# year
xx <- land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  mutate(irrigation_intens=irrigated_out_of_tot_land_cult/total_land_cultivated*100) %>% 
  filter(irrigation_intens>= 0,irrigation_intens<=100) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(mean(irrigation_intens))




  mutate(across(is.numeric, round))













