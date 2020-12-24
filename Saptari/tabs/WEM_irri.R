
#-------------------Irrigation hours - 4 districts-  `water_use_6.2------------------------------------------
WS2017 <-
  Water_extraction_mechanism_Baseline_2017_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WS2018 <-
  Water_extraction_mechanism_Midline_2018_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)


WS2019 <-
  Water_extraction_mechanisms_Endline_2019_Saptari %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WRSB2018 <-
  Water_extraction_mechanism_Baseline_2018_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WRSB2019 <-
  Water_extraction_mechanism_Endline_EPC_2019_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

water_use_6.2 <- rbind(WS2017,WS2018,WS2019,WRSB2018,WRSB2019) %>% 
  left_join(Control_and_treatment_4_districts)

water_use_6.2 <- water_use_6.2 %>% 
  mutate(ps1=days_in_a_season_pump_use_s_p1* hours_per_day_use_s__p_1,
         ps2=days_in_a_season_pump_use_s_p2* hours_per_day_use_s__p_2,
         ps3=days_in_a_season_pump_use_s_p3* hours_per_day_use_s__p_3,
         pm1=days_in_a_season_pump_use_m_p1* hours_per_day_use_m__p_1,
         pm2=days_in_a_season_pump_use_m_p2* hours_per_day_use_m__p_2,
         pm3=days_in_a_season_pump_use_m_p3* hours_per_day_use_m__p_3,
         pw1=days_in_a_season_pump_use_w_p1* hours_per_day_use_w__p_1,
         pw2=days_in_a_season_pump_use_w_p2* hours_per_day_use_w__p_2,
         pw3=days_in_a_season_pump_use_w_p3* hours_per_day_use_w__p_3) %>% 
  mutate(Summer_hr = rowSums(.[names(.)[24:26]], na.rm = T),
         Monsoon_hr = rowSums(.[names(.)[27:29]], na.rm = T),
         Winter_hr = rowSums(.[names(.)[30:32]], na.rm = T),
         Yearly_hr = rowSums(.[names(.)[24:32]], na.rm = T),
         p1_hr = rowSums(.[names(.)[c(24,27,30)]], na.rm = T),
         p2_hr = rowSums(.[names(.)[c(25,28,31)]], na.rm = T),
         p3_hr = rowSums(.[names(.)[c(26,29,32)]], na.rm = T),
  )


water_use_6.2$district [water_use_6.2$district %in% 1:3] <- "RSB"
water_use_6.2$district [water_use_6.2$district == "SAPTARI" ] <- "Saptari"


water_use_6.2 <- water_use_6.2 %>% 
  mutate(days_monsoon = rowSums(.[names(.)[c(10,12,14)]], na.rm = T),
         days_winter = rowSums(.[names(.)[c(16,18,20)]], na.rm = T),
         days_summer = rowSums(.[names(.)[c(4,6,8)]], na.rm = T),
         days_year = rowSums(.[names(.)[c(4,6,8,10,12,14,16,18,20)]], na.rm = T),
         days_p1 = rowSums(.[names(.)[c(4,10,16)]], na.rm = T),
         days_p2 = rowSums(.[names(.)[c(6,12,18)]], na.rm = T),
         days_p3 = rowSums(.[names(.)[c(8,14,20)]], na.rm = T)
  )

# remove 0
water_use_6.2[water_use_6.2==0] <- NA

write.csv(water_use_6.2,"C:/Users/Dan/Documents/R/Saptari/data/water_use_6.2.csv", row.names = FALSE)


#organize the dataset to HH as rows-----

# monsoon
x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,mnsn)

xmnsn <- spread(x, year, mnsn)
xmnsn <- xmnsn %>% rename(monsoon_2017="2017",monsoon_2018="2018",monsoon_2019="2019")

# summer
x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,smr)

xsmr <- spread(x, year, smr)
xsmr <- xsmr %>% rename(summer_2017="2017",summer_2018="2018",summer_2019="2019")

# winter
x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,wntr)

xwntr <- spread(x, year, wntr)
xwntr <- xwntr %>% rename(winter_2017="2017",winter_2018="2018",winter_2019="2019")

# pump 1
xp1 <- spread(x, year, p1)
xp1 <- xp1 %>% rename(PUMP1_2017="2017",PUMP1_2018="2018",PUMP1_2019="2019")


# xs the combined dataset
xs <- inner_join(xmnsn,xsmr) %>% 
  inner_join(xwntr) 
# and combin with dataset from `Master.R` tab



# hours -pump_water_use_6.2_hr----
# (household_questionnaire_id,year,district,pump_type__p_1,pump_type__p_2,pump_type__p_3)

# pump 1 and 2
p1p2p3 <- water_use_6.2 %>% 
  filter(TreatmentControl== "Treatment",district=="Saptari",
         !household_questionnaire_id %in% c("T308707002" ,"T301911010")) %>% 
  group_by(year) %>% 
  summarise_at(c("p1_hr","p2_hr","p3_hr","Yearly_hr"), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))

# Yearly
pump_water_use_6.2_hr <- water_use_6.2 %>% 
  filter(TreatmentControl== "Treatment",district=="Saptari",
         !household_questionnaire_id %in% c("T308707002" ,"T301911010")) %>% 
  group_by(district, year) %>% 
  summarise_at(c("Yearly"), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))

water_use_6.2 %>% 
  filter(TreatmentControl== "Treatment",district=="RBS",
         !household_questionnaire_id %in% c("T308707002" ,"T301911010")) %>% 
  group_by(year) %>% 
  summarise_at(c("p1_hr","p2_hr","p3_hr","Yearly_hr"), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))


SAPTARI_season_water_use_6.2_hr <- water_use_6.2 %>% 
  filter(TreatmentControl== "Treatment",district=="RSB",
         !household_questionnaire_id %in% c("T308707002" ,"T301911010")) %>% 
  group_by(district, year) %>% 
  summarise_at(c("Monsoon","Winter","Summer"), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round)) %>% 
  arrange( desc(district))

pump_water_use_6.2_hr <- spread(pump_water_use_6.2_hr,district,Yearly)
pump_water_use_6.2_hr <-   gather(pump_water_use_6.2_hr, key="district", value="Yearly", c("Saptari","RSB"))


ggplot(data=pump_water_use_6.2_hr, aes(x=year, y=Yearly,fill=year)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = Yearly, group = year),
            position = position_dodge(0.8),
            vjust = 1.2, size = 3.5, color = "white")+
  ggtitle("Saptari")+
  xlab(" ") +
  theme(legend.position = "none",plot.title = element_text(size = rel(2), hjust = 0.5))+
  scale_x_continuous(breaks=seq(2018,2019,1))


# days -The number of days the pump is running --------------------

days_water_use_6.2 <- days_water_use_6.2 %>% 
  filter(TreatmentControl== "Treatment") %>% 
  group_by(district, year) %>% 
  summarise_at(c("days_smr","days_mnsn","days_wntr","days_year"), sum, na.rm = TRUE)

