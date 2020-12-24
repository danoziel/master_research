# o-------

Land_18_19 <- rbind(Lands_Baseline_2018_,Lands_Endline_EPC_2019_) %>% 
  inner_join(Control_and_treatment_4_districts) %>% mutate( season_detail = season )

Land_18_19 [is.na(Land_18_19)] <- 0 #replace NA to 0

Land_18_19 <- 
 Land_18_19 %>%group_by(year,household_questionnaire_id) %>%  mutate(total_land_cultivated_year=sum(total_land_cultivated))

Land_18_19$season[Land_18_19$season == "MONSOON 2074"] <- "Monsoon"
Land_18_19$season[Land_18_19$season == "MONSOON 2075"] <- "Monsoon"

Land_18_19$season[Land_18_19$season == "WINTER 2074"] <- "Winter"
Land_18_19$season[Land_18_19$season == "WINTER 2075"] <- "Winter"

Land_18_19$season[Land_18_19$season == "ANNAUL 2074-75"] <- "Annual"
Land_18_19$season[Land_18_19$season == "ANNUAL 2075-76"] <- "Annual"

Land_18_19$season[Land_18_19$season == "SUMMER 2076"] <- "Summer"
Land_18_19$season[Land_18_19$season == "SUMMER 2075"] <- "Summer"

nca <- lands_I_18_19 %>% select(household_questionnaire_id,year, land_for_cultivation) 

Land_18_19 <- inner_join(Land_18_19,nca,by=c("household_questionnaire_id","year"))

Land_18_19[545,15] <- 60
Land_18_19[546,15] <- 60
Land_18_19[547,15] <- 60
Land_18_19[548,15] <- 60

Land_18_19[293,15] <- 14
Land_18_19[294,15] <- 14
Land_18_19[295,15] <- 14
Land_18_19[296,15] <- 14

Land_18_19[165,15] <- 35
Land_18_19[166,15] <- 35
Land_18_19[167,15] <- 35
Land_18_19[168,15] <- 35


  
  
  
#[4.9]Irrigated area out of total land cultivated = $Ir_Retio ----
# by HH / Year
as <- R_Lands_Baseline_2018_ %>%
  drop_na(total_land_cultivated,irrigated_out_of_tot_land_cult)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(sum_total=sum(total_land_cultivated),sum_ir=sum(irrigated_out_of_tot_land_cult)) %>% 
  mutate(ir.total=sum_ir/sum_total) %>% filter(ir.total<=1) %>% 
  summarise(mean(sum_ir), mean(ir.total),mean(total_land_cultivated,na.rm = T))

#Season
as <- R_Lands_Baseline_2018_ %>%
  select(household_questionnaire_id,total_land_cultivated,irrigated_out_of_tot_land_cult,season,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% group_by(season) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),mean(Ir_Retio),mean(total_land_cultivated))


# TC
T309708020






#[4.9]Irrigated area out of total land cultivated = $Ir_Retio ----
Treatment <- subset(R.Lands_Baseline_2018_,  TC == 1)

# by HH / Year-Treatment
R.Lands_Baseline_2018_ %>% filter(TC == 1) %>% 
select(household_questionnaire_id,irrigated_out_of_tot_land_cult,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(sum_Ir = mean(Ir_Retio), sum_total=sum(irrigated_out_of_tot_land_cult), n()) %>% 
  summarise(mean(sum_Ir), mean(sum_total),n())

#Season -Treatment
R.Lands_Baseline_2018_ %>% filter(TC == 1) %>% 
  select(household_questionnaire_id,irrigated_out_of_tot_land_cult,season,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(season) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),mean(Ir_Retio))
