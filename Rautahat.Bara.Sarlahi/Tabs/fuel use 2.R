# total_litres_consumed_dieselkero
#משתנה יחוס # irrigated_out_of_tot_land_cult

a <- Procurement_Baseline_2018_ %>% inner_join(Control_and_treatment_4_districts) %>%
  filter(TC==1,!is.na(total_litres_consumed_dieselkero),total_litres_consumed_dieselkero<10000) %>% 
  select(1,29)
  
#b <- Procurement_Endline_EPC_2019_%>% inner_join(Control_and_treatment_4_districts) %>%
#  filter(TC==1,!is.na(total_litres_consumed_dieselkero)) %>% select(1,29)

b1 <- Procurement_Baseline_2018_ %>% inner_join(Control_and_treatment_4_districts) %>%
  filter(TC==0,!is.na(total_litres_consumed_dieselkero),total_litres_consumed_dieselkero<10000) %>% 
  select(1,29)
b2 <- Procurement_Endline_EPC_2019_%>% inner_join(Control_and_treatment_4_districts) %>%
  filter(TC==0 ,!is.na(total_litres_consumed_dieselkero)) %>% select(1,29)
control <- inner_join(b1,b2)

bind <- bind_rows(control,a)
bind %>% group_by(TreatmentControl) %>% count()

bind <-  bind%>% 
  inner_join(Water_extraction_mechanism_Baseline_2018_[,c(1,2,54)]) %>% 
  inner_join(Procurement_Baseline_2018_[,c(1,19:20,27)])
  
total_irri_land <- R_Lands_Baseline_2018_[,c(1,8)] %>% group_by(household_questionnaire_id) %>%
  summarise(total_irri_land=sum(irrigated_out_of_tot_land_cult))  

om <- bind[c(54,64),]  

bind <- inner_join(bind,total_irri_land)
bind <- bind_rows(bind,om)
bind$total_irri_land [is.na(bind$total_irri_land)] <- 0 #replace NA to 0
bind <- arrange(bind,desc(total_irri_land))
matching_list <- bind[c(3:24,29:34,40:41,43:50,53:64),1:2]

# base-matchig
pb <- inner_join(Procurement_Baseline_2018_,matching_list) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(total_litres_consumed_dieselkero),n())

# end-matchig
pe <- inner_join(Procurement_Endline_EPC_2019_,matching_list) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(total_litres_consumed_dieselkero,na.rm = T),n())
  
