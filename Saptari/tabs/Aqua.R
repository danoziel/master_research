x <- S_Lands_I_Baseline_2017_ %>% filter(land_for_aquaculture_ponds>0) %>%group_by(TreatmentControl)%>%
  summarise(m=mean(land_for_aquaculture_ponds),n(),ha=m*0.0339)

Aquaculture_Baseline_2017_ %>% filter(total_area_of_pond>0) %>%group_by(TreatmentControl)%>% 
  summarise(m=mean(total_area_of_pond),ha=m*0.0339,n())

aquaculture <- rbind(Aquaculture_Baseline_2017_,
                     Aquaculture_Midline_2018_ ,
                     Aquaculture_Endline_2019_Saptari ,
                     Aquaculture_Baseline_2018_,
                     Aquaculture_Endline_EPC_2019_ ) %>% 
  inner_join(Control_and_treatment_4_districts)

aquaculture[58,4] <- 60
aquaculture[460,3] <- 3
aquaculture[460,4] <- 80
aquaculture[493,4] <- 80
aquaculture [461,3] <- 1
aquaculture [332,3] <- 1

which(aquaculture$household_questionnaire_id == "T107607004")
aquaculture$total_area_of_pond[which(aquaculture$household_questionnaire_id == "T107607004")] <- 10
aquaculture$ponds_cul_fish [which(aquaculture$household_questionnaire_id == "T107607004")] <- 3

which(aquaculture$household_questionnaire_id == "T109202002")
aquaculture [21,4] <- 12

aquaculture <- aquaculture %>% filter(practice_aquaculture==1)
aquaculture <- aquaculture %>% select(year,everything())

which(aquaculture$household_questionnaire_id == "C001705001")
[1] 13 41 60
aquaculture[13,5] <- 10

aquaculture$total_area_of_pond[which(aquaculture$household_questionnaire_id == "C008109001")] <- 40

> which(aquaculture$household_questionnaire_id == "T103207002")
[1] 19 26 61
> 
  > aquaculture[26,5] <- 9



