
# dataset pump_type ----
pump_type17 <- 
  Water_extraction_mechanism_Baseline_2017_ %>%
  select(household_questionnaire_id, pump_type__p_1,pump_type__p_2,pump_type__p_3,year)
  
pump_type18 <- 
  Water_extraction_mechanism_Midline_2018_ %>%
  select(household_questionnaire_id, pump_type__p_1,pump_type__p_2,pump_type__p_3,year)

pump_type19 <- 
  Water_extraction_mechanisms_Endline_2019_Saptari %>%
  select(household_questionnaire_id, pump_type__p_1,pump_type__p_2,pump_type__p_3,year)

pump_type_sap <- 
  bind_rows(pump_type17,pump_type18,pump_type19) %>% 
  mutate(district="Saptari")


pump_type2018 <- 
  Water_extraction_mechanism_Baseline_2018_ %>%
  select(household_questionnaire_id, pump_type__p_1,pump_type__p_2,pump_type__p_3,year)

pump_type2019 <- 
  Water_extraction_mechanism_Endline_EPC_2019_ %>%
  select(household_questionnaire_id, pump_type__p_1,pump_type__p_2,pump_type__p_3,year)

pump_type <- 
  bind_rows(pump_type2018,pump_type2019) %>% 
  mutate(district="Rautahat_Bara_Sarlahi") %>% 
  bind_rows(pump_type_sap) %>% 
  inner_join(Control_and_treatment_4_districts) %>% filter(TC==1) %>% 
  select(-c(7,8))

# ---------------------

PT <- 
  pump_type %>% 
  filter(district == "Saptari") %>% 
  group_by(district,year) %>% 
  count(pump_type__p_2)

pump_type %>% 
  filter(district == "Rautahat_Bara_Sarlahi") %>% 
  group_by(district,year) %>% 
  count(pump_type__p_1)

  rbind(Procurement_17_18_19,Procurement_18_19) %>% 
  filter(TreatmentControl=="Treatment") %>% 
  mutate(district=ifelse(district == "SAPTARI","Saptari",
                         ifelse(district %in% c("1","2","3"),"Rautahat_Bara_Sarlahi", district))) %>% 
  select(year,district,household_questionnaire_id,do_you_use_a_domestic_connect) %>% 
  group_by(district,year) %>% count(do_you_use_a_domestic_connect)
