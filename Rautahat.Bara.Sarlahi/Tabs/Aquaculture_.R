R_Aquaculture_Baseline_2018_ <- inner_join(Aquaculture_Baseline_2018_,Control_and_treatment_4_districts)

# [11.1] Do you practice aquaculture/fish farming? # 1 Yes # 2 No #
R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>% 
  count(practice_aquaculture) %>% mutate(freq = n / sum(n))

# [11.2]In how many ponds do you cultivate the fish (or are planning to)?
table(R_Aquaculture_Baseline_2018_$ponds_cul_fish)

# [11.3]What is the total area of the pond? (in kathas)1424
aa <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(total_area_of_pond),total_area_of_pond!=400) %>% 
  summarise(n(),mean(total_area_of_pond))

# [11.6]If yes, what type of WEM is used for filling or draining pond for aquaculture? 
R_Aquaculture_Baseline_2018_ %>% filter(!is.na(type_wem_aquaculture)) %>% count(type_wem_aquaculture)


#[11.4] Do all ponds have sufficient water for aquaculture throughout the year?----
# 1 Yes # 2 No #

a <- R_Aquaculture_Baseline_2018_ %>%
  filter(!is.na(suff_water_aquaculture),total_area_of_pond!=400) %>%
  group_by(suff_water_aquaculture)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 
            
#[11.5] Is a WEM already in use for filling and draining the ponds?----

a <- R_Aquaculture_Baseline_2018_ %>%
  filter(!is.na(wem_used_forpond),total_area_of_pond!=400) %>% 
  group_by(wem_used_forpond)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 

#Are you planning to use the SPIP for aquaculture?----
a <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(planning_spip_aqua)) %>%
  count(planning_spip_aqua)%>% mutate(freq = n / sum(n)) 

a <- R_Aquaculture_Baseline_2018_ %>% filter(practice_aquaculture==1,!is.na(planning_spip_aqua)) %>%
  count(planning_spip_aqua)%>% mutate(freq = n / sum(n)) 

#-------
table(Aquaculture_Baseline_2018_$type_wem_aquaculture) #  1 Electric / 2   Diesel/ 3 other
table(Aquaculture_Baseline_2018_$average_tofill_pond)
table(Aquaculture_Baseline_2018_$average_time_drainpond)
table(Aquaculture_Baseline_2018_$type_of_wem_aqua_p1) #  1 Electric / 2   Diesel
table(Aquaculture_Baseline_2018_$type_of_wem_aqua_p2)
table(Aquaculture_Baseline_2018_$cost_per_hour_p1)
table(Aquaculture_Baseline_2018_$cost_per_hour_p2)
table(Aquaculture_Baseline_2018_$total_no_of_hours_aqua_p1)
table(Aquaculture_Baseline_2018_$total_no_of_hours_aqua_p2)
table(Aquaculture_Baseline_2018_$annual_cost_aquaculture)
table(Aquaculture_Baseline_2018_$annual_revenue_aquaculture)






