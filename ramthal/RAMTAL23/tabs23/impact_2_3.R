
#  [ 
library(readr)
a_plots_crop <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_crop.csv")

m1 <-  lm(inputs_per_acre_K  ~ in_project + dist_Km_boundary +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = dr22 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)

df %>% group_by(agri_vars) %>% 
  summarise(n=n(),Q99=quantile(val_22, probs = 0.99, na.rm = TRUE))


library(dplyr)
library(tidyr)
library(tidyverse)

library(kableExtra )

library(RColorBrewer)
display.brewer.pal(n = 11, name = 'Dark2')
my_colors <- brewer.pal(8, "Dark2") # Paired, Set1

####
df_economic_22 <- 
  df_land %>% 
  left_join(df_inputs) %>% 
  left_join(df_revenue_22) %>% 
  left_join(df_assets_22) %>% 
  pivot_longer(-hh_id,names_to = "economic_vars", values_to = "val_22")%>% 
  rbind(
    df_income_NonCrop_22 %>% select(-income_NonCrop_mhh) %>% 
      rename(economic_vars = income_type, val_22=income_NonCrop)
    ) %>% 
  rbind(df_income_agri_22) %>% 
  arrange(hh_id)

####
df_economic_bl <- 
  df_land_bl %>% 
  left_join(df_inputs_bl) %>% 
  mutate(k_revenue_per_acre=0) %>% 
  left_join(df_assets_bl) %>% 
  pivot_longer(-hh_id,names_to = "economic_vars", values_to = "val_BL")%>% 
  rbind(
    df_income_NonCrop_bl %>% select(-income_NonCrop_mhh) %>% 
      rename(economic_vars = income_type, val_BL=income_NonCrop)
    ) %>%  rbind(df_income_agri_BL  ) %>% arrange(hh_id)

df_economic <- 
  df_economic_22 %>% 
  left_join(df_economic_bl) %>% 
  left_join(rmtl_cntrl_vars )%>%
  mutate(	
    val_22= ifelse(economic_vars =="k_revenue_per_acre" & val_22>80,NA,val_22))







#__________________________  land holding  _ ----

# ) DFs
# df_land_holding is same as land_holding from agri_vars in impact.1.R script
df_land_holding <- # former [df_land] and [df_land_bl]
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(val_22=sum(acres,na.rm = T)) %>% 
  mutate(ecomony_vars= "land_holding" ) %>% 
  select(hh_id, ecomony_vars, val_22) %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,total_acre16)) %>% 
  rename(val_bl= total_acre16) %>% 
  # summarise(quantile(val_bl, probs = 0.99, na.rm = TRUE))
  mutate(val_22= ifelse(val_22 > 40 ,NA,val_22) 
  )
                
df_land_holding %>% summarise(quantile(val_bl, probs = 0.99, na.rm = TRUE))


#__________________________  INPUTS  _______ ----

#_______ df_inputs
# L70		irrigation equipment
# L71		Mechanization
# L72		Fuel
# L73		Labor

land_cult_2022 <-  # same as in impact.1.R script
  a_plots_crop %>% 
  left_join(a_plots_size) %>% 
  select(hh_id, season, plotID, acres) %>% distinct()  %>% 
  group_by(hh_id,season) %>% summarise(acres=sum(acres,na.rm = T),.groups = "drop") %>% 
  filter(season != "KHA22") %>% 
  group_by(hh_id) %>% summarise(cult_acre_22=sum(acres,na.rm = T),.groups = "drop")


df_inputs_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, contains ("l70"),contains ("l71"),# contains ("l72"),
         contains ("l73")) %>% 
  pivot_longer(-hh_id,names_to = "observation",values_to = "inputs_Rs" ) %>% 
  separate(observation, into = c("inputs", "season"), sep = "_") %>% 
  filter(season != "KHA22") %>% 
  group_by(hh_id) %>% 
  summarise(inputs_Rs = sum(inputs_Rs,na.rm = T),.groups = "drop"
  ) %>% 
  left_join(land_cult_2022) %>% 
  mutate(val_22 = inputs_Rs/cult_acre_22/1000,
         val_22 = ifelse(val_22==0, NA, val_22),
         ecomony_vars= "inputs_per_acre_K") %>% 
  select(hh_id, ecomony_vars, val_22)

cult_acre_2016 <- BL_plotCrop  %>% select( hh_id,plotID,season) %>% distinct() %>% 
  left_join(bl6_plotAcre %>% rename(plotID=plot_num)) %>% 
  filter(season != "rabi_2014_15") %>% 
  # filter(season != "rabi_2015_16") %>% 
  group_by(hh_id) %>% 
  summarise(cult_acre=sum(plot_acre))

inputs_bl <- 
  rmtl_baseline2016 %>% 
  select(hh_id,D49_2015:D60_2014) %>% select(hh_id , contains("2015")) %>% 
  pivot_longer(-hh_id,names_to = "inputs", values_to = "inputs_Rs" ) %>% 
  separate(inputs, into = c("inputs", "year"), sep = "_") %>% 
  select(-year) %>% 
  filter(inputs %in% c( "D58", # "irriEquipment"
                        "D59", # "mechanization"
                        "D60")) %>%  # "labor"
  group_by(hh_id) %>% 
  summarise(inputs_Rs=sum(inputs_Rs,na.rm = T)) %>% 
  left_join( cult_acre_2016) %>% 
  mutate(val_bl= inputs_Rs/cult_acre/1000, 
         val_bl = ifelse(val_bl==0, NA, val_bl),
         ecomony_vars="inputs_per_acre_K") %>% 
  select(hh_id, ecomony_vars, val_bl)

inputs_bl %>% right_join(hh_2022) %>% t_test(val_bl ~ farmers_hh,detailed = T)

df_inputs_22 %>% summarise(p99 = quantile(val_22, probs = 0.99, na.rm = TRUE))
inputs_bl %>% summarise(p99 = quantile(val_bl, probs = 0.99, na.rm = TRUE))
 
df_inputs <- 
  df_inputs_22 %>% left_join(inputs_bl) %>% 
  mutate(val_22= ifelse(val_22 > 40, NA, val_22 )) 





#__________________________  Revenue  _______ ----

#__________________________  income_agri  _______ ----

#____________________  df_income_NonCrop   ____________________________________ ----

# F1	Income sent by seasonal migrating household members
# F2	Remittances (from permanent migrants)
# F3 f3	 [Calculate from cultivation module]
# f3_1   Rented land
# F4	   Own livestock (Net profit)
# F5	Own non-agricultural business (Net profit)
# F6	Salaried job
# F7	Casual work or daily labor by current household members
# F9	Government pension or scheme
# F10	Rent/lease of property or land (land, house, vehicle, electronic appliances, tractor, etc.)
# F11	   Other jobs or activities not already mentioned
# F12	Total (Rs.) [sum F1-F11]
# F13	   According to what you indicated, your total HH income is Rs. [     ].

df_income_NonCrop_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, f1_amt, f2_amt,f3_1_amt, f4_amt, f5_amt, f6_amt, f7_amt, f9_amt, f10_amt, f11_amt 
  ) %>%
  pivot_longer(-hh_id ,
               names_to = "income_type_f",
               values_to = "income22") %>% 
  mutate(income22 = ifelse(income22 == -999,0,income22),
         income_type=
           ifelse(income_type_f %in% c("f1_amt","f2_amt","f9_amt"),
                  "k_income_assistance","k_income_independent" )) %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop") %>% 
  left_join( rmtl_srvy22 %>% select(hh_id, r1 )) %>% 
  mutate(r1 = ifelse(r1 == 0,1,r1)) %>% 
  mutate(income_NonCrop_mhh=income_NonCrop/r1 ) %>% select(-r1)

d_I1 <- df_income_NonCrop_22 %>% select(-income_NonCrop_mhh) %>% rename(val_22=income_NonCrop)
d_I2 <- df_income_NonCrop_22 %>% select(-income_NonCrop) %>% 
  mutate(income_type = paste0(income_type, "_mhh"))%>% rename(val_22=income_NonCrop_mhh)

df_income_NonCrop_bl <- 
  rmtl_baseline2016 %>%
  select(hh_id,starts_with("F")) %>% 
  select(hh_id, contains("year"),-F3_year, -F12_year)%>%
  pivot_longer(-hh_id ,
               names_to = "income_type_f",
               values_to = "income22") %>% 
  mutate(income_type=
           ifelse(income_type_f %in% c("F1_year","F2_year","F9_year"),
                  "k_income_assistance","k_income_independent" ))  %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop") %>% 
  left_join( rmtl_baseline2016 %>% select(hh_id, C1 )) %>% 
  mutate(C1=ifelse(C1<1,1,C1), # Total household members
         income_NonCrop_mhh=income_NonCrop/C1 # Normalized to number of household members
  ) %>% select(-C1)

d_I1b <- df_income_NonCrop_bl %>% select(-income_NonCrop_mhh) %>% rename(val_bl=income_NonCrop)
d_I2b <- df_income_NonCrop_bl %>% select(-income_NonCrop) %>% 
  mutate(income_type = paste0(income_type, "_mhh"))%>% rename(val_bl=income_NonCrop_mhh)

df_income_NonCrop <- 
  d_I1 %>% left_join(d_I1b) %>% 
  # rbind( d_I2 %>% left_join(d_I2b) ) %>% 
  rename(ecomony_vars=income_type) %>% 
  mutate(
    val_22 = case_when(
      ecomony_vars == "k_income_assistance" & val_22 > 306   ~ NA_real_,
      TRUE ~ val_22 ))


# TYPE of income ----

# TYPE of income - amt

F_2022 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("f") ) %>% 
  select(hh_id:f2_amt,f5:f10_amt,f12) %>% 
  pivot_longer(-hh_id ,
               names_to = "income_type",
               values_to = "income22") %>% 
  mutate(income_type = ifelse(income_type=="f12","f12_amt",income_type))


rmtl_baseline2016 %>% select(hh_id,starts_with("F"))
# library(stringr)
F_2015 <- rmtl_baseline2016 %>% # right_join(hh_2022) %>% 
  select(hh_id,starts_with("F")) %>% 
  select( -ends_with("month"),
          -starts_with(c("farmer","F3","F4","F11","F13","F14")) )%>% 
  pivot_longer(-hh_id ,
               names_to = "income_type",
               values_to = "income15")%>% 
  mutate( 
    income_type = str_to_lower(income_type),  # make all lowercase 
    income_type = case_when(
      str_detect(income_type, "_year$") ~ str_replace(income_type, "_year$", "_amt"),
      str_detect(income_type, "_source$") ~ str_replace(income_type, "_source$", ""),
      TRUE ~ income_type) )%>% 
  mutate( 
    income15 = case_when(
      income15 == 0 ~ NA,
      income15 == 2 ~ 0 ,
      TRUE ~ income15) )



income <- F_2022 %>% 
  left_join(F_2015) %>%
  mutate(income22=ifelse(income22==-999,NA,income22)) %>% 
  filter(!(is.na(income22) & is.na(income15))) %>%  
  mutate(income22= ifelse(income22 > 1, income22/1000, income22),
         income15= ifelse(income15 > 1, income15/1000, income15) )
income$income22[is.na(income$income22)] <- 0
income$income15[is.na(income$income15)] <- 0

# TYPE of income - binary

df_income_NonCrop_bl_binary <- 
  rmtl_baseline2016 %>%
  select(hh_id,starts_with("F")) %>% 
  select(hh_id, contains("year"),-F3_year,-F4_year, -F12_year)%>%
  pivot_longer(-hh_id ,
               names_to = "income_type_f",
               values_to = "income22") %>% 
  mutate(income_type=
           ifelse(income_type_f %in% c("F1_year","F2_year","F9_year"),
                  "income_assistance", # Income by seasonal/  permanent  migrating OR Government pension or scheme
                  "income_NonCrop" ))  %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop"
  ) %>% 
  mutate(income_NonCrop_01=ifelse(income_NonCrop == 0,0,1) 
  ) %>% select(-income_NonCrop) %>% 
  pivot_wider(names_from = income_type,
              values_from =income_NonCrop_01 )

#____________________  df_assets  __________________________       ----

# How many of this item does the household currently own? (0 if none)

# LIVESTOCK # [E6Cows E7Bullock E8Buffaloes E9Goats&sheep]
# FARM EQUIPMENT # [E10Tractor E11Plough E12Thresher E13Seed drill] 
# VEHICLES # [E15Cycles E16Motorcycles E17Cars] 
# HH ITEMS # [E18Fridge # E19	Television]

### REMOVE [E14 JCB]
### skip E20Gold E21Silver

# assets_22 .........
vars_e01 <- rmtl_srvy22 %>% select(hh_id,starts_with("e"),-e21) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",       # LIVESTOCK
    "Tractor","Plough","Thresher","Seed_drill","JCB", # FARM EQUIPMENT 
    "Cycles","Motorcycles","Cars",                    # VEHICLES
    "Fridge","TV"), .cols = 2:15) %>%                 # HH ITEMS
  select(-JCB)
vars_e01[is.na(vars_e01)] <- 0

# vars_e01 %>% count(Buffaloes)
# vars_e01$Buffaloes=ifelse(vars_e01$Buffaloes>9,NA,vars_e01$Buffaloes)

assets_22 <-  
  vars_e01 %>%  
  mutate(
    Livestock= Cows+Bullock+Buffaloes+Goats_sheep,
    Farm_equipments=Tractor+Plough+Thresher+Seed_drill,
    Vehicles_Home= Cycles+Motorcycles+Cars+Fridge+TV,
    Total_assets= Livestock+Farm_equipments+Vehicles_Home) %>% 
  select(hh_id,Livestock,Farm_equipments,Vehicles_Home) %>%
  mutate(across( -hh_id, ~ ifelse(if_all(-hh_id, ~ .x == 0), NA, .x)
  )) %>% 
  pivot_longer(-hh_id ,names_to = "ecomony_vars",values_to = "val_22")

# assets_15 .........

vars_e01_BL <- 
  rmtl_baseline2016 %>% select(hh_id,starts_with("E")) %>% 
  select(hh_id, ends_with("_1"),-c(E20_1:E11_to_E13_1)) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",
    "Tractor","Plough","Thresher","Seed_drill","JCB",
    "Cycles","Motorcycles","Cars",
    "Fridge","TV"), .cols = 2:15)%>%
  select(-JCB)
vars_e01_BL[is.na(vars_e01_BL)] <- 0

assets_15 <-  
  vars_e01_BL %>% 
  mutate(
    Livestock= Cows+Bullock+Buffaloes+Goats_sheep,
    Farm_equipments=Tractor+Plough+Thresher+Seed_drill,
    Vehicles_Home= Cycles+Motorcycles+Cars + Fridge+TV,
    Total_assets = Livestock + Farm_equipments + Vehicles_Home) %>% 
  select(hh_id,Livestock,Farm_equipments,Vehicles_Home) %>%
  mutate(across( -hh_id, ~ ifelse(if_all(-hh_id, ~ .x == 0), NA, .x)
  )) %>% 
  pivot_longer(-hh_id ,names_to = "ecomony_vars",values_to = "val_bl")

df_assets <- assets_22 %>% 
  inner_join(assets_15) %>% 
  mutate(
    val_22 = case_when(
      ecomony_vars == "Farm_equipments" & val_22 > 6   ~ NA_real_,
      ecomony_vars == "Livestock" & val_22 >= 100   ~ NA_real_, # 60
      ecomony_vars == "Vehicles_Home" & val_22 > 7   ~ NA_real_, # 7
      TRUE ~ val_22 ))

# Analysis ----

df_economic <- 
  df_land_holding %>% 
  rbind(df_inputs) %>% 
  rbind(df_return_invest %>% rename(ecomony_vars=economic_vars)) %>% 
  rbind(df_income_NonCrop) %>% 
  rbind(df_assets) %>% 
  left_join(rmtl_cntrl_vars)
#

# Balance Tests ----
library(kableExtra)
library(rstatix)
library(broom)

df_economic %>% # filter(cardinal_direction == "south" ) %>% 
  group_by(ecomony_vars) %>%
  do(tidy(t.test(val_bl ~ in_project, data = .))) %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(ecomony_vars, estimate2,estimate1, p.value) %>% 
  rename(In = estimate2 ,Out = estimate1 ) %>% 
  kableExtra::kable() %>% kableExtra::kable_paper()


# REG model     ----
# (1) model formula inputs
fml <- val_22   ~ in_project + dist_Km_boundary + 
  val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
  total_acre16 + housing_str321 + 
  job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + 
  Thresher + Seed_drill + Motorcycle + Fridge
# fml <- val_22   ~ in_project

# (2) Nest by status and fit ----
fits <- df_economic %>%
  group_by(ecomony_vars) %>% nest() %>%
  mutate(  model = map(data, ~ lm(fml, data = .x)),
           coefs = map(model, tidy),
           stats = map(model, glance))

# (3) Stacked outputs + (4) Join with model summary stats
inproj_with_fit <- 
  fits %>% unnest(coefs) %>%  filter(term == "in_project") %>% 
  select(ecomony_vars, estimate, std.error, p.value) %>% ungroup() %>% 
  left_join( fits %>% unnest(stats) %>% select(ecomony_vars,nobs, r.squared) 
  ) %>% 
  pivot_longer(-ecomony_vars, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = ecomony_vars, values_from = value )

# (5) control mean
control_mean <- df_economic %>% filter(in_project==0) %>% 
  group_by(ecomony_vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% 
  pivot_wider (names_from = "ecomony_vars", values_from = "Mean") %>% 
  mutate(metric="control_mean") 

# REG table     ----

inproj_with_fit %>%
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  )))   %>%    
  kableExtra::kable() %>% kableExtra::kable_paper()


# (Intercept) for naive model
fits %>% unnest(coefs) %>%  filter(term == "(Intercept)") %>% 
  select(term,agri_vars, estimate) %>% ungroup()  %>% 
  left_join( fits %>% unnest(stats) %>% select(agri_vars) 
  ) %>% 
  pivot_longer(-c(term,agri_vars), names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = agri_vars, values_from = value ) %>% 
  select(-metric) %>% 
  kable() %>% kable_paper()


# PLOT                  ----
library(jtools)
library(ggplot2)

#custom theme to format ----
mp_theme=theme_bw()+
  theme(
    panel.grid.major.y =element_blank(),
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))

models_list_land <- 
  fits_land %>% { setNames(.$model, .$status) } # names become model names in the legend

m1_plot <- 
  plot_summs(models_list_land ,coefs = c("In Project" = "in_project"),
             model.names = names(models_list_land),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Land Holding in Acre", y = NULL) +
  xlim(-1, 2) 

m1_plot + mp_theme




  




# LEVELS -------------{Season}---{input_type}----
# inputs  {Season} {input_type} ----


costesA <- rmtl_srvy22 %>% 
  select(hh_id, contains ("l70"),  # irrigation equipment
                contains ("l71"),  # Mechanization
                # contains ("l72"),  # Fuel
                contains ("l73")) %>%  # Labor
  pivot_longer(-hh_id,names_to = "observation",values_to = "inputs_Rs" ) %>% 
  separate(observation, into = c("inputs", "season"), sep = "_") %>% 
  mutate(inputs=case_when(inputs=="l70"~"irriEquipment",inputs=="l71" ~ "mechanization",inputs=="l73" ~ "labor", T ~ "fuel")
         ) %>% 
  mutate(inputs_Rs=ifelse(inputs_Rs <= 0 ,NA,inputs_Rs) ) 

a_plots_crop %>% count(hh_id,season,plotID) %>% 
  # [a_plots_size] in the top of this script
  left_join( a_plots_size %>% select(hh_id, plotID, acres)) %>% 
  group_by(hh_id,season) %>% 
  summarise(sum_acre=sum(acres,na.rm = T),.groups = "drop") %>% 
  left_join(costesA) %>% 
  filter(!is.na( inputs ))

# inputs Baseline 
inputs_2015 <- rmtl_baseline2016 %>% 
  select(hh_id,D49_2015:D60_2014) %>% select(hh_id , contains("2015")) %>% 
  pivot_longer(-hh_id,names_to = "inputs", values_to = "inputs_Rs" ) %>% 
  separate(inputs, into = c("inputs", "year"), sep = "_") %>% 
  select(-year) %>% 
  mutate(
    inputs = case_when(
      inputs=="D58" ~ "irriEquipment",
      inputs=="D59" ~ "mechanization",
      inputs=="D60" ~ "labor",
      TRUE ~ "Else")
  ) %>% filter(inputs != "Else" ) %>% ungroup() 





# Revenue {Season}  ----
# L78	Total revenue? [season-crop]


revenue_rabi_22 =
  a_plots_revenue %>% filter(season=="rabi") %>% 
  group_by(hh_id) %>% 
  summarise(revenue=sum(plotRevenue,na.rm = T)) %>% ungroup() %>% 
  left_join(size_acre %>% group_by(hh_id) %>% summarise(acres= sum(acres)) ) %>% 
  mutate(k_revenue_per_acre=revenue/acres/1000) %>% 
  left_join(rmtl_cntrl_vars)

quantile(revenue_rabi_22$k_revenue_per_acre, probs = 0.98)
drS22 <- revenue_rabi_22  %>% filter(k_revenue_per_acre < 25 )
drS22 %>% group_by(in_project) %>% summarise(rev=mean(k_revenue_per_acre,na.rm = T)) 

m1 <-  lm(k_revenue_per_acre  ~ in_project + dist_Km_boundary +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = drS22 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)


revenue_kharif_22 <- 
  a_plots_revenue %>% filter(season !="rabi") %>% 
  group_by(hh_id,season) %>% 
  summarise(revenue=sum(plotRevenue,na.rm = T)) %>%
  group_by(hh_id) %>% 
  summarise(revenue=mean(revenue,na.rm = T)) %>% ungroup() %>% 
  left_join(size_acre %>% group_by(hh_id) %>% summarise(acres= sum(acres)) ) %>% 
  mutate(k_revenue_per_acre=revenue/acres/1000) %>% 
  left_join(rmtl_cntrl_vars)

quantile(revenue_kharif_22$k_revenue_per_acre, probs = 0.99)
quantile(revenue_kharif_22$k_revenue_per_acre, probs = 0.98)

drK22 <- revenue_kharif_22  %>% filter(k_revenue_per_acre < 35 )
drK22 %>% group_by(in_project) %>% summarise(rev=mean(k_revenue_per_acre,na.rm = T)) 

m1 <-  lm(k_revenue_per_acre  ~ in_project + dist_Km_boundary +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = drK22 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)














#__________________________ Migration  __________________________       ----

# r1 How many household members live in this house?
n_roster_bl <- rmtl_baseline2016 %>% select(hh_id, C1) %>% rename(roster_N.bl=C1)
n_roster_18 <- rmtl_midline2018 %>% select(hh_id, c1_exist) %>% rename(roster_N.18=c1_exist)
n_roster_22 <- rmtl_srvy22 %>% select(hh_id,r1) %>% rename(roster_N.22=r1)

HHn_adults_22 <- rmtl_srvy22 %>% select(hh_id,starts_with("r5_")) %>% 
  pivot_longer(-hh_id,names_to = "name",values_to = "age") %>%     
  mutate(age=as.numeric(age)) %>% filter(age>17) %>% 
  mutate(name = str_replace(name, "^r5_hh_age", "r"))


# migrants_work_seasonal [new]
# [RC3]	new Q	Where do they work most of the time in the last 2 years?
# NO  #1 Within the Village #2 In a different Village #3	In town/city in same district
# YES #4	In town/city in different district # 5Different State
#
rc3 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("rc3")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "left") %>% 
  mutate(left =ifelse(left %in% c(4,5),1,0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_seasonal_22=sum(left))  


# migrants_work_permanent [new]
# [R32]	How many people, who previously lived in the household in the past 10 years now live elsewhere?
# [R39]		Why did they move away?	#1	For work #2	Married out #3	For Study
#
r39 <- 
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("r39"), -contains("other")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "why_left") %>% 
  mutate(why_left = ifelse(!is.na(why_left) & why_left == 1, 1, 0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_permanent_22=sum(why_left))

# migrants_work [new]
#
migrants_work_22 <- left_join(rc3,r39)



### r3_ What is the household member status ? [old]
attr(rmtl_srvy22$r3_1, "labels")
rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("r3_"))

### r26_ Since 2016, has [member's name] migrated from the village for work for a period of 6 months or more?
migration_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("r26_")) %>% 
  pivot_longer(-hh_id,
               names_to = "mmbr", values_to = "migrated") %>% 
  filter(!is.na(migrated)) %>% 
  group_by(hh_id) %>% 
  summarise(migrated22=sum(migrated)) %>% 
  left_join(n_roster_bl ) %>% 
  mutate(migrated_22_pct= 
      ifelse(migrated22==0 & roster_N.bl==0,0,migrated22/roster_N.bl)) %>% 
  mutate(migrated_22_01=ifelse(migrated_22_pct==0,0,1) ) %>% 
  select(hh_id,migrated_22_pct,migrated_22_01 )

migration_22 %>% left_join(hh_2022) %>% 
  group_by(farmers_hh) %>% 
  summarise(mean(migrated_22_01)*100, 
            mean(migrated_22_pct)*100)

### migration BL 2015 ### [NEW] 

# [   ] No. of migrants_work_seasonal
# [C15]	Where do they work during the rainy season? 1-5
# [C21]	Where do they work during the rest of the year? 1-5

C15 <- 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C15")) %>% 
  pivot_longer(-hh_id,names_to = "name",values_to = "val") %>% 
  mutate(val=ifelse(val %in% c(4,5),1,0)) %>% 
  group_by(hh_id) %>% summarise(Migrants_work_rainy =sum(val)) 

C21 <- 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C21")) %>% 
  pivot_longer(-hh_id,names_to = "name",values_to = "val") %>% 
  mutate(val=ifelse(val %in% c(4,5),1,0)) %>% 
  group_by(hh_id) %>% summarise(Migrants_work_restYr =sum(val)) 


# [   ] No. of migrants_work_permanent
# [C27] How many people, who previously lived in the household in the past 10 years now live elsewhere?
# [C34]	Why did they move away?	1	For work |2	Married out |3	For Study
#
c34 <- 
  rmtl_baseline2016 %>% 
  select(hh_id, starts_with("C34")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "why_left") %>%
  mutate(why_left = ifelse(!is.na(why_left) & why_left == 1, 1, 0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_permanent_15=sum(why_left))

# migrants_work [new]
#
migrants_work_BL <- left_join(c34,C21) %>% 
  rename(migrants_work_seasonal_15 = Migrants_work_restYr )



### migration BL 2015 ### [old]
# C1	How many household members live in this house?
# C27	How many people, who previously lived in the household in the past 10 years now live elsewhere?
# C18_	Where do they reside most of the time during the rest of the year?

migration_bl <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("C23"))  %>% 
  pivot_longer(-hh_id,names_to = "mmbr", values_to = "migrated") %>% 
  group_by(hh_id) %>% summarise(migrated=sum(migrated,na.rm = T)) %>% 
  left_join(n_roster_bl ) %>% 
  mutate(migrated_BL_pct= ifelse(roster_N.bl==0,0,migrated/roster_N.bl)) %>% 
  mutate(migrated_BL_01=ifelse(migrated_BL_pct==0,0,1) )%>% 
  select(hh_id,migrated_BL_pct,migrated_BL_01 )
________________________



# DESC STST ----
migration_NEW =
  migrants_work_22 %>% 
  left_join(migrants_work_BL) %>%
  left_join(rmtl_cntrl_vars) 

migration = # [OLD]
  migration_22 %>% 
  left_join(migration_bl) %>%
  left_join(rmtl_cntrl_vars) 

migration_NEW %>% 
  group_by(in_project) %>% 
  summarise(mean( migrants_work_seasonal_15),
            mean( migrants_work_permanent_15))

public_assistance official_assistance 
# REG ----
m1 <-  lm(migrants_work_permanent_22  ~ in_project +
            dist_Km_boundary + migrants_work_permanent_15 +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            job_income_sourceS + govPnsin_scheme + rent_property+
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge, 
          migration_NEW)
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F )

m2 <-  lm(migrated_prc_22  ~ in_project, migration)
summary(m2)

m3 <-  lm(migrated_prc_18  ~ in_project +
            dist_Km_boundary + migrated_prc_15 +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            job_income_sourceS + govPnsin_scheme + rent_property+
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge, 
          migration)
sjPlot::tab_model(m3 ,  show.se = T,digits = 5, show.stat  = F )

m4 <-  lm(migrated_prc_18  ~ in_project, migration)
summary(m4)




# PLOT ---- 
fml1 <- migrated_prc_22  ~ in_project +dist_Km_boundary + migrated_prc_15 +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
  total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + Thresher + Seed_drill + Motorcycle + Fridge

fml3 <- migrated_prc_18  ~ in_project +dist_Km_boundary + migrated_prc_15 +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
  total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + Thresher + Seed_drill + Motorcycle + Fridge

fits <- migration %>% nest() %>%
  mutate(model = map(data, ~ lm(fml1 #fml3
                                , data = .x)),
         coefs = map(model, tidy),
         stats = map(model, glance))

models_list <- 
  fits %>% { setNames(.$model, .$status) } 
#           ignore Warning message

m1_plot <- plot_summs(models_list ,coefs = c("In Project" = "in_project"),
           model.names = names(models_list),
           inner_ci_level = NULL, point.shape = F) + 
  labs(title = "Fraction members of a HH who migrates",
    x = "% of imigration (per HH)", y = NULL) +
  xlim(-0.05, 0.15) 
m1_plot + mp_theme



#__________________________ social level df   __________________________ ----

# RC1	What are their main income activities most of the time in the last 2 years?
# RC2	Where do they reside most of the time in the last 2 years?
# RC3	Where do they work most of the time in the last 2 years?
rmtl_srvy22$rc1_1
rmtl_srvy22$rc2_1
rmtl_srvy22$rc3_1
# rc1_1_1  = Question _ Answer _ HH member

###
# RC1	What are their main income activities most of the time in the last 2 years?
rc1 <- 
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("rc1_")) %>%   
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "rc", values_to = "val") %>% 
  separate(rc, into = c("rc", "ans", "hhm"), sep = "_", convert = F)%>% 
  filter(!is.na(val)) %>% 
  select(hh_id,hhm, ans,val)

total_hh_members <- 
  rc1 %>% select(hh_id,hhm) %>% distinct() %>% count(hh_id)

df_work_extra_22 <- 
  rc1 %>% mutate(val= ifelse(ans %in% c(9:12) & val==1,1,0 )) %>% 
  select(hh_id,hhm,val) %>% distinct() %>% 
  group_by(hh_id,hhm) %>% summarise(val = sum(val)) %>% 
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(extra_work_pct_22=val/n,
         extra_work_freq_22=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,extra_work_pct_22,extra_work_freq_22 )
# df_work_extra
# df_work_extra %>% filter(hh_id == 100071)


### BL
c13 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("C17"))  %>% 
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "c13", values_to = "ans") %>% 
  separate(c13, into = c("c13", "rank", "hhm"), sep = "_", convert = F
           )

c17 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("C17"))  %>% 
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "c17", values_to = "ans") %>% 
  separate(c17, into = c("c17", "rank", "hhm"), sep = "_", convert = F
  )

df_work_extra_bl <- 
  rbind(c13 %>% select(-c13) ,
        c17 %>% select(-c17)) %>% 
  select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(extra_work_pct_bl=val/n,
         extra_work_freq_bl=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,extra_work_pct_bl,extra_work_freq_bl )


# Gander : woman at work
# df_work_gender_22
df_gender_roster <- 
  rmtl_srvy22 %>% 
  select(hh_id,starts_with("r4_")) %>% 
  pivot_longer(-hh_id, names_to = "rc", values_to = "gender") %>% 
  separate(rc, into = c("rc", "hhm"), sep = "_hh_gender_", convert = F)%>% 
  filter(!is.na(gender)) %>% 
  select(hh_id,hhm,gender)

rc1 <- 
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("rc1_")) %>%   
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "rc", values_to = "val") %>% 
  separate(rc, into = c("rc", "ans", "hhm"), sep = "_", convert = F)%>% 
  filter(!is.na(val)) %>% 
  select(hh_id,hhm, ans,val)

df_work_gender_22 <- 
  rc1 %>% mutate(val= ifelse(ans %in% c(9:12) & val==1,1,0 )) %>% 
  select(hh_id,hhm,val) %>% distinct() %>% 
  group_by(hh_id,hhm) %>% summarise(val = sum(val)) %>% 
  left_join( df_gender_roster) %>% 
  group_by(hh_id,gender) %>% summarise(val = sum(val),n=n()) %>% 
  group_by( hh_id) %>% mutate(n=sum(n)) %>% ungroup() %>% 
  filter(gender ==2) %>% 
  mutate(
         woman_work_pct_22=val/n,
         woman_work_freq_22=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,woman_work_pct_22,woman_work_freq_22 )


# df_work_gender_bl

df_gender_rosterBL <- 
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with("c3_")) %>% 
  pivot_longer(-hh_id, names_to = "c3", values_to = "gender") %>% 
  separate(c3, into = c("c3", "hhm"), sep = "_", convert = F) %>% 
  filter(!is.na(gender)) %>% mutate(gender=ifelse(gender==0,2,gender)) %>% 
  select(hh_id,hhm,gender)

c17 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("C17"))  %>% 
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "c17", values_to = "ans") %>% 
  separate(c17, into = c("c17", "rank", "hhm"), sep = "_", convert = F
  )


df_work_gender_bl <- 
  c17 %>% 
  select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  group_by(hh_id,hhm) %>% summarise(val = sum(val)) %>% 
  left_join( df_gender_rosterBL) %>% 
  group_by(hh_id,gender) %>% summarise(val = sum(val),n=n()) %>% 
  group_by( hh_id) %>% mutate(n=sum(n)) %>% ungroup() %>% 
  filter(gender ==2) %>% 
  mutate(
    woman_work_pct_bl=val/n,
    woman_work_freq_bl=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,woman_work_pct_bl,woman_work_freq_bl )





migrants_work_22
migrants_work_BL




df_social_22 <- 
  literacy22 %>% rename(literacy_rates=literacy_22) %>% 
  left_join(edu_level_2nd_gen_22 %>% rename(edu_gen2=edu_gen2_22)
  ) %>%
  left_join(edu_private_22 %>% select(hh_id,Private_22) %>% rename(private_school=Private_22) 
            ) %>% 
  left_join( migration_NEW
             ) %>% 
  left_join(df_work_extra_22 %>% 
              rename(extra_work_pct=extra_work_pct_22 ,extra_work_freq= extra_work_freq_22)
            ) %>%
  left_join(df_work_gender_22 %>% 
              rename(woman_work_pct=woman_work_pct_22  ,woman_work_freq=woman_work_freq_22)
            ) %>% 
  pivot_longer(-hh_id,names_to = "social_vars", values_to = "val_22")
  
  
df_social_bl <- 
  literacy15 %>% rename(literacy_rates=literacy_15) %>% 
  left_join(edu_level_2nd_gen_15 %>% rename(edu_gen2=edu_gen2_15)
            ) %>% 
  left_join(edu_private_15 %>% select(hh_id,Private_15) %>% rename(private_school=Private_15) 
            ) %>% 
  left_join (
    migrants_work_BL
    ) %>% 
  left_join(
    df_work_extra_bl %>% rename(extra_work_pct=extra_work_pct_bl  ,extra_work_freq= extra_work_freq_bl)
    ) %>%
  left_join(df_work_gender_bl %>% 
              rename(woman_work_pct=woman_work_pct_bl ,woman_work_freq=woman_work_freq_bl)
            ) %>% 
  pivot_longer(-hh_id,names_to = "social_vars", values_to = "val_bl")
  

df_social <- 
  df_social_22 %>% 
  left_join(df_social_bl) %>% 
  left_join(hh_info) 

controls <- c(
  "dist_Km_boundary", "hh_haed_age", "hh_haed_gendar", "hh_haed_edu_level", 
  "total_acre16", "housing_str321", "job_income_sourceS", "govPnsin_scheme", 
  "rent_property", "livestock_dairy", "Bullock", "Tractor", "Plough", 
  "Thresher", "Seed_drill", "Motorcycle", "Fridge"
)


library(tidyverse)
library(broom)

fits <- df_social %>%  
  group_by(social_vars) %>%  
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "val_bl")
      fml <- as.formula(paste("val_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance) )

reg_table_social <- fits %>% 
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(social_vars, estimate, std.error, p.value) %>% 
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate = paste0(sprintf("%.3f", estimate), stars),
    std.error = paste0("(", sprintf("%.3f", std.error), ")"),
    p.value = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join(fits %>% unnest(stats) %>% select(social_vars, nobs, r.squared)) %>%
  left_join( # Join control means
    df_social %>% 
      filter(in_project == 0) %>% 
      group_by(social_vars) %>% 
      summarise(c_mean = mean(val_22,na.rm = T))
  )

plot_social <- fits %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project",
         !str_detect(social_vars, "freq$")
         ) %>%
  mutate( color_group = ifelse(social_vars=="migrated", "orange4",
                               ifelse(social_vars %in% c("extra_work_pct","woman_work_pct" ),"orange3","blue4"))
          ) %>% 
  mutate(social_vars = case_when(
    social_vars == "literacy_rates" ~ "Literacy \nrate",
    social_vars == "edu_gen2" ~ "Education \nlevel: \n2nd Gen ",
    social_vars == "private_school" ~ "Education: \nPrivate \nschool",
    social_vars == "migrated_pct" ~ "Work \nmigration",
    social_vars == "extra_work_pct" ~ "Off-farm \nwork",
    social_vars == "woman_work_pct" ~ "Off-farm \nwork. \nWomen")
    )

plot_social %>%  
  ggplot(aes(x = social_vars, y = estimate, color = color_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 1.5) +
  geom_point(size = 4) +
  scale_color_identity() +
  labs(title = 'Impact of "in_project" (Balanced Sample)', 
       y = "Change in Coefficient Estimate", x = "") +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 20, color = "gray25"),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14)
  )








RC1$farmers_hh <- factor(RC1$farmers_hh,
                        levels = c("outside_ramthal", "inside_ramthal"))


ggplot(RC1, aes( y = pct, x =ans , fill = factor(farmers_hh) )) +
  geom_col(position = "dodge", width = 0.6)+
  coord_flip()+
  scale_fill_manual(values = c("outside_ramthal" = "lightgray", "inside_ramthal" = "steelblue"),
                    labels = c("outside_ramthal" = "Out of Project", "inside_ramthal" = "In Project")) +
  
  
  
  scale_fill_manual(
    values = c("Out of Project" = "lightgray", "In Project" = "steelblue"),
    breaks = c("In Project", "Out of Project")) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "H3 most important factors to a determining agricultural success") + 
  theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.x =element_blank(),
    text=element_text(family="serif"),
    axis.text.x=element_text(size=10),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 14)
  )




### Following Qs for HH members who answer 9,10,11 and 12 
# RC2	Where do they reside most of the time in the last 2 years?
# RC3	Where do they work most of the time in the last 2 years?
rc23 <- 
  rmtl_srvy22 %>% 
  select(hh_id, starts_with(c("rc2_","rc3_"))) %>%   
  pivot_longer(-hh_id, names_to = "rc", 
               values_to = "ans") %>% 
  filter(!is.na(ans)) %>% 
  separate(rc, into = c("rc", "hhm"), sep = "_", convert = TRUE) %>% 
  select(hh_id,rc,hhm, ans) %>% 
  mutate(Ans = as_factor(ans))
  
  





#__________________________ education  __________________________       ----

# r7	Are they literate?
# r8	What is their educational level? (0=NOT literate)
# r11 	Is the institution public or private?
# r12		What are the annual tuition fees?
# The HH head is the same as in 2016, so there is no need to examine it

# literacy 22
# r7	Are they literate?

literacy22= 
  rmtl_srvy22 %>% select(hh_id,starts_with("r7" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literacy_22=mean(literate,na.rm=T)) %>% 
  mutate(literacy_22 = ifelse(literacy_22<0,NA,literacy_22)) # transfer NaN to NA
  
  

# literacy 15
# C6 Are they literate

literacy15= 
  rmtl_baseline2016 %>% select(hh_id,starts_with("c6" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literacy_15=mean(literate,na.rm=T)) %>% 
  mutate(literacy_15 = ifelse(literacy_15<0,NA,literacy_15))




# edu_level22
# r8	What is their educational level? 

edu_level22 = 
  rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  select(-contains("new")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level") %>% 
  group_by(hh_id) %>% 
  summarise(edu_level_22=mean(edu_level,na.rm=T)) %>% 
  mutate(edu_level_22 = ifelse(edu_level_22<0,NA,edu_level_22)) # transfer NaN to NA

# edu_level15
# C7 educational level

edu_level15 = 
  rmtl_baseline2016 %>% select(hh_id, starts_with("c7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level") %>% 
  group_by(hh_id) %>% 
  summarise(edu_level_15=mean(edu_level,na.rm=T)) %>% 
  mutate(edu_level_15 = ifelse(edu_level_15<0,NA,edu_level_15)) # transfer NaN to NA



# edu_level_2nd_gen_22
r8_22 =  
  rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  select(-contains("new")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
r8_22$id_member <- sub("^r8_(\\d{1,2})","r_\\1",r8_22$id_member )


r6_22 = # R6 What is their relationship to the head of household?
  rmtl_srvy22 %>% select(hh_id,starts_with("r6" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
r6_22$id_member <- sub("^r6_(\\d{1,2})","r_\\1",r6_22$id_member )

edu_level_2nd_gen_22 <- 
  left_join (r8_22,r6_22) %>% 
  mutate(edu_gen_2nd=ifelse(
    relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2_22=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2_22 = ifelse(edu_gen2_22<0,NA,edu_gen2_22)) # transfer NaN to NA


# edu_level_2nd_gen_15

# C7 educational level
# C5 elationship to the head of household

c7_15 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("c7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
c7_15$id_member <- sub("^C7_(\\d{1,2})","C_\\1",c7_15$id_member )

C5_15 = # C5 elationship to the head of household
  rmtl_baseline2016 %>% select(hh_id,starts_with("c5" ), -contains("os") ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
C5_15$id_member <- sub("^C5_(\\d{1,2})","C_\\1",C5_15$id_member )


edu_level_2nd_gen_15 <- 
  left_join (c7_15,C5_15) %>% 
  mutate(edu_gen_2nd=ifelse(
    relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2_15=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2_15 = ifelse(edu_gen2_15<0,NA,edu_gen2_15)) # transfer NaN to NA




# private school 2022
# R11 	Is the institution public or private?
edu_private_22 = 
  rmtl_srvy22 %>% select(hh_id,starts_with("r11" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(is.na (institut),0,institut),
         Private =ifelse(institut==2,1,0),
         public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_Private =sum(Private ), n_public=sum(public)) %>% 
  mutate(Private_22 = ifelse(n_Private  > 0, 1,
                  ifelse(n_public > 0, 0, NA)))


# private school 2015
# C10	Is the institution public or private?

edu_private_15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C10" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(!institut %in% c(1,2) ,0,institut),
         Private =ifelse(institut==2,1,0),
         public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_Private =sum(Private ), n_public=sum(public)) %>% 
  mutate(Private_15 = ifelse(n_Private  > 0, 1,
                         ifelse(n_public > 0, 0, NA)))
  
  
  
  
  
# tuition 2022
# R12		What are the annual tuition fees?
tuition22 =
  rmtl_srvy22 %>% select(hh_id,starts_with("r12" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "tuition") %>% 
  mutate(n_mm= ifelse(tuition >= 0, 1, NA )) %>% 
  group_by(hh_id) %>% 
  summarise(tuitionHH= sum(tuition,na.rm = T),n=sum(n_mm,na.rm = T)) %>% 
  mutate(tuition_hhm_22=tuitionHH/n) %>% 
  mutate(tuition_hhm_22 = ifelse(tuition_hhm_22<0,NA,tuition_hhm_22)) # transfer NaN to NA

# tuition 2015
# C11	What are the annual tuition fees?
  
tuition15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C11" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "tuition") %>% 
  mutate(tuition= ifelse(tuition < 0, NA, tuition )) %>% 
  mutate(n_mm= ifelse(tuition >= 0, 1, NA )) %>% 
  group_by(hh_id) %>% 
  summarise(tuitionHH= sum(tuition,na.rm = T),n=sum(n_mm,na.rm = T)) %>% 
  mutate(tuition_hhm_15=tuitionHH/n) %>% 
  mutate(tuition_hhm_15 = ifelse(tuition_hhm_15<0,NA,tuition_hhm_15)) # transfer NaN to NA



education1522 <- 
  literacy22 %>% left_join(literacy15) %>% 
  left_join(edu_level22) %>% left_join(edu_level15) %>% 
  left_join(edu_level_2nd_gen_22) %>% left_join(edu_level_2nd_gen_15) %>% 
  left_join(edu_private_22 %>% select(hh_id,Private_22)) %>% 
  left_join(edu_private_15 %>% select(hh_id,Private_15)) %>% 
  left_join(tuition22 %>% select(hh_id,tuition_hhm_22))%>% 
  left_join(tuition15 %>% select(hh_id,tuition_hhm_15))

education1522
library(writexl)
# write_xlsx(education1522, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/education1522.xlsx")

# Desc stat ----

education1522 %>% 
  select(hh_id,ends_with("22")) %>% 
  pivot_longer(-hh_id,names_to = "edu",values_to = "rate") %>% 
  left_join(rmtl_InOut %>% select(hh_id,in1_out0,drip_use,ir_use )) %>% 
  mutate(in_project= ifelse(in1_out0==1,"In Project","Out Project")) %>% 
  group_by(edu, in_project) %>% 
  summarise(rate= mean(rate,na.rm=T)) %>%
  mutate(
    rate =  ifelse (
      edu %in% c("literacy _22","Private _22"), rate * 100,
      ifelse(edu == "tuition_hhm_22", rate/1000,rate )
    ) ) %>% 
  mutate(edu = recode(edu,
                       "edu_gen2_22" = "2nd Gen\nEducation level\n(avg. per HH)",
                       "edu_level_22"  = "Education\nlevel\n(avg. per HH)",
                       "literacy_22" = "HH literacy \n(% per HH)" ,
                       "Private_22" = "Private  School\n(% per Group)",
                       "tuition_hhm_22" ="Tuition Fee\n(Avg. in K Rs.\nper HH member)")
         ) %>% 
  # filter(!edu %in% c("HH literacy \n(% per HH)", 
  #                    "Private  School\n(% per Group)") )%>% 
  ggplot(
    aes(x = edu, y = rate, fill = factor(in_project))) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("steelblue" ,"lightgray"))+
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Education 2022") + 
  theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.x =element_blank(),
    text=element_text(family="serif"),
    axis.text.x=element_text(size=12),
    legend.text = element_text(size = 12)
  )



# REG ----
library(stringr)
library(tidyr)
library(purrr)
library(broom)

education_imact <- 
  education1522 %>% 
  pivot_longer(-hh_id,names_to = "edu",values_to = "rate") %>%
  mutate(
    year = str_extract(edu, "(22|15)$"),     # extract suffix year
    edu = str_remove(edu, "_(22|15)$") # clean edu name (remove suffix)
  ) %>%
  mutate(
    rate= ifelse(edu == "tuition_hhm",rate/1000,rate),
    year= ifelse(year == "22", "edu_2022", "edu_BL")) %>% # create edu_2022 and edu_BL
  pivot_wider( names_from = year, values_from = rate
               ) %>%
  left_join(rmtl_InOut) %>%
  left_join(rmtl_cntrl_vars)

# (1) model formula inputs
fml <- edu_2022  ~ in_project +
            dist_Km_boundary + edu_BL +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            job_income_sourceS + govPnsin_scheme + rent_property+
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge

# (2) Nest by status and fit 
fits <- education_imact %>%
  group_by(edu) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# (3) Stacked outputs + (4) Join with model summary stats
inproj_with_fit <- 
  fits %>% unnest(coefs) %>% 
  filter(term == "in_project") %>% 
  select(edu, estimate, std.error, p.value) %>% ungroup() %>% 
  left_join(
    fits %>% unnest(stats) %>% select(edu,nobs, r.squared) 
    ) %>% 
  pivot_longer(-edu, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = edu, values_from = value )


# (5) control mean
control_mean <- education_imact %>% 
  filter(in_project==0) %>% 
  group_by(edu) %>% 
  summarise(Mean = mean(edu_2022,na.rm=T)) %>% 
  pivot_wider (names_from = "edu", values_from = "Mean") %>% 
  mutate(metric="control_mean") %>% 
  select(metric 
         ,literacy  ,edu_level ,edu_gen2 ,Private  ,tuition_hhm)

# (6) Yes No rows
YesNo_rows <- tribble(
  ~metric, ~literacy  ,~edu_level ,~edu_gen2 ,~Private  ,~tuition_hhm,
  "Control vars", "Yes","Yes", "Yes","Yes", "Yes",
  "Dist. boundary", "Yes","Yes", "Yes","Yes", "Yes",
  "Baseline value", "Yes","Yes", "Yes","Yes", "Yes")

# reg table ----
df_html <- inproj_with_fit %>%
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(YesNo_rows) %>% 
  rename_with(~ c("literacy  rate","Edu level", "Edu level 2nd gen", 
                  "Private  school (%)","Tuition fee"), .cols = 2:6)

library(kableExtra)
df_html [c(1:3,7:9,4:6),] %>% 
  kable("html", 
        caption = "education measurements",
        align = "c") %>%
  kable_classic( full_width = F) 


# PLOT ----------------------------------
library(jtools)

models_list1 <- fits %>% filter(edu != "tuition_hhm") %>% 
              { setNames(.$model, .$edu) }

models_list2 <- fits %>% filter(edu %in% c("Private", "tuition_hhm")) %>% 
              { setNames(.$model, .$edu) }


m1_plot1 <- plot_summs(
  models_list1 ,coefs = c("In Project" = "in_project"),
  model.names = c("Literacy", "Education level",
                  "2nd generation\nEducation level", 
                  "Private  school" ),
  inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Education rete", y = NULL) +
  xlim(-.2, 0.45) 
m1_plot1 + mp_theme

Tuition_color <- c( "white", "#E1A900") 
m1_plot2 <- plot_summs(
  models_list2 ,coefs = c("In Project" = "in_project"),
  model.names = c("", "Tuition fee       " ),
  inner_ci_level = NULL, point.shape = F,colors = Tuition_color ) + 
  labs(x = "Education rete", y = NULL) +
  xlim(-5, 2.5) 
m1_plot2 + mp_theme


#__________________________  SOCIAL CAPITAL __________________________       ----



social_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, farmers_hh, starts_with("h"))%>% 
  mutate(farmers_hh=ifelse(
    farmers_hh=="inside_ramthal","In Project",
    "Out of Project"))


attr(rmtl_srvy22$h1, "labels") # 1=dissatisfied|2|3|4|5=Satisfied
attr(rmtl_srvy22$h2, "labels") # 1=important|2|3|4=Not

#++ H1 +++++++++++++++++++++++++++++++++++++++++++++++++++|

H1 <- # 1- 5
  social_22 %>% 
  select(hh_id, farmers_hh,h1 ) %>% 
  group_by(farmers_hh) %>% 
  summarise(h1=mean(h1, na.rm = T) ) 

# inside on top after flipping
H1$farmers_hh <- factor(H1$farmers_hh,
                        levels = c("Out of Project", "In Project"))

ggplot(H1, aes(x = farmers_hh, y = h1, fill = farmers_hh)) +
  geom_col() +
  geom_text(aes(label = round(h1,2)),hjust = 1.5,color = "black",size = 4) +
  scale_fill_manual(values = c("Out of Project" = "lightgray", "In Project" = "steelblue"))+
  coord_flip() +
  labs(x = "", y = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    panel.grid = element_blank()
  )

#++ H2 +++++++++++++++++++++++++++++++++++++++++++++++++++|

H2 <- 
  social_22 %>% 
  select(hh_id, farmers_hh,h2 ) %>% 
  count(farmers_hh,h2) %>% 
  group_by(farmers_hh) %>% mutate(N=sum(n)) %>% 
  mutate(pct = n/N * 100) %>% 
  mutate(Rank = case_when(
    h2 == 1 ~ "Extremely important", 
    h2 == 2 ~ "Important",
    h2 == 3 ~ "Somewhat important", 
    h2 == 4 ~ "Not that important"
  )) 



library(ggplot2)
library(dplyr)

# Order categories in the SAME order as legend (left → right)
H2$Rank <- factor(H2$Rank,
                  levels = c("Extremely important","Important",
                             "Somewhat important","Not that important"))

# inside on top after flipping
H2$farmers_hh <- factor(H2$farmers_hh,
                        levels = c("Out Project", "In Project"))

ggplot(H2, aes(fill = Rank, y = pct, x = farmers_hh)) +
  geom_col(position = position_stack(reverse = T)) +
  geom_text(aes(label = round(pct)),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "black",size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Extremely important"  = "#d2a940",
    "Important"            = "#e4cc87",
    "Somewhat important"   = "#86a9c0",
    "Not that important"   = "#507088"
  )) +
  labs(x = "", y = "% of respondents", fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 11) ,
    axis.text.y = element_text(size = 14),
    panel.grid = element_blank()
  )


#++ H3 +++++++++++++++++++++++++++++++++++++++++++++++++++|

# 	From the following list, rank the top 3 most important factors 
# H3 to a determining agricultural success

# H3
H3 =
  social_22 %>% 
  select(hh_id, farmers_hh, starts_with("h3_"),-h3__777 ) %>% 
  pivot_longer(-c(hh_id ,farmers_hh) ,
               names_to = "ans", values_to = "n") %>% 
  group_by(farmers_hh,ans) %>% 
  summarise(pct =mean(n),.groups = "drop" ) %>% 
  mutate(Ans = recode(ans,
                      "h3_1" = "Skill/knowledge",
                      "h3_2" = "Access to Gov. schemes",
                      "h3_3" = "Ability to get loans" , # Ability to get loans (credit)
                      "h3_4" = "Having money to invest", # Having money to invest in inputs and machinery"
                      "h3_5" = "Political connections",
                      "h3_6" = "Luck/Good fortune", # Good fortune/luck/divine grace
                      "h3_7" = "Hard work")
  ) 

H3$Ans <- factor(H3$Ans ,levels = c("Political connections","Ability to get loans" ,"Having money to invest","Luck/Good fortune","Access to Gov. schemes","Hard work","Skill/knowledge"))

H3$farmers_hh <- factor(H3$farmers_hh, levels = c("Out of Project", "In Project"))

ggplot(H3, aes( y = pct, x =Ans , fill = factor(farmers_hh) )) +
  geom_col(position = "dodge", width = 0.6)+
  coord_flip()+
  scale_fill_manual(
    values = c("Out of Project" = "lightgray", "In Project" = "steelblue"),
    breaks = c("In Project", "Out of Project")) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "H3 most important factors to a determining agricultural success") + 
  theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.x =element_blank(),
    text=element_text(family="serif"),
    axis.text.x=element_text(size=10),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 14)
  )

#++ H4  +++++++++++++++++++++++++++++++++++++++++++++++++++|

# 	From the following list, rank the top 3 most important factors 
# H4 determining whether a farmer will have water for irrigation

# 	From the following list, rank the top 3 most important factors 
# H3 factors to a determining agricultural success
# H4 factors determining whether a farmer will have water for irrigation

H4 =
  social_22 %>% 
  select(hh_id, farmers_hh, starts_with("h4_"),-h4__777 ) %>% 
  pivot_longer(-c(hh_id ,farmers_hh) ,
               names_to = "ans", values_to = "n") %>% 
  group_by(farmers_hh,ans) %>% 
  summarise(pct =mean(n),.groups = "drop" ) %>% 
  mutate(Ans = recode(ans,
                      "h4_1" = "Skill/knowledge",
                      "h4_2" = "Access to Gov. schemes",
                      "h4_3" = "Ability to get loans" , # Ability to get loans (credit)
                      "h4_4" = "Having money to invest", # Having money to invest in inputs and machinery"
                      "h4_5" = "Political connections",
                      "h4_6" = "Luck/Good fortune", # Good fortune/luck/divine grace
                      "h4_7" = "Hard work")
  ) 
H4$Ans <- factor(H4$Ans ,levels = c("Political connections",
                                    "Ability to get loans" ,
                                    "Having money to invest",
                                    "Luck/Good fortune",
                                    "Access to Gov. schemes",
                                    "Hard work",
                                    "Skill/knowledge"))


ggplot(H4, aes( y = pct, x =Ans , fill = factor(farmers_hh) )) +
  geom_col(position = "dodge", width = 0.6)+
  coord_flip()




#++ H18_H23  ++++++++++++++++++++++++++++++++++++++++++++++|

H18_H23 <- 
  social_22 %>% 
  select(hh_id, farmers_hh,h18:h23 ) %>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-hh_id) %>% 
  pivot_longer(!farmers_hh, names_to = "POV",
               values_to = "value") %>% 
  mutate(Value=value/2)


# reorder POV so it plots top-to-bottom correctly
H18_H23$POV <- factor(H18_H23$POV, levels = rev(unique(df_long$POV)))

p_in <- 
  H18_H23 %>% filter(farmers_hh == "In Project") %>%
  ggplot(aes(x = Value, y = POV, color = farmers_hh)) +
  geom_point(size = 4) +
  geom_text(aes(label = round(Value, 2)), hjust = -0.5, size = 4) +
  scale_color_manual(values = c("steelblue3")) +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(
    text = element_text(family = "serif"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  ) + xlim(1.8,4.5)

p_out <- 
  H18_H23 %>% filter(farmers_hh!= "In Project") %>%
  ggplot(aes(x = Value, y = POV, color = farmers_hh)) +
  geom_point(size = 4) +
  geom_text(aes(label = round(Value, 2)), hjust = 1.5, size = 4) +
  scale_color_manual(values = c( "gray55")) +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(
    text = element_text(family = "serif"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )+ xlim(1.8,4.5)


ggsave("views_plot.png", p, bg = "transparent", 
       width = 14, height = 6)




#__________________________ __________________________       ----


# INCOME                                   ----

vars_f01 <- rmtl_srvy22 %>% select(farmers_hh, hh_id,starts_with("f")) 
names(vars_f01)
names(vars_f01) <- c("farmers_hh","hh_id",
                     "Migrating_household_income_hh",
                     "Migrating_household_income_amt",
                     "Remittance_hh","Remittance_amt",
                     "revenue12_amt",
                     "lease_land_hh","lease_land_amt",
                     "Own_livestock_hh","Own_livestock_net_profit",
                     "non_agri_business_hh","non_agri_business_net_profit",
                     "Salaried_job_hh","Salaried_job_amt",
                     "Casual_work_hh","Casual_work_amt",
                     "Gov_pension_scheme_hh","Gov_pension_scheme_amt",
                     "Rent_property_hh","Rent_property_amt",
                     "Other_activities_hh","Other_activities_amt",
                     "income_2021","sum_income12months" 
) 
vars_f02=vars_f01
vars_f02[is.na(vars_f02)] <- 0
vars_f02[vars_f02==-999] <- NA

vars_f02$Migrating_household_income_amt=ifelse(vars_f02$Migrating_household_income_amt==6,NA,vars_f02$Migrating_household_income_amt)

vars_f02$Remittance_amt=ifelse(vars_f02$Remittance_amt<1999,NA,vars_f02$Remittance_amt)

vars_f02$sum_income12months=ifelse(vars_f02$sum_income12months<9999,NA,vars_f02$sum_income12months)

income_21_22 = 
  vars_f02 %>% 
  mutate(
    income_2022_withoutRevenue=sum_income12months-revenue12_amt,
    income_2022_withRevenue=sum_income12months,
    total_income_sources_2022=Migrating_household_income_hh+
                        Remittance_hh+lease_land_hh+ #Own_livestock_hh+
                        non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Gov_pension_scheme_hh+
                        Rent_property_hh+Other_activities_hh,
    external_income=Migrating_household_income_amt+ Remittance_amt+Gov_pension_scheme_amt ) %>% 
  mutate(external_income=ifelse( is.na(external_income),0,external_income))

rm(vars_f01,vars_f02)


# F13	What is the annual household income in 2021? (Rs.)
# F12	Total income NOT A QUESTION Software will sum F1-F11

income_21_22 %>% group_by(farmers_hh) %>% summarise( mean(income_2021,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withoutRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(total_income_sources_2022,na.rm = T))

t01 <- income_21_22 %>% t_test(income_2022_withoutRevenue ~ farmers_hh , detailed = T) 
t02 <- income_21_22 %>% t_test(income_2022_withRevenue ~ farmers_hh , detailed = T) 
t03 <- income_21_22 %>% t_test(income_2021 ~ farmers_hh , detailed = T) 
t04 <- income_21_22 %>% t_test(total_income_sources_2022 ~ farmers_hh , detailed = T) 

t_F <- rbind(t01,t02,t03,t04) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

t_F$.y.[t_F$.y.=="income_2022_withoutRevenue"] <- "2022 without crop revenue"
t_F$.y.[t_F$.y.=="income_2022_withRevenue"] <- "2022 with crop Revenue"
t_F$.y.[t_F$.y.=="income_2021"] <- "2021 with crop Revenue"
t_F$.y.[t_F$.y.=="total_income_sources_2022"] <- "total income sources 2022 "

nice_table(t_F,title = c("Table F | Income ಆದಾಯ   " ,"Household income from the farm and from sources other than the farm" ),
           note = c("", "income_2021| F13	What is the annual household income in 2021? (Rs.)",
                    "income_2022_withoutRevenue| F13 minus F3(revenue12_amt)",
                    "income_2022_withRevenue| F12	Total income NOT A QUESTION Software will sum F1-F11",
                    "num_income_sources_2022| count F1-F11","🟨" ))

# External income
library(dplyr)

income_21_22 %>%
  select(hh_id, farmers_hh, Migrating_household_income_hh, Remittance_hh, Gov_pension_scheme_hh) %>%
  group_by(farmers_hh) %>%
  summarise(
    n = n(),
    migration = sprintf("%.1f%%", sum(Migrating_household_income_hh) / n * 100),
    remittances = sprintf("%.1f%%", sum(Remittance_hh) / n * 100),
    pensions_or_subsidie = sprintf("%.1f%%", sum(Gov_pension_scheme_hh) / n * 100)
  ) %>% kbl() %>% kable_material()




summary(income_21_22$Gov_pension_scheme_amt)


t05 <- income_21_22 %>% t_test(external_income ~ farmers_hh , detailed = T) 
t06 <- income_21_22 %>% t_test(Gov_pension_scheme_amt ~ farmers_hh , detailed = T) 

t_F129 <- rbind(t05,t06) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p)
t_F129$.y.[t_F129$.y.=="external_income"] <- "External income"
t_F129$.y.[t_F129$.y.=="Gov_pension_scheme_amt"] <- "Pension/subsidies [Gov/NGO]"

nice_table(t_F129,title = c("Table F1.2.9 | External income" ,"family assistance, Pensions and subsidies - Government/Non-Government" ),
           note = c("", "[F1]Income sent by seasonal migrating household members" , 
                    "[F2]Remittances (from permanent migrants)" , 
                    "[F9]Government pension or scheme" ,"🟨" ))



######################    essantials    ----

# Function to compute summary statistics
compute_summary <- function(x) {
  c(
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    Q25 = quantile(x, 0.25),
    Q75 = quantile(x, 0.75),
    P9= quantile(x, 0.9),
    P95= quantile(x, 0.95),
    Min = min(x),
    Max = max(x)
  )
}
compute_summary_1_99 <- function(x) {
  c(
    Count = n(),
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    P1 = quantile(x, 0.01),
    P99 = quantile(x, 0.99),
    Min = min(x),
    Max = max(x)
  )
}

# group by as variable
group_var <- yield22_acre$your_grouping_variable

# Apply the function to each group
summary_stats <- tapply(yield22_acre$kg_per_acre, group_var, compute_summary)

# Convert the result to a data frame
summary_df <- as.data.frame(do.call(rbind, summary_stats))


attr(a_rmtl_srvy22$l7_rank_3, "labels")

flat_vector <- unlist(L48[,-1], use.names = FALSE)
table(flat_vector, useNA = "always") 

# remove columns only NA or empty
select(where(~!all(.x=="")))
select(where(~!all(is.na(.x))))

# write.csv
write.csv(rmtl_In_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_In_groups.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv", row.names=FALSE)





















