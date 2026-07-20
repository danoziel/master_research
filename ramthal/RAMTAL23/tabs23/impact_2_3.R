
#  [ 
library(readr)
a_plots_crop <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_crop.csv")

attr(rmtl_srvy22$l7_rank_3, "labels")


library(writexl)
# write_xlsx(education1522, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/education1522.xlsx")




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






#________________________ TABLE: agri-economy  ____________ ----

#__________  df_inputs     -----
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
         vars= "inputs_annual") %>% 
  select(hh_id, vars, val_22)

df_inputs_type_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, contains ("l70"),contains ("l71"),# contains ("l72"),
         contains ("l73")) %>% 
  pivot_longer(-hh_id,names_to = "observation",values_to = "inputs_Rs" ) %>% 
  separate(observation, into = c("inputs", "season"), sep = "_") %>% 
  filter(season != "KHA22") %>%
  mutate(inputs = case_match(inputs,
                             "l70" ~ "input_irrigation",
                             "l71" ~ "input_mechanization",
                             "l73" ~ "input_labor",
                             .default = inputs # שומר על ערכים אחרים אם ישנם
  )) %>%
  group_by(hh_id,inputs ) %>% 
  summarise(inputs_Rs = sum(inputs_Rs,na.rm = T),.groups = "drop"
  ) %>% 
  left_join(land_cult_2022) %>% 
  mutate(inputs_Rs = ifelse(inputs_Rs < 0, NA, inputs_Rs)) %>% 
  group_by(hh_id) %>%
  mutate(new_condition = sum(inputs_Rs, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(inputs_Rs = ifelse(new_condition == 0, NA, inputs_Rs))%>% 
  mutate(val_22 = inputs_Rs/cult_acre_22/1000) %>% 
  rename(vars=inputs ) %>% 
  select(hh_id, vars, val_22)

cult_acre_bl <- bl_crop_plot_3s %>% 
  filter(season != "rabi_2014_15") %>% 
  select(hh_id, plotID) %>% 
  left_join(BL_plotAcre) %>%
  summarise(acre = sum(plot_acre,na.rm = T),.by = hh_id )

df_inputs_bl <- 
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
  left_join( cult_acre_bl) %>% 
  mutate(val_bl= inputs_Rs/acre/1000, 
         val_bl = ifelse(val_bl==0, NA, val_bl),
         vars="inputs_annual") %>% 
  select(hh_id, vars, val_bl)

df_inputs_type_bl <- 
  rmtl_baseline2016 %>% 
  select(hh_id,D49_2015:D60_2014) %>% select(hh_id , contains("2015")) %>% 
  pivot_longer(-hh_id,names_to = "inputs", values_to = "inputs_Rs" ) %>% 
  separate(inputs, into = c("inputs", "year"), sep = "_") %>% 
  select(-year) %>% 
  filter(inputs %in% c( "D58", # "irriEquipment"
                        "D59", # "mechanization"
                        "D60")) %>%  # "labor"
  mutate(inputs = 
           case_match(inputs, "D58" ~ "input_irrigation",
                      "D59" ~ "input_mechanization",
                      "D60" ~ "input_labor",.default = inputs)
         ) %>%
  group_by(hh_id,inputs) %>% 
  summarise(inputs_Rs=sum(inputs_Rs,na.rm = T),.groups = "drop") %>% 
  left_join( cult_acre_bl) %>% mutate(acre=ifelse(acre==0,NA,acre)) %>% 
  group_by(hh_id) %>%
  mutate(new_condition = sum(inputs_Rs, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(inputs_Rs = ifelse(new_condition == 0, NA, inputs_Rs))%>% 
  mutate(val_bl = inputs_Rs/acre/1000) %>% 
  rename(vars=inputs ) %>% 
  select(hh_id, vars, val_bl)




df_inputs %>% right_join(hh_2022) %>%
  group_by(vars) %>%  t_test(val_bl ~ farmers_hh,detailed = T) 

df_inputs %>% group_by(vars) %>%
  summarise(p99 = quantile(val_22, probs = 0.99, na.rm = TRUE))




# _________ labor Mandays ----

land_cult_2022 <- 
  plots_crop_2022 %>% 
  select(hh_id, season, plotID, acres) %>% distinct() %>% 
  group_by(hh_id,season) %>% summarise(acres=sum(acres,na.rm = T),.groups = "drop") %>% 
  filter(season != "kharif_2022") %>% 
  group_by(hh_id) %>% summarise(cult_acre_22=sum(acres,na.rm = T),.groups = "drop")


library(haven)

# l76_prev_1_hh_1_1_1
# l76_[prev/new plot]_[season 1/2/3]_hh_[1 prev hh mmbr/2 new]_[mmbr num]_[plot num]
## season 1 = kha / season 1 = rab / season 1 = KHA22
L76 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("l76")) %>% 
  select(!where(is.character)) %>% 
  pivot_longer(cols = -hh_id,names_to = "season",values_to = "days_22")

family_labor_22 <-
  L76 %>% 
  filter(!is.na(days_22)) %>% 
  mutate(
    season = stringr::str_remove(season, "_hh.*"),
    season = stringr::str_replace(season, "l76_[a-zA-Z_]+(?=[0-9])", "season_")
  ) %>% # count(season)
  filter(season != "season_3") %>% 
  group_by(hh_id) %>% 
  summarise(labor_days_total = sum(days_22, na.rm = TRUE),.groups = "drop") %>%
  left_join(land_cult_2022) %>% 
  mutate(labor_days_perAcre = labor_days_total/cult_acre_22  ) %>% 
  select(-cult_acre_22) %>% 
  pivot_longer(cols = -hh_id,names_to = "vars",values_to = "val_22"
  )
family_labor_22 %>% left_join(hh_2022) %>% group_by(vars,farmers_hh) %>% summarise(mean(val_22,na.rm=T))

family_labor_22 %>% filter(vars!="labor_days_total") %>% 
  ggplot(aes(x = val_22)) + geom_histogram()

family_labor_22 %>% group_by(vars) %>% 
  summarise(n=n(),Q99=quantile(val_22, probs = 0.99, na.rm = TRUE))


# Amount used on plot - Labor	Mandays        
D24 <- rmtl_baseline2016 %>% 
  select(hh_id,starts_with("D43_uasge"),-ends_with("_0")) %>% 
  pivot_longer(cols = -hh_id,names_to = c("observation"),values_to = "val_bl") %>% 
  separate(observation, into = c("D" ,"pn","season", "plotID"), sep = "_") %>%
  filter(val_bl >= 0,season != "3" ) %>% 
  select(hh_id ,season ,plotID ,val_bl)

labor_days_BL <- # rr <- 
  D24 %>%   
  group_by(hh_id) %>%
  summarise(labor_days_total=sum(val_bl)) %>% 
  left_join(rmtl_baseline2016 %>% select(hh_id,D3) ) %>% 
  mutate(labor_days_total=labor_days_total*D3) %>% 
  left_join(
    crop_acre_BL %>%  # in impact1.R 
      group_by(hh_id) %>%
      summarise(total_cult_acre = sum(acre_crop_BL,na.rm = T)) %>%
      filter(total_cult_acre > 0)
  ) %>%
  mutate(labor_days_perAcre=labor_days_total / total_cult_acre )%>% 
  mutate(across(everything(), ~na_if(., 0))) %>% 
  select(hh_id ,labor_days_perAcre ,labor_days_total) %>% 
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_bl"
  ) 

d24_bl <- labor_days_BL %>% inner_join(hh_2022[,1:2])
d24_bl %>% inner_join(hh_2022) %>%  group_by(vars,farmers_hh) %>% summarise(meanBL=mean(val_bl,na.rm=T) ,n=n())
d24_bl %>% inner_join(hh_2022) %>%  group_by(vars) %>% summarise(Q99=quantile(val_bl, probs = 0.99, na.rm = TRUE))

d24_bl %>% filter(vars=="labor_days_total") %>% 
  ggplot(aes(x = val_bl)) + geom_histogram()

d24_bl  %>%
  mutate(val_bl=ifelse(vars=="labor_days_total" & val_bl > 2780 ,NA,val_bl)) %>% 
  group_by(vars) %>% 
  t_test(val_bl ~ farmers_hh, var.equal = F, detailed = T)

# DF for REG
labor_days <- 
  family_labor_22 %>% left_join(labor_days_BL) %>% 
  mutate( val_bl=ifelse(vars=="labor_days_total" & val_bl > 2780 ,NA,val_bl))



#__________________________  df_income_NonCrop __ ----

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

r1A_22 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with(c("r9_")), -ends_with("_bin")) %>% 
  pivot_longer(
    cols = !hh_id, 
    names_to = "id_member", 
    values_to = "at_school") %>% summarise(at_school=sum(at_school,na.rm = T),.by = hh_id) %>%   left_join( rmtl_srvy22 %>% select(hh_id, r1 )) %>% mutate(r1 = r1 - at_school) %>% select(hh_id, r1)

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
  mutate(r1 = ifelse(r1 < 2,2,r1)) %>% 
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
  mutate(C1=ifelse(C1<2,2,C1), # Total household members
         income_NonCrop_mhh=income_NonCrop/C1 # Normalized to number of household members
  ) %>% select(-C1)

d_I1b <- df_income_NonCrop_bl %>% select(-income_NonCrop_mhh) %>% rename(val_bl=income_NonCrop)
d_I2b <- df_income_NonCrop_bl %>% select(-income_NonCrop) %>% 
  mutate(income_type = paste0(income_type, "_mhh"))%>% rename(val_bl=income_NonCrop_mhh)

# op=
  d_I1 %>% left_join(d_I1b)  %>% 
  rbind( d_I2 %>% left_join(d_I2b) ) %>% left_join(hh_2022) %>% 
  group_by(income_type) %>% 
  summarise(y22_99 = quantile(val_22, probs = 0.99, na.rm = T),
            bl_99 = quantile(val_bl, probs = 0.99, na.rm = T))

  df_income_NonCrop <- 
    d_I1 %>% left_join(d_I1b) %>% 
    rbind( d_I2 %>% left_join(d_I2b) ) %>% 
    rename(vars=income_type) %>% 
    mutate(val_22 = case_when(
      vars == "k_income_assistance" & val_22 > 306 ~ NA_real_,
      vars == "k_income_assistance_mhh" & val_22 > 57 ~ NA_real_,
      
      vars == "k_income_independent" & val_22 > 505 ~ NA_real_,
      vars == "k_income_independent_mhh" & val_22 > 114 ~ NA_real_,
      TRUE ~ val_22 ))%>% 
    mutate(val_bl = case_when(
      vars == "k_income_assistance" & val_bl > 288 ~ NA_real_,
      vars == "k_income_assistance_mhh" & val_bl > 37 ~ NA_real_,
      
      vars == "k_income_independent" & val_bl > 404 ~ NA_real_,
      vars == "k_income_independent_mhh" & val_bl > 83 ~ NA_real_,
      TRUE ~ val_bl ))


#__________________________  df_assets  ________  ----

# How many of this item does the household currently own? (0 if none)

# LIVESTOCK # [E6Cows E7Bullock E8Buffaloes E9Goats&sheep]
# FARM EQUIPMENT # [E10Tractor E11Plough E12Thresher E13Seed drill E14JCB ] 
# VEHICLES # [E15Cycles E16Motorcycles E17Cars] 
# HH ITEMS # [E18Fridge # E19	Television]
rmtl_srvy22 %>% select(hh_id,starts_with("e"),-e21) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val") %>% 
  filter(val>0) %>% count(vars) %>% as.data.frame()
### REMOVE [E14 JCB]
### skip E20Gold E21Silver

tbl_t1 %>% filter(p.value<0.12) %>% 
  full_join(tbl_t2 %>% filter(p_south<0.12))

# assets_22 .........
df_tabE2022 <- rmtl_srvy22 %>% select(hh_id,starts_with("e"),-e21) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",       # LIVESTOCK
    "Tractor","Plough","Thresher","Seed_drill","JCB", # FARM EQUIPMENT 
    "Cycles","Motorcycles","Cars","Fridge","TV"       # VEHICLES # HH ITEMS
    ), .cols = 2:15) |> select(-Plough)

df_tabE22 <- df_tabE2022
df_tabE22[, 6:14] <- +(df_tabE22[, 6:14] >= 1)
# df_tabE22[, paste0(c("Tractor","Plough","Thresher","Seed_drill","JCB","Cycles","Motorcycles","Cars","Fridge","TV"), "01")] <-  +(df_tabE22[, c("Tractor","Plough","Thresher","Seed_drill","JCB","Cycles","Motorcycles","Cars","Fridge","TV")] >= 1)
df_tabE22$Livestock_n <- rowSums(df_tabE22[, c( "Cows","Bullock","Buffaloes","Goats_sheep")], na.rm = TRUE)
df_tabE22$Farm_equipments <- rowSums(df_tabE22[, c("Seed_drill","JCB", "Tractor", "Thresher")] > 0, na.rm = TRUE)
# df_tabE22$Vehicles_n <- rowSums(df_tabE22[, c("Cycles","Cars")], na.rm = TRUE) # -Motorcycles
# df_tabE22$Vehicles_1 <- rowSums(df_tabE22[, c("Cycles","Cars")] > 0, na.rm = TRUE) # -Motorcycles
# df_tabE22$Home_n <- rowSums(df_tabE22[, c("Fridge","TV")], na.rm = TRUE)
# df_tabE22$Home_1 <- rowSums(df_tabE22[, c("Fridge","TV")] > 0, na.rm = TRUE)
# df_tabE22$Vehicles_Home_n <- rowSums(df_tabE22[, c("Cycles","Cars","Fridge","TV")], na.rm = TRUE)# -Motorcycles
# df_tabE22$Vehicles_Home_1 <- rowSums(df_tabE22[, c("Cycles","Cars","Fridge","TV")] > 0, na.rm = TRUE)# -Motorcycles

df_assets22 <- df_tabE22 %>% 
  select(-c(Cows:Goats_sheep)) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val_22")

# assets_15 .........

df_tabE2016 <- 
  rmtl_baseline2016 %>% select(hh_id,starts_with("E")) %>% 
  select(hh_id, ends_with("_1"),-c(E20_1:E11_to_E13_1)) %>% 
  rename_with(~ c(
    "Cows","Bullock","Buffaloes","Goats_sheep",
    "Tractor","Plough","Thresher","Seed_drill","JCB",
    "Cycles","Motorcycles","Cars","Fridge","TV"), 
    .cols = 2:15)|> select(-Plough)

df_tabE16 <-  df_tabE2016
df_tabE16[, 6:14] <- +(df_tabE16[, 6:14] >= 1)
 
# df_tabE16[, paste0(c("Tractor","Plough","Thresher","Seed_drill","JCB","Cycles","Motorcycles","Cars","Fridge","TV"), "01")] <- +(df_tabE16[, c("Tractor","Plough","Thresher","Seed_drill","JCB","Cycles","Motorcycles","Cars","Fridge","TV")] >= 1)

df_tabE16$Livestock_n <- rowSums(df_tabE16[, c( "Cows","Bullock","Buffaloes","Goats_sheep")], na.rm = TRUE)
df_tabE16$Farm_equipments <- rowSums(df_tabE16[, c("Seed_drill","JCB","Tractor","Thresher")] > 0, na.rm = TRUE) 


# df_tabE16$Vehicles_n <- rowSums(df_tabE16[, c("Cycles","Cars")], na.rm = TRUE) # -Motorcycles
# df_tabE16$Vehicles_1 <- rowSums(df_tabE16[, c("Cycles","Cars")] > 0, na.rm = TRUE) # -Motorcycles
# df_tabE16$Home_n <- rowSums(df_tabE16[, c("Fridge","TV")], na.rm = TRUE)
# df_tabE16$Home_1 <- rowSums(df_tabE16[, c("Fridge","TV")] > 0, na.rm = TRUE)
# df_tabE16$Vehicles_Home_n <- rowSums(df_tabE16[, c("Cycles","Cars","Fridge","TV")], na.rm = TRUE) # -Motorcycles
df_tabE16$Vehicles_Home_1 <- rowSums(df_tabE16[, c("Cycles","Cars","Fridge","TV")] > 0, na.rm = TRUE) # -Motorcycles

df_assets15 <- df_tabE16 %>%  
  select(-c(Cows:Goats_sheep)) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val_bl")

df_assets15 %>% count(vars)

# assets 22 15 ........................................................
df_assets <- df_assets22 %>% inner_join(df_assets15)
#......................................................................

# Analysis ----

df_economic <- 
  df_income_NonCrop %>% 
  rbind(df_return_invest %>% rename(ecomony_vars=economic_vars)) %>% filter(ecomony_vars != "hh_acre") %>%
  # rbind(df_inputs) %>%
  rbind(df_assets) %>%
  left_join(rmtl_cntrl_vars) %>%   # select(in_project, dist_Km_boundary) %>% 
  mutate(
    dist_squared = dist_Km_boundary^2, # 1. המרחק בריבוע (עצמאי)
    interaction_linear = in_project * dist_Km_boundary, # 2. אינטראקציה ליניארית
    interaction_quadratic = in_project * dist_squared ,    # 3. אינטראקציה ריבועית
    elev = ifelse(in_project==0, elevation_m*-1,elevation_m) 
    )










# PLOT                  ----
library(jtools)
library(ggplot2)

#custom theme to format
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


#__________________________ SOCIAL  __________________________       ----

#___________ migrants_work_22  ___      ----

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
library(haven)
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
migrants_work_22 <- left_join(rc3,r39) %>% 
  rename(migrants_work_permanent= migrants_work_permanent_22,
         migrants_work_seasonal= migrants_work_seasonal_22)



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
  rename(migrants_work_permanent= migrants_work_permanent_15 ,
         migrants_work_seasonal = Migrants_work_restYr )



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




#______________ gendar migration   ____ ----

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

df_work_offFarm_22 <- 
  rc1 %>% mutate(val= ifelse(ans %in% c(9:12) & val==1,1,0 )) %>% 
  select(hh_id,hhm,val) %>% distinct() %>% 
  group_by(hh_id,hhm) %>% summarise(val = sum(val)) %>%  
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(offFarm_work_pct_22=val/n,
         offFarm_work_freq_22=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,offFarm_work_pct_22,offFarm_work_freq_22 )
# df_work_offFarm
# df_work_offFarm %>% filter(hh_id == 100071)


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

df_work_offFarm_bl <- 
  rbind(c13 %>% select(-c13) ,
        c17 %>% select(-c17)) %>% 
  select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(offFarm_work_pct_bl=val/n,
         offFarm_work_freq_bl=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,offFarm_work_pct_bl,offFarm_work_freq_bl )


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
         woman_offFarmwork_pct_22=val/n,
         woman_offFarmwork_freq_22=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,woman_offFarmwork_pct_22,woman_offFarmwork_freq_22 )


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



  df_work_gender_bl <- c17 %>% 
    select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  summarise(val = sum(val),.by=c(hh_id,hhm)) %>% 
  left_join( df_gender_rosterBL) %>% 
  summarise(val = sum(val),n=n(),.by = c(hh_id,gender)) %>% 
  group_by( hh_id) %>% mutate(n=sum(n)) %>% ungroup() %>% 
  filter(gender ==2) %>% 
  mutate(
    woman_offFarmwork_pct_bl=val/n,
    woman_offFarmwork_freq_bl=ifelse(val !=0, 1,0)) %>% 
  select(hh_id,woman_offFarmwork_pct_bl,woman_offFarmwork_freq_bl )



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
    mutate(literacy_22 = ifelse(literacy_22 <= 0,NA,literacy_22)) # transfer NaN to NA
  
  
  
  # literacy 15
  # C6 Are they literate
  
  literacy15= 
    rmtl_baseline2016 %>% select(hh_id,starts_with("c6" ) ) %>% 
    pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
    group_by(hh_id) %>% 
    summarise(literacy_15=mean(literate,na.rm=T)) %>% 
    mutate(literacy_15 = ifelse(literacy_15 <= 0,NA,literacy_15))
  
  
  
  
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
  
  
  
  


# DF [df_social] ----


df_social_22 <- 
  literacy22 %>% rename(literacy_rates=literacy_22) %>% 
  left_join(edu_level_2nd_gen_22 %>% rename(edu_gen2=edu_gen2_22)
  ) %>%
  left_join(edu_private_22 %>% select(hh_id,Private_22) %>% rename(private_school=Private_22) 
            ) %>% 
  left_join( migrants_work_22
             ) %>% 
  left_join(df_work_offFarm_22 %>% 
              rename(offFarm_work_pct=offFarm_work_pct_22 ,offFarm_work_freq= offFarm_work_freq_22)
            ) %>%
  left_join(df_work_gender_22 %>% 
              rename(woman_offFarmwork_pct=woman_offFarmwork_pct_22  ,woman_offFarmwork_freq=woman_offFarmwork_freq_22)
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
    df_work_offFarm_bl %>% rename(offFarm_work_pct=offFarm_work_pct_bl  ,offFarm_work_freq= offFarm_work_freq_bl)
    ) %>%
  left_join(df_work_gender_bl %>% 
              rename(woman_offFarmwork_pct=woman_offFarmwork_pct_bl ,woman_offFarmwork_freq=woman_offFarmwork_freq_bl)
            ) %>% 
  pivot_longer(-hh_id,names_to = "social_vars", values_to = "val_bl")
  

df_social <- 
  df_social_22 %>% 
  left_join(df_social_bl) %>% 
  filter(!social_vars %in% c("woman_offFarmwork_freq", "offFarm_work_freq")) %>% 
  left_join(rmtl_cntrl_vars
            )  # %>%  filter(cardinal_direction == "south" )
  



    







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
























