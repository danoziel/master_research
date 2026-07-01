
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

# balance test - t-test ----
tbl_t1 <- 
  df_tableBbl  %>%
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project,cardinal_direction)
            ) %>% 
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in_project, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars, estimate2,estimate1, p.value) %>% 
  rename(In = estimate2 ,Out = estimate1) 

tbl_t2 <- 
  df_tableBbl  %>%
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project,cardinal_direction)
  ) %>% filter(cardinal_direction=="south") %>% 
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in_project, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars, estimate2,estimate1, p.value) %>% 
  rename(In_south = estimate2 ,Out_south = estimate1, p_south=p.value ) 

tbl_t1 %>% full_join(tbl_t2) %>% 
  kbl() %>% kable_paper()

tbl_t1 %>% filter(p.value<0.12) %>% 
  full_join(tbl_t2 %>% filter(p_south<0.12))


#
# PLOT bin Km   Xaxis as boundry ----
#  > bin Km
bin <- 0.25
bin_unit <- "Km"
df_land_bins <- df_220626 %>% filter(!is.na(dist_Km_boundary)) %>%
  mutate(dist_bin_numeric = floor(dist_Km_boundary / bin) * bin) %>%
  mutate(dist_bin = paste0(dist_bin_numeric, " : ", dist_bin_numeric + bin)) %>%
  summarise(
    mean_dist = mean(dist_Km_boundary),mean_val = mean(val_22, na.rm = T),
    in_project = first(in_project),n=n(),.by = c(dist_bin_numeric, dist_bin)
  ) %>%
  ungroup() %>%
  mutate(project_group = ifelse(in_project == 1, "In", "Out")) %>%
  arrange(dist_bin_numeric) %>% 
  mutate(dist_bin = factor(dist_bin, levels = unique(dist_bin)),
         pct=ifelse(in_project==1,n/923,n/646))
#  > bin PLOT    
ggplot(df_land_bins, aes(x = dist_bin, y = mean_val, fill = project_group)) +
  geom_col(color = "white") + scale_fill_manual(values = c("Out" = "gray60", "In" = "steelblue")) +
  labs(title = paste0("Bin=", bin, bin_unit ), fill="Groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# PLOT bin nHH  Xaxis as boundry ----
#  > bin No. of farmers
bin <- 40
bin_unit <- "farmers"
df_land_bins <- df_220626 %>%  filter(!is.na(dist_Km_boundary)) %>%
  arrange(dist_Km_boundary) %>%
  group_by(in_project) %>%
  mutate(bin_id = floor((row_number() - 1) / farmers_per_bin)) %>% ungroup() %>%
  group_by(in_project, bin_id) %>%
  summarise( min_dist = min(dist_Km_boundary), max_dist = max(dist_Km_boundary), mean_val = mean(val_22, na.rm = T),.groups = "drop") %>%
  mutate(dist_bin = paste0("[", round(min_dist, 2), " : ", round(max_dist, 2), "]")) %>%
  mutate(project_group = ifelse(in_project == 1, "In", "Out")) %>%
  arrange(min_dist) %>%
  mutate(dist_bin = factor(dist_bin, levels = unique(dist_bin)))

#  > bin PLOT    
ggplot(df_land_bins, aes(x = dist_bin, y = mean_val, fill = project_group)) +
  geom_col(color = "white") + scale_fill_manual(values = c("Out" = "gray60", "In" = "steelblue")) +
  labs(title = paste0("Bin=", bin, bin_unit ), fill="Groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#
#_______ DF crop season __ [df_bl_cropACRE]   ----

# sesasonaly 
df_bl_cropACRE <-
  crop_BL %>%  
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(acre_crop = sum(acre_crop, na.rm = T), .groups="drop") %>% 
  pivot_wider(
    names_from = season,
    values_from = acre_crop,
    values_fill = 0
  ) %>% 
  mutate(
    Rabi = pmax(rabi_2014_15, rabi_2015_16, na.rm = TRUE),
    Kharif = kharif_2015
  )%>%
  select(hh_id, crop_common, Kharif, Rabi) %>%
  pivot_longer(
    cols = c(Kharif, Rabi),
    names_to = "season",
    values_to = "acre_crop"
  ) %>% summarise(val_bl = sum(acre_crop,na.rm = T),.by = c(hh_id, crop_common)) %>% 
  complete(hh_id, crop_common, fill = list(val_bl = 0)) %>% 
  inner_join(hh_2022[,1:2])

df_bl_cropACRE%>% 
  group_by(crop_common  ) %>%
  do(tidy(t.test(val_bl ~ farmers_hh , data = .))) 

df_cropAcre_season <- 
  crop_BL %>%  
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(val_bl = sum(acre_crop, na.rm = T), .groups="drop")  %>% 
  mutate(vars = paste(crop_common, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0)) 

df_cropAcre_season %>% 
  inner_join(rmtl_16_18_22_sample[,c(1,5)]) %>% 
  group_by(vars) %>% t_test(val_bl ~ sample )%>% 
  filter(p <0.15)

df_cropAcre_season %>%
    group_by(vars  ) %>%
    do(tidy(t.test(val_bl ~ sample , data = .))) %>% 
    mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
    select(vars,estimate, estimate2,estimate1, p.value) %>% 
    rename(In = estimate2 ,Out = estimate1) %>% 
    filter(p.value <0.15)

#_______ DF croptype season __ [df_tableB]   ----

df_tableBbl <- crop_BL %>%   # in impact1.R 
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(acre_crop = sum(acre_crop, na.rm = T), .groups="drop") %>% 
  pivot_wider(
    names_from = season,
    values_from = acre_crop,
    values_fill = 0
  ) %>% 
  mutate(
    Rabi = pmax(rabi_2014_15, rabi_2015_16, na.rm = TRUE),
    Kharif = kharif_2015
  )%>%
  select(hh_id, crop_common, Kharif, Rabi) %>%
  pivot_longer(
    cols = c(Kharif, Rabi),
    names_to = "season",
    values_to = "acre_crop"
  )%>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  )%>% summarise(val_bl = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_"))  %>% 
  select(hh_id, vars, val_bl)%>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

crop_BL %>%   # in impact1.R 
  filter(season != "rabi_2015_16") %>% 
  mutate(season=ifelse(season=="kharif_2015","Kharif","Rabi")) %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop") %>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% summarise(val_bl = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_tableB_1 <- plots_crop_2022 %>%   # in DF.22.R 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif"))  %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop"
  )

toor_opposites <- df_tableB_1 %>%
  filter(crop_common == "Toor") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
jowar_opposites <- df_tableB_1 %>%
  filter(crop_common == "Sorghum_jowar") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
Bengalgram_opposites <- df_tableB_1 %>%
  filter(crop_common == "Bengal_gram") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
Sugarcane_opposites <- df_tableB_1 %>%
  filter(crop_common == "Sugarcane") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))

df_tableB <- 
  bind_rows(df_tableB_1, toor_opposites) %>% 
  bind_rows(jowar_opposites) %>%  bind_rows(Sugarcane_opposites) %>%  
  mutate(season = case_when(crop_common == "Greengram" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Bengal_gram" ~ "Rabi" ,T~ season )
  ) %>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% 
  summarise(val_22 = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_")) %>% 
  select(hh_id, vars, val_22) %>% 
  complete(hh_id, vars, fill = list(val_22 = 0))%>% 
  left_join(df_tableBbl)
#
df_cropACRE <-  bind_rows(df_tableB_1, toor_opposites) %>% 
  bind_rows(jowar_opposites) %>%  bind_rows(Sugarcane_opposites) %>%  
  mutate(season = case_when(crop_common == "Greengram" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Bengal_gram" ~ "Rabi" ,T~ season )
  ) %>% 
  summarise(val_22 = sum(acre_crop),.by = c(hh_id, crop_common)
  ) %>% 
  select(hh_id, crop_common, val_22) %>% 
  complete(hh_id, crop_common, fill = list(val_22 = 0)) %>% 
  left_join(df_bl_cropACRE) %>% rename(vars = crop_common) %>% select(-farmers_hh)
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ----
df_tableB <- plots_crop_2022 %>%   # in DF.22.R 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop_22 = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% 
  summarise(val_22 = sum(acre_crop_22,na.rm = T),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(
    vars = paste(crop_type, season, sep = "_")
  ) %>% 
  select(hh_id, vars, val_22) %>% 
  complete(hh_id, vars, fill = list(val_22 = 0)) %>% 
  left_join(df_tableBbl)
df_tableB$val_bl[is.na(df_tableB$val_bl)] <- 0
#
#_______ DF crop year ____ [df_tableC_]  ----

# annualy 2021-22

df_tableC_bl <- crop_BL %>%   # in impact1.R   filter(season != "kharif_2021") %>% 
  filter(season != "rabi_2015_16") %>% 
  mutate(season=ifelse(season=="kharif_2015","Kharif","Rabi")) %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop") %>%  
  mutate(vars = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% summarise(val_bl = sum(acre_crop),.by = c(hh_id, vars)
  ) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

df_tableC <- plots_crop_2022 %>%   # in DF.22.R 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop_22 = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(vars = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% 
  summarise(val_22 = sum(acre_crop_22,na.rm = T),.by = c(hh_id, vars)
  )%>% 
  select(hh_id, vars, val_22) %>% 
  complete(hh_id, vars, fill = list(val_22 = 0))%>% 
  left_join(df_tableC_bl)
df_tableC$val_bl[is.na(df_tableC$val_bl)] <- 0
#

#_______ DF crop adopt year ____ [df_tableD_]  ----

df_tableD <- df_tableC %>%
  mutate(
    vars = sub("acre", "adopted", vars),
    val_22 = +(val_22 != 0),
    val_bl = +(val_bl != 0)
  )
#


#_______ DF per crop _____  ----
df_crop_22=plots_crop_2022 %>%   # in DF.22.R 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(val_22 = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(vars = paste(crop_common, season, sep = "_")) %>% 
  select(hh_id, vars, val_22) %>% 
  complete(hh_id, vars, fill = list(val_22 = 0)) %>% left_join(rmtl_cntrl_vars) 

df_per_crop_acre_bl = crop_BL %>%   right_join (hh_2022) %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(val_bl = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(vars = paste(crop_common, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0)) 

df_per_crop_acre_bl %>% left_join (hh_2022) %>% 
  group_by(vars) %>% t_test(val_bl  ~ in1_out0) %>% as.data.frame()

df_Bengal_gram_bl <- df_per_crop_acre_bl %>% filter(vars %in% c("Bengal_gram_rabi_2014_15","Bengal_gram_rabi_2015_16" ) )
df_Toor_bl <- df_per_crop_acre_bl %>% filter(vars %in% c("Toor_kharif_2015","Toor_rabi_2014_15","Toor_rabi_2015_16" ) )


# _____________ANALYSIS  cultivated land__________ ----

df_tableD %>% count(vars)
df_tableB %>% count(vars)
df_tableC  %>% count(vars)

# mutate(val_22=ifelse(val_22 > 41, NA,val_22)) %>% mutate(val_bl=ifelse(val_bl > 16, NA,val_bl)) 

df_220626 <- df_tableCBD %>% filter(vars=="Rainfed_acre_Kharif") %>% 
  left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3000, NA,dist_Km_boundary)
  )
df_220626_1 <- df_tableCBD %>% filter(vars=="Rainfed_acre_Kharif") %>% 
  left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1600, NA,dist_Km_boundary)
  )%>% filter(cardinal_direction =="south")

mA <-  lm(val_22  ~ in_project, df_220626 )
mB <-  lm(val_22  ~ in_project +hh_haed_age+val_bl + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626 )
mC <-  lm(val_22  ~ in_project + dist_Km_boundary +val_bl+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626)

m1 <-  lm(val_22  ~ in_project, df_220626_1 )
m2 <-  lm(val_22  ~ in_project +hh_haed_age+val_bl + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626_1 )
m3 <-  lm(val_22  ~ in_project + dist_Km_boundary +val_bl+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626_1)

summary(mA)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(mB)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(mC)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]

summary(m1)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(m2)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(m3)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]

sjPlot::tab_model(mA,mB,mC ,m3,  terms = c("in_project","dist_Km_boundary"), show.se = F,digits = 3, show.stat  = F  ,show.ci = F)


df_220626 %>% t_test(val_22 ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)
df_220626 %>% t_test(val_bl ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)


df_tableD %>% count(vars)
df_tableB %>% count(vars)
df_tableC  %>% count(vars)
df_tableCBD  %>% count(vars)

df_tableCBD <- 
  df_tableC %>% 
  rbind(df_tableB) %>% 
  rbind(df_tableD) %>% 
  mutate(
    val_22=ifelse(vars=="WaterIntensive_acre" & val_22> 26 ,NA,val_22), # 15
    val_22=ifelse(vars=="Rainfed_acre" & val_22 > 50, NA,val_22), 
    # RABI
    val_22=ifelse(vars=="WaterIntensive_acre_Rabi" & val_22> 10 ,NA,val_22),
    val_22=ifelse(vars=="Rainfed_acre_Rabi" & val_22> 47 ,NA,val_22),
    # KHARIF
    val_22=ifelse(vars=="WaterIntensive_acre_Kharif" & val_22> 15 ,NA,val_22),
    val_22=ifelse(vars=="Rainfed_acre_Kharif" & val_22> 44 ,NA,val_22)
  ) %>% 
  mutate(  
    val_bl=ifelse(vars=="WaterIntensive_acre" & val_bl> 26, NA,val_bl),  # 18.1
    val_bl=ifelse(vars=="Rainfed_acre" & val_bl > 40, NA,val_bl),
    val_bl=ifelse(vars=="Rainfed_acre_Rabi" & val_bl >= 40, NA,val_bl),
    val_bl=ifelse(vars=="WaterIntensive_acre_Rabi" & val_bl> 22,NA,val_bl),
    val_bl=ifelse(vars=="WaterIntensive_acre_Kharif" & val_bl> 15,NA,val_bl),
    val_bl=ifelse(vars=="Rainfed_acre_Kharif" & val_bl> 16,NA,val_bl)
  ) 
#

#_______ DF assets _______ [df_assets]  ----


df_land_holding <- 
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(val_22=sum(acres,na.rm = T)) %>% 
  mutate(vars= "land_holding" ) %>% 
  select(hh_id, vars, val_22) %>% 
  left_join(rmtl_cntrl_vars) %>% rename(val_bl=total_acre16 ) %>% 
  select(hh_id, vars, val_22, val_bl) 

unique(df_assets$vars)

df_assets_22 <- 
  df_assets %>% 
  filter(vars %in% c("Farm_equipments_n", "Livestock_n","Vehicles_Home_1")
         ) %>% 
  rbind(df_land_holding)%>% 
  mutate(val_22=ifelse(vars=="land_holding" & val_22 > 39 ,NA,val_22)
         )
df_assets_22 %>% count(vars)

#_______df for fml ___________  ----

###########  cultivated land
df_tableCBD %>% count(vars)
df_reg_22 <- df_tableCBD %>% left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3000, NA,dist_Km_boundary)
  )
df_reg_22  %>% count(vars)
#

## df_cropACRE ----
df_reg_22 <- 
  df_cropACRE %>% left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3000, NA,dist_Km_boundary)
  )%>% 
  mutate(val_22=ifelse(vars=="Toor" & val_22 > 46 ,NA,val_22)
  )

df_reg_22  %>% count(vars)

df_reg_22 %>% count(vars)


###########  Total assets ----
df_assets_22 %>% count(vars)

df_assets_22_Agri <- df_assets_22 %>% filter(vars %in% c("land_holding", "Farm_equipments_n"  ))
df_assets_22_Economic  <- df_assets_22 %>% filter(vars %in% c("Livestock_n", "Vehicles_Home_1"  ))

df_assets_22_Agri %>% count(vars)
df_assets_22_Economic %>% count(vars)

########### inputs
df_inputs <- 
  df_inputs_type_22 %>% left_join(df_inputs_type_bl) %>% 
  mutate(
    val_bl=ifelse(vars =="input_mechanization" & val_bl > 3.9, NA,val_bl)
  )
df_inputs %>% count(vars)

########### labor_days
labor_days <- 
  family_labor_22 %>% left_join(labor_days_BL) %>% 
  mutate( val_bl=ifelse(vars=="labor_days_total" & val_bl > 2780 ,NA,val_bl))

labor_days %>% count(vars)

########### 2nd agricolture table reg outcome

df_assets_22_Agri %>% count(vars)
df_inputs %>% count(vars)
labor_days %>% count(vars)

df_reg_22 <- 
  df_assets_22_Agri %>% 
  rbind(df_inputs) %>% 
  rbind(labor_days) %>% 
  left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3000, NA,dist_Km_boundary)
  )
df_reg_22 %>% count(vars)

########### ### Economic  table reg outcome

df_return_invest %>% count(economic_vars)
df_assets_22_Economic %>% count(vars)
df_income_NonCrop%>% count(vars)

df_reg_22 <- 
  df_return_invest%>% rename(vars=economic_vars) %>% 
  filter(vars != "hh_acre") %>% 
  rbind(df_assets_22_Economic) %>% 
  rbind(df_income_NonCrop) %>%  filter(vars != "k_income_assistance_mhh") %>%
  left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3000, NA,dist_Km_boundary)
  )
df_reg_22 %>% count(vars)


# 1. reg model     ----

library(dplyr)
library(purrr)
library(broom)
library(tidyr)

fml_a <- val_22 ~ in_project
fml_b <- val_22 ~ in_project + val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge
fml_c <- val_22 ~ in_project + dist_Km_boundary +val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge
formulas <- list(model_a = fml_a, model_b = fml_b, model_c = fml_c)
#
# 2. control_mean  ----
control_mean <- df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(metric="control_mean") 
#
# [final_table] ________ Entire Sample __ [df_reg_22] ______________________ ----

final_table <- map_dfr(formulas, function(fml) {
  df_reg_22 %>% 
    group_by(vars) %>% 
    summarise(
      m = list(lm(fml, data = pick(everything()))),
      c = list(tidy(m[[1]]) %>% filter(term == "in_project") %>% select(estimate, std.error, p.value)),
      s = list(glance(m[[1]]) %>% select(nobs, r.squared)),
      .groups = "drop"
    ) %>% 
    unnest(c(c, s))
}, .id = "model_name") %>% 
  select(-m) %>% 
  pivot_longer(c(estimate, std.error, p.value, nobs, r.squared), names_to = "metric") %>% 
  pivot_wider(names_from = vars, values_from = value) 

df_reg_22  %>% count(vars)

# [final_table_south] __ South Sample ___ [df_reg_22_south] ________________ ----

df_reg_22_south <- df_reg_22 %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1600, NA,dist_Km_boundary)
  ) %>% filter(cardinal_direction =="south")

control_mean_south <- df_reg_22_south %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(metric="control_mean") 

final_table_south <- map_dfr(formulas, function(fml) {
  df_reg_22_south %>% 
    group_by(vars) %>% 
    summarise(
      m = list(lm(fml, data = pick(everything()))),
      c = list(tidy(m[[1]]) %>% filter(term == "in_project") %>% select(estimate, std.error, p.value)),
      s = list(glance(m[[1]]) %>% select(nobs, r.squared)),
      .groups = "drop"
    ) %>% 
    unnest(c(c, s))
}, .id = "model_name") %>% 
  select(-m) %>% 
  pivot_longer(c(estimate, std.error, p.value, nobs, r.squared), names_to = "metric") %>% 
  pivot_wider(names_from = vars, values_from = value) 

df_reg_22  %>% count(vars)


# Print crop acre per crop ________________________________________________  -----

library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  kbl(caption = "Entire sample") %>% kable_paper()%>%
  footnote(c(" "))

final_table_south %>% 
  rbind(control_mean_south %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>%  kbl(caption = "South") %>% kable_paper()



# Print Farming_income ________________________________________________  -----

library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,
               Farming_income,revenue_per_acre,
               k_income_assistance,k_income_independent,k_income_independent_mhh,
               Livestock_n, Vehicles_Home_1
  ) %>% 
  kbl(caption = "Entire sample") %>% kable_paper()%>%
  footnote(c(" "))

final_table_south %>% 
  rbind(control_mean_south %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,Farming_income,revenue_per_acre,k_income_assistance,k_income_independent,k_income_independent_mhh,Livestock_n, Vehicles_Home_1
  ) %>% kbl(caption = "South") %>% kable_paper()











# Print 2nd agri table [df_assets_22_Agri]___[df_inputs]____[labor_days]_____  -----
library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,
               land_holding,Farm_equipments_n,
               input_irrigation,input_mechanization,input_labor,
               labor_days_total,labor_days_perAcre
               ) %>% 
  kbl(caption = "Entire sample") %>% kable_paper()%>%
  footnote(c("Assets removed for significant in the baseline: Motorcycles from Vehicles_Home/ Tractor from Farm_equipments_n"))

final_table_south %>% 
  rbind(control_mean_south %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,land_holding,Farm_equipments_n,input_irrigation,input_mechanization,input_labor,labor_days_total,labor_days_perAcre
  ) %>% kbl(caption = "South") %>% kable_paper()





# Print table [cultivated land]_____________________________________________________________  -----

library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,
               WaterIntensive_acre,Rainfed_acre,
               WaterIntensive_acre_Rabi,Rainfed_acre_Rabi,
               WaterIntensive_acre_Kharif,Rainfed_acre_Kharif,
               WaterIntensive_adopted, Rainfed_adopted
               ) %>% 
  kbl(caption = "Entire sample") %>% kable_paper()%>%
  footnote(c(" "))

final_table_south %>% 
  rbind(control_mean_south %>% mutate(model_name="Model_A",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% select(model_name,metric,WaterIntensive_acre,Rainfed_acre,WaterIntensive_acre_Rabi,Rainfed_acre_Rabi,WaterIntensive_acre_Kharif,Rainfed_acre_Kharif,WaterIntensive_adopted, Rainfed_adopted
  ) %>% kbl(caption = "South") %>% kable_paper()











# REG Rainfed_acre_Kharif ------

