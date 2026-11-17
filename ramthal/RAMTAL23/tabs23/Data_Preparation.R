
# ==============================================================================
# RAMTHAL IRRIGATION PROJECT | Impact Evaluation
# Replication Package: Data Preparation
# ==============================================================================

# # Prepared Data Frames
# 1. Source Data
# 2. Primary Data Frames
# 3. Derived Data Frames
# 4. Analysis Data Frames

#...............................................................................
#
# SOURCE DATA: Required Input Files ----
#...............................................................................

# Datasets required to run the script, using the object names listed below.


# Baseline Survey (2016)
rmtl_baseline2016 <- haven::read_dta("https://example.org/Ramthal_Source_Data/rmtl_baseline2016.dta")

# Midline Survey (2018)
rmtl_midline2018 <- haven::read_dta("https://example.org/Ramthal_Source_Data/rmtl_midline2018.dta")

# Endline Survey (2022)
rmtl_srvy22 <- haven::read_dta("https://example.org/Ramthal_Source_Data/rmtl_srvy22.dta")

# Follow-up Survey # Farmer Interviews (2024)
rmtl_2024 <- readxl::read_excel("https://example.org/Ramthal_Source_Data/rmtl_2024.xlsx")
rmtl_interviews_june24 <- readxl::read_excel("https://example.org/Ramthal_Source_Data/rmtl_interviews_june24.xlsx")

# Crop List 
list_crop <- readxl::read_excel("https://example.org/Ramthal_Source_Data/list_crop.xlsx")

# Distance (Km) from the project boundar
dist_boundary_vars <- readxl::read_excel("https://example.org/Ramthal_Source_Data/list_crop.xlsx")

# Sampling of households based on a survey
rmtl_16_18_22_sample <- readxl::read_excel("https://example.org/Ramthal_Source_Data/list_crop.xlsx")

# ============================================================
# GENERAL / COMBINED DATA FRAMES
# ============================================================

hh_baseline
hh_2022

rmtl_cntrl_vars

# Irrigation Methods by Household and Year | DF: irrigation_BL_to_22  ----

### Irrigation 2021-2022 ----

irrigation_2021_22 <-  
  a_irri_rain_method %>%  select( hh_id,season ,irri_method) %>% distinct() %>% 
  mutate(drip  = ifelse(irri_method == "drip",1,0 ),
         flood = ifelse(irri_method %in% c("flood","furrows","hose"),1,0),
         irri  = ifelse(irri_method == "rain",0,1 )
  ) %>%
  mutate(drip_2021 = ifelse(season %in% c("kharif_2021","rabi_2021_22"),drip,NA )) %>% 
  mutate(drip_2022 = ifelse(season %in% c("kharif_2022","rabi_2021_22"),drip,NA )) %>% 
  mutate(drip_2021_22 = drip
  ) %>% 
  mutate(flood_2021 = ifelse(season %in% c("kharif_2021","rabi_2021_22"),flood,NA )) %>% 
  mutate(flood_2022 = ifelse(season %in% c("kharif_2022","rabi_2021_22"),flood,NA )) %>% 
  mutate(flood_2021_22 = flood
  ) %>% 
  mutate(irri_2021 = ifelse(season %in% c("kharif_2021","rabi_2021_22"),irri,NA )) %>% 
  mutate(irri_2022 = ifelse(season %in% c("kharif_2022","rabi_2021_22"),irri,NA )) %>% 
  mutate(irri_2021_22 = irri
  ) %>% 
  group_by(hh_id) %>% 
  summarise(
    drip_use_2021=sum(drip_2021,na.rm = T),
    drip_use_2022=sum(drip_2022,na.rm = T),
    drip_use_2021_22= sum(drip_2021_22,na.rm = T),
    
    flood_use_2021=sum(flood_2021,na.rm = T),
    flood_use_2022=sum(flood_2022,na.rm = T),
    flood_use_2021_22= sum(flood_2021_22,na.rm = T),
    
    ir_use_2021=sum(irri_2021,na.rm = T),
    ir_use_2022=sum(irri_2022,na.rm = T),
    ir_use_2021_22= sum(irri_2021_22,na.rm = T)
  ) %>% 
  mutate(
    drip_use_2021 = ifelse(drip_use_2021==0,0,1), 
    drip_use_2022 = ifelse(drip_use_2022==0,0,1),
    drip_use_2021_22 = ifelse(drip_use_2021_22==0,0,1),
    
    flood_use_2021 = ifelse(flood_use_2021==0,0,1), 
    flood_use_2022 = ifelse(flood_use_2022==0,0,1),
    flood_use_2021_22 = ifelse(flood_use_2021_22==0,0,1),
    
    ir_use_2021 = ifelse(ir_use_2021==0,0,1), 
    ir_use_2022 = ifelse(ir_use_2022==0,0,1),
    ir_use_2021_22 = ifelse(ir_use_2021_22==0,0,1) 
  )   %>% left_join(ir_6methods_2021_2022 ) 

### Irrigation 2018-2020 ---------

# Irrigation method:
attr(rmtl_srvy22$l48a_y_18_1, "labels")

ir_flood_18_20 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with("l48a_y"),-contains("other")) %>% 
  pivot_longer(cols = -c(farmers_hh,hh_id),names_to = "observation",  values_to = "irri_method") %>% 
  separate(observation, into = c("L48a","y" ,"year_ir","crop15"), sep = "_") %>% 
  mutate(year_ir =as.numeric(year_ir),year_ir=year_ir+2000) %>% 
  mutate(irri_method=ifelse(is.na(irri_method),0,irri_method)
  ) %>% 
  group_by(hh_id,year_ir) %>% 
  mutate( flood_use = ifelse(any(irri_method  %in% c(1,2,6 )), 1,0)) %>% 
  select(hh_id,flood_use,year_ir) %>% distinct() %>% ungroup() %>% 
  pivot_wider(names_from = "year_ir", 
              values_from = "flood_use",   
              names_glue = "{.value}_{year_ir}" )

ir_18_20 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with("l48a_y"),-contains("other")) %>% 
  pivot_longer(cols = -c(farmers_hh,hh_id),names_to = "observation",  values_to = "irri_method") %>% 
  separate(observation, into = c("L48a","y" ,"year_ir","crop15"), sep = "_") %>% 
  mutate(year_ir =as.numeric(year_ir),year_ir=year_ir+2000) %>% 
  mutate(irri_method=ifelse(is.na(irri_method),0,irri_method)) %>% 
  group_by(hh_id,year_ir) %>% 
  mutate(
    ir_method = ifelse(3 %in% irri_method , "drip", 
                ifelse(any(irri_method  == 2), "furrows",
                ifelse(any(irri_method  == 1), "flood",
                ifelse(any(irri_method  == 4), "sprinkler",
                ifelse(any(irri_method  == 6), "hose","rain"))))) 
  ) %>%
  select(hh_id,ir_method,year_ir) %>% distinct() %>% ungroup() %>% 
  mutate(ir_use=ifelse(ir_method=="rain",0,1),
         drip_use=ifelse(ir_method=="drip",1,0))

irrigation_2018_2020 <- 
  ir_18_20  %>% pivot_wider(
    names_from = year_ir, values_from = c(ir_method,drip_use, ir_use), 
    names_glue = "{.value}_{year_ir}" ) %>% 
  left_join(ir_flood_18_20)


### Irrigation 2017      ----

irrigation_2017 <- 
  ml18_irri_methods %>% 
  rename(ir_method_2017=irri_method  ) %>%
  mutate(
    drip_use_2017=ifelse(ir_method_2017=="drip",1,0),
    flood_use_2017  =ifelse(ir_method_2017 %in% c("flood","furrows","hose"),1,0),
    ir_use_2017  =ifelse(ir_method_2017=="rain",0,1)
  ) %>%  
  select(hh_id, ir_method_2017, drip_use_2017, flood_use_2017, ir_use_2017)

### Irrigation Baseline  ----

irrigation_BL <- 
  bl_ir_method %>% # IN  df16.R
  rename(
    ir_method_BL=ir_method_name,
    drip_use_BL=drip_use,
    ir_use_BL=ir_use) %>% 
  mutate(flood_use_BL=ifelse(ir_method_BL %in% c( "flood" , "manual"),1,0)) %>% 
  select(hh_id, ir_method_BL, drip_use_BL, ir_use_BL, flood_use_BL)

### Irrigation Baseline to 2022  ----

irrigation_BL_to_22 <- 
  full_join(irrigation_2021_22,
            irrigation_2018_2020) %>% 
  full_join(irrigation_2017) %>% 
  full_join(irrigation_BL) %>% 
  mutate(flood_use = ifelse(
    flood_use_2017==1 | flood_use_2018==1|flood_use_2019==1|flood_use_2020==1|flood_use_2021_22==1,1,0
  ) ) %>% 
  left_join(rmtl_InOut %>%  select(hh_id, drip_use,ir_use)) %>% 
  rename(drip_use_6y=drip_use,
         flood_use_6y=flood_use,
         ir_use_6y=ir_use)



# ==============================================================================
# ENDLINE DATA FRAMES         ----
# ==============================================================================

# L39a	Crop-Plot-Season 21-22
# L39a	Crop-Plot-Season	kharif 2022 # Rabi 2021/2022 # Kharif 2021
# Perennial/biseasonal crops will be listed in Kharif

# Step 1: Extract all numeric crops (prev + new combined)
regular_crops <- rmtl_srvy22 %>%
  select(hh_id, matches("^l39_(prev|new)_\\d+_\\d+_\\d+$")) %>%
  pivot_longer(-hh_id,
    names_to = c("type", "season", "crop_code", "plot_num"),
    names_pattern = "^l39_(prev|new)_(\\d+)_(\\d+)_(\\d+)$",
    values_to = "val",values_drop_na = TRUE
  ) %>% filter(val > 0) %>% select(-val)

# Step 2: Extract all text "other" crops (prev + new combined)
other_crops <- rmtl_srvy22 %>%
  select(hh_id, matches("^l39_(prev|new)_\\d+_other_\\d+")) %>%
  pivot_longer( -hh_id,
    names_to = c("type", "season", "crop_text", "plot_num"),
    names_pattern = "^l39_(prev|new)_(\\d+)_(other)_(\\d+)",
    values_to = "raw_text",values_drop_na = TRUE
  ) %>% filter(raw_text != "") %>%
  mutate(crop_code = case_match(
      raw_text,
      c("Ajwain", "Ajjawain")   ~ "99",
      "Cucumber"~ "90", "Coriander seeds"~ "96",
      "Fodder for Cattle"~ "91", "mulberry leaves"~ "92",
      "Neem tree"~ "93", "palm tree"~ "94",
      "Sandal and Neem trees" ~ "95",.default = raw_text)
  ) %>% select(hh_id, type, season, crop_code, plot_num)

# Step 3: Combine, format IDs and seasons, and join metadata
plots_crop_2022 <- bind_rows(regular_crops, other_crops) %>%
  mutate(
    crop_code = as.integer(crop_code),
    plot_num  = as.integer(plot_num) + if_else(type == "new", 10L, 0L),
    plotID    = sprintf("plot_%02d", plot_num),
    season    = 
      case_match( season,"1" ~ "kharif_2021", "2" ~ "rabi_2021_22","3" ~ "kharif_2022")
  ) %>% left_join(list_crop, by = "crop_code") %>%
  mutate(crop_num    = row_number(),
         crop_number = paste0("crop", crop_num),
         plot_crop   = sprintf("%02d_%d", plot_num, crop_num),
         .by = c(hh_id, season, plotID)
  ) %>%
  left_join(a_plots_size %>% select(hh_id, plotID, acres), by = c("hh_id", "plotID")) %>%
  select(hh_id, season, plotID, crop_common, crop_name, crop_number, plot_crop, acres)




# a_irri_rain_method
# a_plots_size
# a_source_irri

# crop_acre_drip     ----
crop_acre_drip = plots_crop_2022 %>%
  filter(season != "kharif_2022") %>%
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop=acres/n) %>% 
  select(hh_id, season,plot_crop,crop_common, acre_crop ) %>% 
  left_join(a_irri_rain_method %>% 
              select(plot_crop,season,hh_id,irri_method) ) %>% 
  mutate(acre_drip_22=ifelse(irri_method=="drip",acre_crop ,0)) %>% 
  group_by(hh_id, crop_common) %>% summarise(acre_drip_22=sum(acre_drip_22))

# crop_acre_22       ----
crop_acre_22 <-
  plots_crop_2022 %>%
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")
  ) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop=acres/n
  ) %>% 
  group_by(hh_id,crop_common) %>% 
  summarise(acre_crop_22 = sum(acre_crop),.groups="drop") 


# crop_acre_drip_22  ----
crop_acre_drip_22 <-
  crop_acre_22 %>% left_join(crop_acre_drip) %>% 
  group_by(hh_id) %>% 
  summarise(acre_crop_22 = sum(acre_crop_22  ),
            drip_acre = sum(acre_drip_22),.groups="drop") %>% 
  mutate(drip_pct =drip_acre / acre_crop_22
  ) %>% select(-acre_crop_22) %>% 
  pivot_longer(-hh_id,names_to = "drip_var",values_to = "drip_22")

# [df_tableB_1] Acre crop by Season ----

#  
df_tableB_1 <- plots_crop_2022 %>%
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif"))  %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop"
  )




# df_income_NonCrop_22 ----
df_income_NonCrop_22 <- 
  rmtl_srvy22 %>% 
  select(hh_id, f1_amt, f2_amt,f3_1_amt, f4_amt, f5_amt, f6_amt, f7_amt, f9_amt, f10_amt, f11_amt 
  ) %>% pivot_longer(-hh_id ,
               names_to = "income_type_f", values_to = "income22") %>% 
  mutate(income22 = ifelse(income22 == -999,0,income22),
         income_type = ifelse(income_type_f %in% c("f1_amt","f2_amt","f9_amt"),
                  "k_income_assistance","k_income_independent" )) %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop")



# ==============================================================================
# MERGED ENDLINE and BASELINE Dataframes
# ==============================================================================




#  DFs for impact analysis ----
###  DF: Impact_Agriculture ----

# 1. Endline Join with baseline values.
df_tableB <- df_tableB_1 |> 
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

# 2. Full agricultural year totals (summing across seasons).
df_tableC <- 
  df_tableB |> mutate(vars = str_remove(vars, "_Kharif|_Rabi")) |> 
  summarise(val_22 = sum(val_22,na.rm = T),
            val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars)
  ) |> arrange(hh_id)


# 3. Trim top 0.5 percentile of values within each variable to reduce outlier influence.
df_tableA_995 <- df_tableC %>% rbind(df_tableB) %>%
  group_by(vars) %>%
  mutate(
    val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl),
    val_22 = ifelse(val_22 > quantile(val_22, 0.995, na.rm = TRUE), NA, val_22)
  ) %>% ungroup()

# 4. Construct binary adoption indicators (1 if acreage > 0, 0 otherwise).
df_tableD <- df_tableC %>%
  mutate(
    vars = sub("acre", "adopted", vars),
    val_22 = +(val_22 != 0),
    val_bl = +(val_bl != 0)
  )

# 5. Combine outcomes into a single dataset
Impact_Agriculture <- bind_rows(df_tableA_995, df_tableD)


###  DF: Impact_Economic    ----

# 1. Trim top 0.5 percentile of values within each variable to reduce outlier influence.
Economic_clean_995 <- 
  df_return_invest %>% rename(vars=economic_vars) %>% 
  rbind(df_income_NonCrop) %>% 
  rbind(df_inputs) %>%
  group_by(vars) %>%
  mutate(
    val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl),
    val_22 = ifelse(val_22 > quantile(val_22, 0.995, na.rm = TRUE), NA, val_22)
  ) %>% ungroup()

Economic_clean_99 <- 
  df_labor_days %>% 
  mutate(
    val_bl = ifelse(val_bl > quantile(val_bl, 0.99, na.rm = TRUE), NA, val_bl),
    val_22 = ifelse(val_22 > quantile(val_22, 0.99, na.rm = TRUE), NA, val_22)
  ) %>% ungroup()

# 2. Combine outcomes into a single dataset.
Impact_Economic <- # Impact_Economic_clean
  Economic_clean_995 %>% 
  rbind(Economic_clean_99) 


###  DF: df_assets          ----

# E6 Cows, E7 Bullock, E8 Buffaloes, E9 Goats&sheep
# E10 Tractor E11 Plough E12 Thresher E13 Seed drill E14 JCB
# E15Cycles, E16Motorcycles, E17 Cars, E18 Fridge, E19	Television

# 1. Endline: Household asset ownership.
df_22 <- rmtl_srvy22 %>% select(hh_id,e6,e7,e8,e9,e10,e12,e13,e14,e17,e18,e19)
df_22[, 6:11] <- +(df_22[, 6:11] >= 1)
df_22$Livestock_n <- rowSums(df_22[, c( "e6","e7","e8","e9")], na.rm = TRUE)
df_22$Farm_equipments <- rowSums(df_22[, c("e13","e14", "e10", "e12")] > 0, na.rm = TRUE)
df_assets22 <- df_22 %>% 
  select(-c(e6:e9)) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val_22") %>% 
  mutate(vars = case_match( 
    vars, "e10" ~ "Tractor", "e12" ~ "Thresher", "e13" ~ "Seed_drill", 
    "e14" ~ "JCB", "e17" ~ "Car", "e18" ~ "Fridge", "e19" ~ "Television",
    .default = vars
  ))

# 2. Baseline: Household asset ownership.
df_BL <- rmtl_baseline2016 %>% select(hh_id,E6_1, E7_1, E8_1, E9_1, E10_1, E12_1, E13_1, E14_1, E17_1, E18_1, E19_1)
df_BL[, 6:11] <- +(df_BL[, 6:11] >= 1)
df_BL$Livestock_n <- rowSums(df_BL[, c( "E6_1","E7_1","E8_1","E9_1")], na.rm = TRUE)
df_BL$Farm_equipments <- rowSums(df_BL[, c("E13_1","E14_1", "E10_1", "E12_1")] > 0, na.rm = TRUE)
df_assets15 <- df_BL %>%  
  select(-c(E6_1:E9_1)) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val_bl") %>% 
  mutate(vars = case_match( 
    vars, "E10_1" ~ "Tractor", "E12_1" ~ "Thresher", "E13_1" ~ "Seed_drill", 
    "E14_1" ~ "JCB","E17_1" ~ "Car", "E18_1" ~ "Fridge", "E19_1" ~ "Television",
    .default = vars
  ))

# 3. Aggregate land holdings for baseline and endline.
df_land_holding <- 
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(val_22=sum(acres,na.rm = T)) %>% 
  mutate(vars= "land_holding" ) %>% 
  select(hh_id, vars, val_22) %>% 
  left_join(rmtl_cntrl_vars) %>% rename(val_bl=total_acre16 ) %>% 
  select(hh_id, vars, val_22, val_bl) %>% 
  mutate(val_22 = ifelse(vars == "land_holding" & val_22 > 39, NA, val_22)) 

# 4. Combine outcomes into a single dataset.
df_assets <- df_assets22 %>% inner_join(df_assets15) %>% rbind(df_land_holding)

###  DF: Impact_Social      ----

#### literacy rates ----
# Share of literate household members

# Endline: Compute household-level indicator
literacy22=  
  rmtl_srvy22 %>% select(hh_id,starts_with("r7" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literacy_rates=mean(literate,na.rm=T)) %>% 
  mutate(literacy_rates = ifelse(literacy_rates <= 0,NA,literacy_rates)) # transfer NaN to NA

# Baseline: Compute household-level indicator
literacy15= 
  rmtl_baseline2016 %>% select(hh_id,starts_with("c6" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  group_by(hh_id) %>% 
  summarise(literacy_rates=mean(literate,na.rm=T)) %>% 
  mutate(literacy_rates = ifelse(literacy_rates <= 0,NA,literacy_rates))


#### Second-generation educational attainment ----
# Average education level for second-generation household members.

# Endline: Compute household-level indicator
# Reshape education level (r8)
r8_22 =  
  rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  select(-contains("new")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
r8_22$id_member <- sub("^r8_(\\d{1,2})","r_\\1",r8_22$id_member )

# Reshape relationship to household head (r6)
r6_22 =
  rmtl_srvy22 %>% select(hh_id,starts_with("r6" ) ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
r6_22$id_member <- sub("^r6_(\\d{1,2})","r_\\1",r6_22$id_member )

# Join datasets, filter for second generation, and compute household mean
edu_level_2nd_gen_22 <- 
  left_join (r8_22,r6_22) %>% 
  mutate(edu_gen_2nd=ifelse(relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2 = ifelse(edu_gen2<0,NA,edu_gen2)) # transfer NaN to NA

#### Private school enrollment ----
# Binary indicator equal to 1 if any household member attends a private school, 0 if only public, NA otherwise.

# Endline: Compute household-level indicator
# Reshape institution type (r11), classify private vs. public, and aggregate to household level
edu_private_22 = 
  rmtl_srvy22 %>% select(hh_id,starts_with("r11" ) ) %>% 
  pivot_longer( !hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(is.na (institut),0,institut), Private =ifelse(institut==2,1,0), public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_Private =sum(Private ), n_public=sum(public)) %>% 
  mutate(private_school = ifelse(n_Private  > 0, 1, ifelse(n_public > 0, 0, NA))) %>% 
  select(hh_id,private_school)

#### Work migratio      n ----
# Number of household members engaged in seasonal or permanent work migration.

# Endline: Compute household-level indicator

# Seasonal work migration
# Count of seasonal migrants by primary work location past 2 years (rc3).
# Non-migrant (0): 1 = Home village; 2 = Other village; 3 = Urban center within district
# Migrant (1): 4 = Urban center outside district; 5 = Other destination
rc3 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("rc3")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "left") %>% 
  mutate(left =ifelse(left %in% c(4,5),1,0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_seasonal=sum(left))  

# Permanent work migration
# Count of permanent migrants whose primary reason for departure was work (r39).
# Coded 1 if moved for employment (1); 0 if marriage (2), education (3), or other responses
r39 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("r39"), -contains("other")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "why_left") %>% 
  mutate(why_left = ifelse(!is.na(why_left) & why_left == 1, 1, 0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_permanent=sum(why_left))

# Join seasonal and permanent work migration counts
migrants_work_22 <- left_join(rc3,r39)

#### Off-Farm work        ----
# Share of household members whose main income activity past 2 years was off-farm (rc1).
# Coded 1 if activity in 9:12 (salaried, business, non-ag labor); 0 otherwise

# Endline: Compute household-level indicator
rc1 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("rc1_")) %>%   
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "rc", values_to = "val") %>% 
  separate(rc, into = c("rc", "ans", "hhm"), sep = "_", convert = F)%>% 
  filter(!is.na(val)) %>% 
  select(hh_id,hhm, ans,val)

df_work_offFarm_22 <- 
  rc1 %>% mutate(val= ifelse(ans %in% c(9:12) & val==1,1,0 )) %>% 
  select(hh_id,hhm,val) %>% distinct() %>% 
  group_by(hh_id,hhm) %>% summarise(val = sum(val)) %>%  
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(offFarm_work_pct=val/n ) %>% 
  select(hh_id,offFarm_work_pct )

#### Gender Off-Farm work ----
# Share of household members who are women engaged in off-farm work.
# Off-farm defined as activities in 9:12 (rc1); gender 2 indicates female (r4)

# Endline: Compute household-level indicator
df_gender_roster <- 
  rmtl_srvy22 %>% 
  select(hh_id,starts_with("r4_")) %>% 
  pivot_longer(-hh_id, names_to = "rc", values_to = "gender") %>% 
  separate(rc, into = c("rc", "hhm"), sep = "_hh_gender_", convert = F)%>% 
  filter(!is.na(gender)) %>% 
  select(hh_id,hhm,gender)

df_work_gender_22 <- 
  rc1 %>% mutate(val= ifelse(ans %in% c(9:12) & val==1,1,0 )) %>% 
  select(hh_id,hhm,val) %>% distinct() %>% 
  summarise(val = sum(val),.by = c(hh_id,hhm)) %>% 
  left_join( df_gender_roster) %>% 
  summarise(val = sum(val),n=n(),.by = c(hh_id,gender)) %>% 
  group_by( hh_id) %>% mutate(n=sum(n)) %>% ungroup() %>% 
  filter(gender ==2) %>% 
  mutate(woman_offFarmwork_pct=val/n) %>% 
  select(hh_id,woman_offFarmwork_pct )

# Aggregating household-level social indicators and merging baseline and endline rounds.

# Endline: merge social indicators and reshape by household
df_social_22 <- 
  literacy22 %>% 
  left_join(edu_level_2nd_gen_22) %>%
  left_join(edu_private_22) %>%
  left_join(migrants_work_22) %>%
  left_join(df_work_offFarm_22) %>%
  left_join(df_work_gender_22) %>% 
  pivot_longer(-hh_id,names_to = "vars", values_to = "val_22")

# Baseline: merge social indicators and reshape by household
df_social_bl <- 
  literacy15 %>% 
  left_join(edu_level_2nd_gen_15) %>%
  left_join(edu_private_15) %>%
  left_join(migrants_work_bl) %>%
  left_join(df_work_offFarm_bl) %>%
  left_join(df_work_gender_bl) %>% 
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_bl")

# Combined panel of social indicators across both rounds
Impact_Social <- df_social_22 %>% left_join(df_social_bl)















# df_income_NonCrop  ----
df_income_NonCrop <- 
  df_income_NonCrop_22 %>% rename(vars=income_type, val_22= income_NonCrop) %>%
  left_join(
    df_income_NonCrop_bl %>% rename(vars=income_type, val_bl= income_NonCrop) 
    )

df_income_NonCrop |> count(val_22)

# df_inputs

# df_labor_days

# df_return_invest







# ==============================================================================
# BASELINE DATA FRAMES  ----
# ==============================================================================

# DF:[bl6_plotAcre] Area of Plot (acres/gunta)            ----

bl_d6 <- rmtl_baseline2016 %>% select(hh_id,D6_1:D6_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "plot_acre")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "D6_(\\d)$", "plot_0\\1")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "^D6_", "plot_")

bl6_plotAcre <- filter(bl_d6,!is.na(plot_acre))


# DF:[bl13_source_irrigate]  Source of irrigation over the last 5 years ----

D13 <-  rmtl_baseline2016 %>% select(hh_id,starts_with("D13"),-c( D13_12,D13_0) )

# Checking what is defined as "other"
D13 |> filter(!is.na(D13_os_1) | !is.na(D13_os_2 ))

# Update accordingly
D13$D13_1[D13$D13_os_1 == "BOREWELL"] <- 5
D13$D13_2[D13$D13_os_2 == "BOREWELL"] <- 5
D13 <-D13 %>% select(-c(D13_os_1,D13_os_2 ) )%>% 
  pivot_longer(-c(hh_id), names_to = "plot_num", values_to = "irri_source_5y")
D13$plot_num <- str_replace(D13$plot_num, "D13_(\\d)$", "plot_0\\1")
D13$plot_num[D13$plot_num == "D13_10"] <- "plot_10" 

# Variable Naming
D13$irri_source_5y[D13$irri_source_5y <0] <- NA
D13$irri_source_5y[D13$irri_source_5y==1] <- "Canal"
D13$irri_source_5y[D13$irri_source_5y==3] <- "Openwell"
D13$irri_source_5y[D13$irri_source_5y==5] <- "Borewell"
D13$irri_source_5y[D13$irri_source_5y==4] <- "River_Pond_Lake"
D13$irri_source_5y[D13$irri_source_5y==2] <- "Tank"


# Keep the first plot with a valid irrigation source for each household.
# If multiple plots have different sources, only the first valid source is retained.
bl13_source_irrigate <- D13 |> filter(irri_source_5y != "NA") |> group_by(hh_id) |> slice(1) |> ungroup() |> 
  right_join(rmtl_baseline2016 %>% select(hh_id))

# DF:[BL_2015_16_crop_IRsource_IRmethod]  ----
##   DF:BL5y_plot_IRsource_IRmethod  ----
## Plot level: size, irrigation status, method, water source

# [bl_d12]
bl_d12 = rmtl_baseline2016 %>% 
  select(hh_id,D12_1,D12_2,D12_3,D12_4,D12_5,D12_6,D12_7,D12_8,D12_9,D12_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_plot_5y") %>% 
  filter(!is.na(irri_plot_5y))
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "D12_(\\d)$", "plot_0\\1")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "^D12_", "plot_")

d12_16=bl_d12 %>% right_join (bl6_plotAcre)

# [D21]
D21_method <- rmtl_baseline2016 %>% 
  select(hh_id, starts_with("D21") )
D21_method$D21_1[D21_method$D21_os_1== "BOREWEL"] <- 1
D21_method$D21_2[D21_method$D21_os_2== "BOREWELL"] <- 1

D21 <- D21_method %>%
  select(-c(D21_os_1, D21_os_2,"D21_12","D21_0","D21_os_0" )) %>%
  mutate(D21_7 = as.numeric(D21_7 )) %>% 
  pivot_longer(-hh_id, 
               names_to = "plot_num", 
               values_to = "irri_method")

D21$plot_num <- str_replace(D21$plot_num, "D21_(\\d)$", "plot_0\\1")
D21$plot_num[D21$plot_num == "D21_10"] <- "plot_10" 

##1 Flood #2 Furrows #3	Drip #4	Sprinkler #5 Manual  #6	Hose # -888	Other, specify
D21$irri_method[D21$irri_method %in% c(1,2)  ] <- "Flood" 
D21$irri_method[D21$irri_method == 3   ] <- "Drip" 
D21$irri_method[D21$irri_method == 4  ] <- "Sprinkler" 
D21$irri_method[D21$irri_method %in% c(5,6)  ] <- "Manual" 


D6_12_13_21=
  bl6_plotAcre %>% left_join(bl_d12) %>% left_join(D13) %>% left_join(D21)

D6_12_13_21$irri_source_5y[D6_12_13_21$hh_id=="106974" & is.na(D6_12_13_21$irri_source_5y) ] <- "Canal"
D6_12_13_21$irri_method[D6_12_13_21$irri_method<0 & !is.na(D6_12_13_21$irri_source_5y) ] <- "Flood"
D6_12_13_21$irri_method[is.na(D6_12_13_21$irri_method) ] <- "Rain"
D6_12_13_21$irri_method[D6_12_13_21$irri_method<0 ] <- "Rain"

BL5y_plot_IRsource_IRmethod = D6_12_13_21





##   DF:bl_crop_plot_3s              ----
## Season-Crop-plot
d24 <- 
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "d24_"), -ends_with("_0") ) %>% select(-contains("os_")) %>% 
  mutate_at(vars(-hh_id), as.character) %>% 
  pivot_longer(-hh_id, names_to = "scp", values_to = "crop_code") %>% 
  mutate(crop_code=as.numeric(crop_code)) %>% 
  filter(crop_code>0) %>% 
  separate(scp, into = c("prefix1", "number1", "prefix2", "number2", "number3"), sep = "_") %>%
  select(-prefix2,-prefix1) %>% 
  unite("season_cropNUM_plot", number1:number3, sep = "_", remove = TRUE)


# Checking where there is an "other"
D24_os <- rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "D24_") )%>% 
  select(hh_id,contains("os"),
         -ends_with( c("11","12","13","14","15","16","17","_0" ))) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  mutate(hh_id=as.character(hh_id)) %>% 
  select(where(~ !(is.numeric(.) )))

# Adding "Other" to the appropriate column
D24_os1 <- rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "D24_"),
         -ends_with( c("11","12","13","14","15","16","17","_0" ))) %>%  
  mutate(
    D24_1_Crop_1_1 = ifelse(D24_1_Crop_1_1 %in% c(-888, -666), D24_1_Crop_1_os_1, D24_1_Crop_1_1),
    D24_1_Crop_2_1 = ifelse(D24_1_Crop_2_1 %in% c(-888, -666), D24_1_Crop_2_os_1, D24_1_Crop_2_1),
    D24_1_Crop_4_1 = ifelse(D24_1_Crop_4_1 %in% c(-888, -666), D24_1_Crop_4_os_1, D24_1_Crop_4_1),
    D24_1_Crop_1_2 = ifelse(D24_1_Crop_1_2 %in% c(-888, -666), D24_1_Crop_1_os_2, D24_1_Crop_1_2),
    D24_1_Crop_1_3 = ifelse(D24_1_Crop_1_3 %in% c(-888, -666), D24_1_Crop_1_os_3, D24_1_Crop_1_3),
    D24_1_Crop_1_4 = ifelse(D24_1_Crop_1_4 %in% c(-888, -666), D24_1_Crop_1_os_4, D24_1_Crop_1_4),
    D24_1_Crop_1_5 = ifelse(D24_1_Crop_1_5 %in% c(-888, -666), D24_1_Crop_1_os_5, D24_1_Crop_1_5),
    D24_2_Crop_1_1 = ifelse(D24_2_Crop_1_1 %in% c(-888, -666), D24_2_Crop_1_os_1, D24_2_Crop_1_1),
    D24_2_Crop_2_1 = ifelse(D24_2_Crop_2_1 %in% c(-888, -666), D24_2_Crop_2_os_1, D24_2_Crop_2_1),
    D24_2_Crop_1_3 = ifelse(D24_2_Crop_1_3 %in% c(-888, -666), D24_2_Crop_1_os_3, D24_2_Crop_1_3),
    D24_3_Crop_1_1 = ifelse(D24_3_Crop_1_1 %in% c(-888, -666), D24_3_Crop_1_os_1, D24_3_Crop_1_1),
    D24_3_Crop_2_1 = ifelse(D24_3_Crop_2_1 %in% c(-888, -666), D24_3_Crop_2_os_1, D24_3_Crop_2_1),
    D24_3_Crop_3_1 = ifelse(D24_3_Crop_3_1 %in% c(-888, -666), D24_3_Crop_3_os_1, D24_3_Crop_3_1),
    D24_3_Crop_1_3 = ifelse(D24_3_Crop_1_3 %in% c(-888, -666), D24_3_Crop_1_os_3, D24_3_Crop_1_3),
    D24_3_Crop_1_4 = ifelse(D24_3_Crop_1_4 %in% c(-888, -666), D24_3_Crop_1_os_4, D24_3_Crop_1_4),
    D24_3_Crop_1_5 = ifelse(D24_3_Crop_1_5 %in% c(-888, -666), D24_3_Crop_1_os_5, D24_3_Crop_1_5)
  )

D24_os2 <- D24_os1 %>%  select(-contains("os") )%>% 
  mutate(across(-hh_id, as.character)) %>%  # Convert all columns (except hh_id) to character
  mutate(across(
    where(is.character), ~ ifelse(. %in% c("NA", "0","-666", "-888", "-777","-999", "-444"), NA, .) # Replace specific character values with NA
  )) %>%   pivot_longer(-hh_id, names_to = "plotID", values_to = "crop_bl")

D24_os3 <- D24_os2 %>%
  mutate(crop_code = case_when(
    crop_bl == "ULLAGADDI"  ~ "28",              # ULLAGADDI -> Onions
    crop_bl == "THENGU" ~ "63",                # THENGU -> Coconut
    crop_bl == "SUNFLOWER" ~ "18",           # SUNFLOWER -> Sunflower
    crop_bl == "SUGARCANE" ~ "17",           # SUGARCANE -> Sugarcane 
    crop_bl == "SPICES" ~ "Spices",                 # SPICES -> Spices
    crop_bl == "SOUTE KAYI" ~ "30",      # SOUTE KAYI -> Bitter Gourd
    crop_bl == "SHIBEHANU" ~ "41",              # SHIBEHANU -> Banana
    crop_bl == "SHIBEHANNU" ~ "41",             # SHIBEHANNU -> Banana
    crop_bl == "RAIN PROBLEM EMPTY LAND" ~ NA_character_,  # RAIN PROBLEM EMPTY LAND -> NA
    crop_bl == "PIRALA HANNU" ~ "46",            # PIRALA HANNU -> Guava
    crop_bl == "PERU" ~ "46",                    # PERU -> Guava
    crop_bl == "ONION" ~ "28",                   # ONION -> Onions
    crop_bl == "NP" ~ NA_character_,                # NP -> NA
    crop_bl == "NO CROPS IN THIS LAND" ~ NA_character_,  # NO CROPS IN THIS LAND -> NA
    crop_bl == "NO CROPS" ~ NA_character_,          # NO CROPS -> NA
    crop_bl == "NO CROP" ~ NA_character_,           # NO CROP -> NA
    crop_bl == "NO" ~ NA_character_,                # NO -> NA
    crop_bl == "MAYU" ~ "43",                # MAYU -> Jackfruit
    crop_bl == "MANGO" ~ "40",                   # MANGO -> Mango
    crop_bl == "KUSUBI" ~ "2",         # KUSUBI -> Sorghum_jowar
    crop_bl == "KOTAMBIRI" ~ "96",           # KOTAMBIRI -> Coriander
    crop_bl == "KOTAMBARE" ~ "96",           # KOTAMBARE -> Coriander
    crop_bl == "KABBU" ~ "Pumpkin",                 # KABBU -> Pumpkin
    crop_bl == "GREENCHILLY" ~ "59",      # GREENCHILLY -> Green Chilly
    crop_bl == "GOVINA JOLA" ~ "2",     # GOVINA JOLA -> Sorghum_jowar
    crop_bl == "EMPTY LAND RAIN PROBLEM" ~ NA_character_, # EMPTY LAND RAIN PROBLEM -> NA
    crop_bl == "COTTON" ~ "16",                 # COTTON -> Cotton
    crop_bl == "COCONUTE" ~ "63",              # COCONUTE -> Coconut
    crop_bl == "COCONUT" ~ "63",               # COCONUT -> Coconut
    crop_bl == "CHIKKU" ~ "42",                 # CHIKKU -> Chikoo
    TRUE ~    crop_bl                                 # Default: Keep the original value if no match
  ))

D24 <- D24_os3 %>% filter(!is.na(crop_bl))

D24$season <- sub("^(D24_1)_.*", "rabi_2015_16", D24$plotID)
D24$season <- sub("^(D24_2)_.*", "kharif_2015", D24$season)
D24$season <- sub("^(D24_3)_.*", "rabi_2014_15", D24$season)
D24$crop_num <- sub("^.*_Crop_(\\d+)_.*", "crop\\1", D24$plotID)
D24$plotID <- str_replace(D24$plotID, "^.*_(\\d)$", "plot_0\\1")
D24$plotID <- str_replace(D24$plotID, "^.*_10$", "plot_10")
D24$plotID[D24$plotID == "D24_10"] <- "plot_10" 



bl_crop_plot_3s <-
  D24 %>%
  left_join(list_crop %>% mutate(crop_code = as.character(crop_code))) %>% 
  mutate(crop_common = case_when(crop_code == "Spices" ~ "Horticulture", 
                                 crop_code == "Pumpkin" ~ "Vegetables",
                                 TRUE ~ crop_common ))%>% 
  mutate(crop_name = case_when(crop_code == "Spices" ~ "Spices", 
                               crop_code == "Pumpkin" ~ "Pumpkin",
                               TRUE ~ crop_name ))%>% 
  mutate(crop_family = case_when(crop_code == "Spices" ~ "Horticulture", 
                                 crop_code == "Pumpkin" ~ "Vegetables",
                                 TRUE ~ crop_family )) %>% 
  select(-crop_bl)




##   DF:bl28_irri_plot_season        ----
# Original excerpt: Pasted text (2)(1).txt, lines 815-855; copied without editing source lines
D28_1 <- # 2015-16 RABI  
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_1") ) %>% 
  select(farmers_hh: D28_1_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_1$plotID <- str_replace(D28_1$plotID, "D28_1_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_1$plotID[D28_1$plotID=="D28_1_irrigated_1_10"] <- "plot_10"
D28_1=D28_1 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% 
  mutate(season="rabi_2015_16") %>% ungroup()

D28_2 <- # 2015 KHARIF
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_2") ) %>% 
  select(farmers_hh: D28_2_irrigated_1_7)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_2$plotID <- str_replace(D28_2$plotID, "D28_2_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_2=D28_2 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="kharif_2015") %>% 
  ungroup()

D28_3 <- # 2014-15 RABI
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_3") ) %>% 
  select(farmers_hh: D28_3_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_3$plotID <- str_replace(D28_3$plotID, "D28_3_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_3$plotID[D28_3$plotID=="D28_3_irrigated_1_10"] <- "plot_10"

D28_3=D28_3 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% 
  summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% 
  distinct() %>% 
  mutate(season="rabi_2014_15") %>% ungroup()

bl28_irri_plot_season <- 
  rbind(D28_1,D28_2,D28_3) %>% 
  rename(irri_plot_3s =irri_plot )

rm(D28_1,D28_2,D28_3)



##   DF's Merging                    ----

BL_2015_16_crop_IRsource_IRmethod <- 
  bl_crop_plot_3s %>% 
  left_join(bl28_irri_plot_season) %>% 
  left_join(bl6_plotAcre %>% select(hh_id, plot_num, plot_acre ) ) %>% 
  left_join(BL5y_plot_IRsource_IRmethod %>% 
              select(hh_id, plot_num,irri_method) ) 

# DF:[crop_acre_drip_BL] Drip irrigation: acres and share (%)  ----
crop_acre_drip_BL <- 
  BL_2015_16_crop_IRsource_IRmethod%>% 
  filter(season != "rabi_2014_15") %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop = plot_acre/n ,
         drip_plot= ifelse(irri_method== "Drip",acre_crop,0)
  ) %>% group_by(hh_id) %>% 
  summarise(acre_crop_bl = sum(acre_crop),
            drip_acre = sum(drip_plot),.groups="drop") %>% 
  mutate(
    drip_pct =drip_acre/acre_crop_bl
  ) %>% select(-acre_crop_bl) %>% 
  pivot_longer(-hh_id,names_to = "drip_var",values_to = "drip_bl")

# DF:[crop_BL] Crop by plot/season: acre and irrigation method   ----
crop_BL <- 
  BL_2015_16_crop_IRsource_IRmethod %>% 
  select(hh_id,plot_num, season, crop_name, crop_common, plot_acre, irri_method) %>% 
  mutate(crop_common = ifelse(crop_name == "Chillies","Chillies", crop_common)) %>% 
  mutate(crop_common = ifelse(crop_name == "Cotton","Oilseeds", crop_common)) %>% 
  mutate(crop_common = ifelse(crop_name == "Onion","Onions", crop_common)
  ) %>%
  mutate(crop_common = ifelse(crop_common %in% c("Pearl.millet_bajra","Cereals"),"Millet", crop_common)) %>% 
  mutate(crop_common = ifelse(crop_common == "Other","Horticulture", crop_common)) %>% 
  mutate(crop_common = ifelse(crop_common == "VegetablesANDFruits","Vegetables", crop_common)) %>%  
  mutate(irri_method = ifelse(is.na(irri_method),"Rain",irri_method))


# df_tableBbl_1 ----

# 1. Baseline Data (crop_BL)
# Matching a crop to its growing season, similar to what was documented in the Endline survey.
# Divide acreage equally across intercropped plots (plot_acre / n).
# Align seasons: Kharif takes 2015; Rabi takes the maximum across 2014-15 and 2015-16.
# Classify crops into WaterIntensive_acre vs. Rainfed_acre.


df_tableBbl_1 <- crop_BL %>%
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(acre_crop = sum(acre_crop, na.rm = T), .groups="drop") %>% 
  pivot_wider( names_from=season, values_from=acre_crop, values_fill = NA) %>% 
  mutate( Rabi = pmax(rabi_2014_15, rabi_2015_16, na.rm = TRUE), Kharif=kharif_2015
  )%>%
  select(hh_id, crop_common, Kharif, Rabi) %>%
  pivot_longer(cols = c(Kharif, Rabi), names_to = "season", values_to = "acre_crop"
  ) |> filter(!is.na(acre_crop)) |> 
  mutate(season = case_when(crop_common == "Toor" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Sunflower" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Maize" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Millet" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Pulses" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Greengram" ~ "Kharif" ,T~ season )
  ) %>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  )

# df_tableBbl   ----
df_tableBbl <- df_tableBbl_1 |> 
  summarise(val_bl = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

# df_tableC_bl  ----
df_tableC_bl <- df_tableBbl|> mutate(vars = str_remove(vars, "_Kharif|_Rabi")) |> 
  summarise(val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars))

# df_tableD_bl  ----
df_tableD_bl <- df_tableC_bl |> 
  mutate(vars = sub("acre", "adopted", vars),val_bl = +(val_bl != 0))


# hh_members_bl   ----

# Extract gender, age, relation to household head, literacy and education level for each household member.
long_var <- function(df, prefix, value_name) {
  df %>%
    select(hh_id, matches(paste0("^", prefix, "_\\d+$"))) %>%
    pivot_longer(-hh_id,
                 names_to = "id_member",
                 names_pattern = paste0("^", prefix, "_(\\d+)$"),
                 names_transform = \(x) paste0("C_", x),
                 values_to = value_name)
}
vars <- c(C5 = "hh_member", C3 = "gender", C4 = "age", C6 = "literate", C7 = "edu_level")

# Clean the member-level data # recode age 0 to 45; recode negative education codes to NA.
hh_members_bl <- imap(vars, \(name, prefix) long_var(rmtl_baseline2016, prefix, name)) %>%
  reduce(full_join, by = c("hh_id", "id_member")) %>%
  filter(!is.na(age)) %>%
  mutate(age = if_else(age == 0, 45, age), edu_level = if_else(edu_level < 0, NA, edu_level))

# hh_head_ids     ----
# Identify the household head (hh_member == 1).
#   If exactly one member reports being the head, that member is the head, otherwise (no reported head, or more than one), member C_1 is the head.
#   Missing head education (NA) is recoded to 0, a value outside the 1-6 scale.
hh_head_ids <- hh_members_bl %>% select(hh_id, id_member, hh_member) |> 
  group_by(hh_id) %>%
  filter(if (sum(hh_member == 1, na.rm = TRUE) == 1) hh_member == 1 else id_member == "C_1") %>%
  ungroup() |> select(-hh_member)

# hh_head_bl      ----
# Data of household head characteristics: gender, age, literacy and education level.
hh_head_bl <- hh_head_ids |> left_join(hh_members_bl) %>%
  ungroup() %>%
  transmute(hh_id,hh_head_age = age, hh_head_edu_level = replace_na(edu_level, 0), hh_head_gender = gender, hh_head_literate=literate )


# df_bl_assets ----
# LIVESTOCK # [E6 Cows] [E7 Bullock] [E8 Buffaloes] [E9 Goats&sheep]
df_bl_assets <- rmtl_baseline2016 %>%
  select(hh_id, E6_1, E7_1, E8_1, E9_1, E10_1, E11_1, E12_1, E13_1, E16_1, E18_1) %>%
  transmute(hh_id,
            tractor_dum     = as.numeric(E10_1 >= 1),
            bullocks_dum    = as.numeric(E7_1 >= 1),
            traction_dum    = pmax(tractor_dum, bullocks_dum, na.rm = TRUE),
            livestock_total = rowSums(pick(E6_1, E7_1, E8_1, E9_1), na.rm = TRUE),
            livestock_dum   = as.numeric(livestock_total >= 1),
            
            livestock_dairy_total = rowSums(pick(E6_1, E8_1, E9_1), na.rm = TRUE),
            livestock_dairy_dum = as.numeric(livestock_dairy_total >= 1),
            
            machinery_total = rowSums(pick(E10_1, E11_1, E12_1, E13_1), na.rm = TRUE),
            machinery_dum   = as.numeric(machinery_total >= 1),
            
            plough_dum     = as.numeric(E11_1 >= 1),
            thresher_dum     = as.numeric(E12_1 >= 1),
            seed_drill_dum     = as.numeric(E13_1 >= 1),
            
            motorcycles_dum     = as.numeric(E16_1 >= 1),
            fridge_dum    = as.numeric(E18_1 >= 1))



# df_income_f3_bl ----
# Original excerpt: Pasted text(20261116-125312).txt, lines 414-419; copied without editing source lines
df_income_f3_bl <- 
  rmtl_baseline2016  %>% 
  select(hh_id,F3_source, F3_month ,F3_year ) |> 
  mutate(F3_year=ifelse(F3_year==0,NA,F3_year),
         income_bl=(F3_month + F3_year)/1000) |> 
  select(hh_id,income_bl )


# [df_income_f12_bl]  Average total annual household income ----
# Original excerpt: Pasted text(20261116-125312).txt, lines 421-425; copied without editing source lines
df_income_f12_bl <- 
  rmtl_baseline2016  %>% select(hh_id,F12_month ,F12_year ) |> 
  mutate(F12_year=ifelse(F12_year==0,NA,F12_year),
         total_income_bl=(F12_month + F12_year)/1000) |> 
  select(hh_id,total_income_bl )


# land_holding_bl ----
# Depends on BL_plotAcre, whose original code is located separately in the Baseline script.
# Original excerpt: Pasted text(20261116-125312).txt, lines 125-125; copied without editing source lines
land_holding_bl <- BL_plotAcre %>% group_by(hh_id) %>% summarise(hh_acre_bl=sum(plot_acre,na.rm = T))


# df_income_NonCrop_bl ----
# Alternative assignments also exist in other scripts; no reconciliation performed.
# Original excerpt: Pasted text (2)(2).txt, lines 297-308; copied without editing source lines
df_income_NonCrop_bl <- 
  rmtl_baseline2016 %>%
  select(hh_id,starts_with("F")) %>% 
  select(hh_id, contains("year"),-F3_year, -F12_year)%>%
  pivot_longer(-hh_id ,
               names_to = "income_type_f",
               values_to = "incomeBL") %>% 
  mutate(income_type=
           ifelse(income_type_f %in% c("F1_year","F2_year","F9_year"),
                  "k_income_assistance","k_income_independent" ))  %>% 
  group_by(hh_id, income_type ) %>% 
  summarise(income_NonCrop = sum(incomeBL,na.rm = T)/1000,.groups = "drop")


# df_inputs_type_bl    ----

cult_acre_bl <- bl_crop_plot_3s %>% 
  filter(season != "rabi_2014_15") %>% 
  select(hh_id, plotID) %>% 
  left_join(BL_plotAcre) %>%
  summarise(acre = sum(plot_acre,na.rm = T),.by = hh_id )

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


# labor_days_BL       ----
# Amount used on plot - Labor	Mandays     
labor_days_BL <- 
  rmtl_baseline2016 %>% 
  select(hh_id, D3, starts_with("D43_uasge"), -ends_with("_0")) %>% 
  pivot_longer(-c(hh_id, D3), 
               names_to = c(NA, NA, "season", NA), names_sep = "_", 
               values_to = "val_bl"
  ) %>% 
  filter(val_bl >= 0, season != "3") %>% 
  summarise(vars = "labor_days_total", val_bl = na_if(sum(val_bl) * first(D3), 0), .by = hh_id
  )

# [edu_level_2nd_gen_15] HH second generation education level ----
#  Education level [1-6 Index]: HH second generation
edu_level_2nd_gen_15 <- rmtl_baseline2016 %>% 
  select(hh_id, starts_with(c("c7", "c5")), -ends_with("_bin"), -contains("os")) %>% 
  pivot_longer(-hh_id,
    names_to = c(".value", "id_member"),
    names_pattern = "(?i)^(c7|c5)_(\\d{1,2})"
  ) %>% 
  rename_with(tolower) %>% 
  summarise(edu_gen2 = mean(c7[c5 %in% c(3, 9, 11:13)], na.rm = TRUE),.by = hh_id
  ) %>% 
  mutate(edu_gen2 = ifelse(is.nan(edu_gen2) | edu_gen2 < 0, NA, edu_gen2))





# [edu_private_15] Household members enrolled in private school -----

# Share of household members enrolled in private school (%)
# Reshape institution type (c10), classify private vs. public, and aggregate to household level
edu_private_15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C10" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(!institut %in% c(1,2) ,0,institut), Private =ifelse(institut==2,1,0), public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_Private =sum(Private ), n_public=sum(public)) %>% 
  mutate(private_school = ifelse(n_Private  > 0, 1, ifelse(n_public > 0, 0, NA))) %>% 
  select(hh_id,private_school)






# [hh_haed_Main_Occupation] HH head primary occupation is in own farm  -----
# HH head primary occupation is in own farm: # Rainy season # Rest of the year
#   What are their main income-earning activities in the [C13] rainy season / [C17]	rest of the year, in order of importance? (Select up to 3).
#   (1) no occupation # (2) student # (3) unpaid house work (housewife) # (4) works on own farm: Cultivation # (5) works on own farm: Dairy # (6) works on own farm: Animal Husbandry # (7) works in household business # (8) unemployed/searching for job # (9) agricultural laborer # (10) casual wage laborer (non-agricultural) # (11) salaried employee
own_farm <- function(prefix, label) {
  rmtl_baseline2016 %>%
    select(hh_id, matches(paste0("^", prefix, "_1_\\d+$"))) %>%
    pivot_longer(-hh_id,
                 names_to = "id_member",
                 names_pattern = paste0("^", prefix, "_1_(\\d+)$"),
                 names_transform = \(x) paste0("C_", x),
                 values_to = "val_bl") %>%
    filter(!is.na(val_bl)) %>%
    mutate(val_bl = as.numeric(val_bl %in% 3:6)) %>%
    right_join(hh_head_ids, by = c("hh_id", "id_member")) %>%
    transmute(hh_id, vars = label, val_bl)
}

hh_haed_Main_Occupation <- bind_rows(
  own_farm("C13", "hh_occupation_ownFarm_rainySeason"),
  own_farm("C17", "hh_occupation_ownFarm_restOFyear"))





# [water_source_bl] Water source for irrigation  -----

# # Water source | Canal
# # Water source | Openwell

water_source_bl <- 
  bl13_source_irrigate %>%
  select(hh_id, irri_source_5y) %>%
  mutate(val = 1) %>%
  pivot_wider(names_from = irri_source_5y, names_prefix = "water_source_",
              values_from = val, values_fill = 0) %>%
  select(-any_of("water_source_NA")) 


# [df_revenue_income_bl] HH Annual income  -----

# Annual income (thousand Rs.): Farming income - Per HH/Per acre
# # Land holding (acre)

df_revenue_income_bl <- 
  df_income_f3_bl %>% 
  left_join(land_holding_bl) %>% 
  mutate( revenue_acre=income_bl/land_holding) %>% 
  rename(farming_income = income_bl) %>% 
  pivot_longer(-hh_id ,names_to = "vars",values_to = "val_bl") 




# [migrants_work_bl] Seasonal/Permanent Migration  -----
# Migration (Total HH members)

# Seasonal work migration
# # Count of seasonal migrants by work location outside district/state (C21).
# # Non-migrant (0): 1 = Home village; 2 = Other village; 3 = Urban center within district
# # Migrant (1):     4 = Urban center outside district; 5 = Other destination
# Permanent work migration
# # Count of permanent migrants whose primary reason for departure was work (c34).
# # Coded 1 if moved for employment (1); 0 if marriage (2), education (3), or remaining responses

migrants_work_bl <- rmtl_baseline2016 %>% 
  transmute(
    hh_id,
    migrants_work_permanent = rowSums(across(starts_with("C34")) == 1, na.rm = TRUE),
    migrants_work_seasonal  = rowSums(across(starts_with("C21"), ~ .x %in% c(4, 5)))
  )



# [df_work_offFarm_bl]  [df_work_gender_bl] Off-farm work  -----
# Off-farm work (percent of HH members)
# Share of household members whose main income activity past 2 years was off-farm
# Coded 1 if activity in 9:12 (salaried, business, non-ag labor); 0 otherwise
# Main income-earning activities in the rainy season (c13) & the rest of the year (c17)
df_work_offFarm_bl <- rmtl_baseline2016 %>% 
  select(hh_id, matches("^(C13|C17)_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id,
    names_to = c(NA, NA, "hhm"),names_sep = "_",
    values_to = "ans",values_drop_na = TRUE
  ) %>% 
  distinct(hh_id, hhm, val = as.integer(ans %in% 9:12)) %>% 
  summarise(offFarm_work_pct = mean(val),.by = hh_id)



# Off-farm defined as activities in 9:12 (c17); gender 2 indicates female (c3)
df_gender_rosterBL <- 
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with("c3_")) %>% 
  pivot_longer(-hh_id, names_to = "c3", values_to = "gender") %>% 
  separate(c3, into = c("c3", "hhm"), sep = "_", convert = F) %>% 
  filter(!is.na(gender)) %>% mutate(gender=ifelse(gender==0,2,gender)) %>% 
  select(hh_id,hhm,gender)

df_work_gender_bl <- 
  c17 %>% select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  summarise(val = sum(val),.by=c(hh_id,hhm)) %>% 
  left_join( df_gender_rosterBL) %>% 
  summarise(val = sum(val),n=n(),.by = c(hh_id,gender)) %>% 
  group_by( hh_id) %>% mutate(n=sum(n)) %>% ungroup() %>% 
  filter(gender ==2) %>% 
  mutate(woman_offFarmwork_pct=val/n) %>% 
  select(hh_id,woman_offFarmwork_pct )











#===============================================================================
#  DFs for SUMMARY STATISTICS  ----
#===============================================================================
#
# Section: Regional Agricultural Practices
# Figure : Drip irrigation and water source 2016–2022
# Appendix Table  : Crop Cultivation Area and Adoption Rates

#...............................................................................
# Section: Regional Agricultural Practices   ----
#...........................................................................----
# Summary data frames: baseline indicators (one data frame per indicator group)
# Below are the data frames that compose df_summaries

library(tidyverse)

# Ave. acre land cultivated per hh in Rabi and Kharif
df_summary1_acres_per_hh_season <- df_tableBbl %>%
  mutate(vars = str_remove(vars, "^[^_]+_[^_]+_")) %>%
  group_by(hh_id, vars) %>%
  summarise(val_bl = sum(val_bl, na.rm = TRUE), .groups = "drop") %>%
  group_by(vars) %>%
  summarise(mean_acre = mean(val_bl, na.rm = TRUE), .groups = "drop")

# % of gross cultivated area by crop category
df_summary2_GCA_share_by_crop  <- df_tableC_bl %>% group_by(vars) %>%
  summarise(val_bl = sum(val_bl, na.rm = TRUE), .groups = "drop") |> 
  mutate(pct_cultivated_acre_crop_type = val_bl/ sum(df_tableC_bl$val_bl, na.rm = TRUE) )

# % of households growing "target crops"
df_summary3_pct_hh_target_crops <- df_tableD_bl %>% group_by(vars) %>% summarise(mean_val_bl = mean(val_bl, na.rm = TRUE), .groups = "drop")

# Total cultivated area per season
season_totals <- df_tableBbl_1 %>% group_by(season) %>% summarise(season_total_acres = sum(acre_crop, na.rm = TRUE))

df_summary4_CROP_season_land_pct_AND_hh_grow_pct <- df_tableBbl_1 %>%
  group_by(season, crop_common) %>%
  summarise(total_acres = sum(acre_crop, na.rm = TRUE),
            total_farmers = n_distinct(hh_id),
            farmer_participation_pct = (total_farmers / n_distinct(df_tableBbl$hh_id)),
            .groups = "drop") %>%
  left_join(season_totals, by = "season") %>%
  mutate(pct_of_season_land = (total_acres / season_total_acres) ) %>%
  group_by(crop_common) %>%
  slice_max(total_farmers, n = 1, with_ties = FALSE) %>%
  ungroup()  %>%
  select(crop_common, pct_of_season_land, farmer_participation_pct)

# Households using flood or drip irrigation
df_summary5_drip_OR_flood_irrigation_hh_share <- 
  irrigation_BL_to_22 |> summarise(mean(drip_use_BL,na.rm=T), mean(flood_use_BL,na.rm=T))

# Water source for irrigation
df_summary6_water_source_hh_share <- water_source_bl |> summarise(
  Canal=mean(water_source_Canal ,na.rm=T), 
  Openwell=mean(water_source_Openwell ,na.rm=T), 
  Surface_water= mean(water_source_River_Pond_Lake ,na.rm=T),
  Borewell= mean(water_source_Borewell ,na.rm=T))
    
# hh owning assets 
df_summary7_asset_own_hh_share <- df_bl_assets %>%
  summarise(across(c(traction_dum, tractor_dum, bullocks_dum, livestock_dum, machinery_dum), ~ mean(.x, na.rm = TRUE)))

# Average annual household agricultural income: per HH and per acre
df_summary8_farming_incom <- df_income_f3_bl %>%
  left_join(land_holding_bl) %>%  transmute(hh_id,HH_farming_incom=income_bl, farming_incom_per_acre=income_bl/land_holding) %>% 
  pivot_longer(-hh_id, names_to = "vars",values_to = "val_bl")%>%
  group_by(vars) %>%
  mutate( val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl)) |> 
  group_by(vars) |> summarise(`mean_Rs.`=mean(val_bl,na.rm=T)*1000)

# Farms relying exclusively on agricultural revenue OR depending on off-farm income
df_summary9_income_source_type <- df_income_NonCrop_bl |>
  mutate(val_bl = as.numeric(income_NonCrop >= 1)) |>
  pivot_wider(names_from = income_type, values_from = val_bl) |>
  count(income = case_when(
    k_income_independent == 1 & k_income_assistance == 1 ~ "Both",
    k_income_independent == 1 ~ "Off-farm income",
    k_income_assistance == 1 ~ "External income sources",
    TRUE ~ "Farming only")) |>
  mutate(pct = n / sum(n))

# Average total annual household income
df_summary10_HH_total_incom <- df_income_f12_bl |> mutate( total_income_bl = ifelse(total_income_bl > quantile(total_income_bl, 0.995, na.rm = TRUE), NA, total_income_bl)) |>  
  summarise(total_hh_income=mean(total_income_bl,na.rm=T)*1000)

# # Combine the results
library(tibble)

df_summaries <- lst(
  df_summary1_acres_per_hh_season,
  df_summary2_GCA_share_by_crop,
  df_summary3_pct_hh_target_crops,
  df_summary4_CROP_season_land_pct_AND_hh_grow_pct,
  df_summary5_drip_OR_flood_irrigation_hh_share,
  df_summary6_water_source_hh_share,
  df_summary7_asset_own_hh_share,
  df_summary8_farming_incom,
  df_summary9_income_source_type,
  df_summary10_HH_total_incom
)

names(df_summaries) <- sub("^df_summary[0-9]+_", "", names(df_summaries))

#...............................................................................
# Figure : Drip irrigation and water source 2016–2022  ----
#..........................................................................----

ft_01 <- setNames( rmtl_srvy22 |> summarise(mean(mm4),.by = in1_out0),
                   c("Reported that infrastructure was installed on their plot", "Freq"))

ft_02 <- setNames(rmtl_srvy22 |> summarise(mean(mm2_1),.by = in1_out0),
                  c("I'm in Ramthal project", "Freq"))

ft_03 <- setNames(rmtl_srvy22 |> select(hh_id, in1_out0) |> left_join(irrigation_BL_to_22) |> 
                    summarise(Drip_freq = mean(drip_use_2021_22,na.rm=T),.by = in1_out0),
                  c("% of drip users 2021-22", "Freq"))

ft_04 <- setNames(rmtl_srvy22 |> select(hh_id, in1_out0) |> left_join(irrigation_BL_to_22) |> 
                    summarise(Flood_freq = mean(flood_use_2021_22, na.rm=T),.by = in1_out0),
                  c("% of floode users 2021-22", "Freq"))

ft_06_Percentage_of_Drip_irrigated_land_and_Acre_size <- setNames(rmtl_srvy22 |> select(hh_id, in1_out0) |> inner_join(crop_acre_drip_22) |> 
                    summarise(vars = mean(drip_22, na.rm=T),.by = c(drip_var, in1_out0)) |> arrange(vars),
                  c("vars","in1_out0", "Value"))

drip_adoption <- irrigation_BL_to_22 |> select(hh_id,drip_use_2017, drip_use_2018, drip_use_2019,drip_use_2020, drip_use_2021, drip_use_2022,drip_use_6y ,drip_use)|> 
  right_join(hh_2022)

ft_10_Percentage_of_drip_adopters <- drip_adoption |> group_by(in1_out0 ) |> 
  summarise(across(starts_with("drip_use_20"), ~ mean(.x, na.rm = TRUE)))

ft_11_cumulative_percent_of_adopters_by_year <- drip_adoption %>%
  mutate(ever_adopted_17_22 = rowSums(select(., starts_with("drip_use_20")), na.rm = TRUE) > 0) %>%
  filter(in1_out0 == 1, ever_adopted_17_22) %>%
  pivot_longer(cols = starts_with("drip_use_20"), names_to = "year", values_to = "adopted"
  ) %>% arrange(hh_id, year) %>%
  group_by(hh_id) %>%
  mutate(cum_adopted = cummax(replace_na(adopted, 0))) %>%
  group_by(year) %>% 
  summarise(n_cum = sum(cum_adopted), cum_percent = round(mean(cum_adopted) * 100, 2),.groups = "drop") %>% select( year,cum_percent)


ft_12_percent_of_NEW_adopters <- drip_adoption %>% filter(in1_out0 == 1) %>%
  pivot_longer( cols = starts_with("drip_use_20"), names_to = "year",values_to = "adopted"
  ) %>%
  mutate(adopted = replace_na(adopted, 0)) %>%
  arrange(hh_id, year) %>%
  group_by(hh_id) %>%
  mutate(is_new = as.integer(adopted == 1 & cumsum(adopted) == 1)) %>%
  group_by(year) %>%
  summarise(new_adopters_pct = round(mean(is_new) * 100, 2),.groups = "drop"
  )


Avg_total_years_of_drip_use <- rmtl_srvy22  %>%
  select(hh_id, starts_with("m20_")) %>%
  pivot_longer(-hh_id,names_to = "M20",values_to = "val") %>%
  separate(M20, into = c("var", "season", "year"), sep = "_", remove = F) %>%
  group_by(hh_id,year) %>% summarise(use=sum(val,na.rm = T)) %>%
  mutate(use=ifelse(use==2,1,use)) %>%
  summarise(drip_use_total_years=sum(use)) %>%
  count(drip_use_total_years) %>% filter(drip_use_total_years>0) %>%
  mutate(N=sum(n),pct = paste0(round(n/N*100), "%")) %>% select(drip_use_total_years, pct)

# # Combine the results
library(tibble)
drip_water_source_DFs <- 
  lst( ft_01, ft_02, ft_03, ft_04,
        ft_06_Percentage_of_Drip_irrigated_land_and_Acre_size, 
        ft_10_Percentage_of_drip_adopters,
        ft_11_cumulative_percent_of_adopters_by_year, 
        ft_12_percent_of_NEW_adopters,
        Avg_total_years_of_drip_use
  )





#...............................................................................
# Appendix Table  : Crop Cultivation Area and Adoption Rates  ----
#...............................................................................


##   1: Baseline Period (2016) ----
# ........................................................................

### Reshape crop data ----
#   Complete zero-acreage combinations across seasons and merge sample status
df_bl_cultivation <- 
  df_tableBbl_1 |> rename(val_bl = acre_crop, vars = crop_common)%>% select(-crop_type ) %>% 
  complete(season, hh_id, vars, fill = list(val_bl = 0)) %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) )

# Count unique households: Full sample, treatment and control
n_total_BL <- n_distinct(df_bl_cultivation$hh_id)
n_out_BL   <- n_distinct(df_bl_cultivation$hh_id[df_bl_cultivation$in_project == 0])
n_in_BL    <- n_distinct(df_bl_cultivation$hh_id[df_bl_cultivation$in_project == 1])

### a. ADOPTION RATE (GROWER SHARE) ----

# ADOPTION | CROPS
# Aggregate grower adoption shares per crop
crop_adopters_summary_BL <- df_bl_cultivation |> select(-season) %>% mutate( val_bl = +(val_bl != 0)) |> distinct() %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    pct_growers_all_BL = num_growers_all / n_total_BL,
    
    # Inside the project
    num_growers_IN  = sum(val_bl > 0 & in_project == 1, na.rm = TRUE),
    pct_growers_IN_BL  = num_growers_IN / n_in_BL,
    
    # Outside the project
    num_growers_OUT = sum(val_bl > 0 & in_project == 0, na.rm = TRUE),
    pct_growers_OUT_BL = num_growers_OUT / n_out_BL
  ) %>% select(vars, pct_growers_all_BL ,pct_growers_IN_BL, pct_growers_OUT_BL )


# ADOPTION | CROP CATEGORY
# Aggregate grower adoption shares by crop category (Traditional vs Target)
crop_type_adopters_summary_BL <- df_tableD_bl %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) ) %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    pct_growers_all_BL = num_growers_all / n_total_BL,
    num_growers_IN  = sum(val_bl > 0 & in_project == 1, na.rm = TRUE),
    pct_growers_IN_BL  = num_growers_IN / n_in_BL,
    num_growers_OUT = sum(val_bl > 0 & in_project == 0, na.rm = TRUE),
    pct_growers_OUT_BL = num_growers_OUT / n_out_BL
  ) %>% select(vars, pct_growers_all_BL ,pct_growers_IN_BL, pct_growers_OUT_BL ) |> 
  mutate(vars=ifelse(vars=="Rainfed_adopted","Traditional crop","Target crop"))

### b. CULTIVATED ACREAGE (MEAN AREA) ----

# ACRE | CROPS
# Aggregate mean acreage per crop, filtered to peak-grower season
crop_acre_summary_BL <- df_bl_cultivation %>%
  summarise( 
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    mean_acre_all_BL   = mean(val_bl, na.rm = TRUE), # full sample
    mean_acre_IN_BL    = mean(val_bl[in_project == 1], na.rm = TRUE),
    mean_acre_OUT_BL   = mean(val_bl[in_project == 0], na.rm = TRUE),
    .by = c(season, vars)
  )|> group_by(vars) %>%
  slice_max(num_growers_all, n = 1, with_ties = FALSE) %>%
  ungroup()  %>% select(-season, -num_growers_all)

# ACRE | CATEGORY
# Aggregate mean household acreage by crop category (Traditional vs Target)
crop_type_acre_summary_BL <- df_tableBbl_1 |> 
  summarise(val_bl = sum(acre_crop),.by = c(hh_id, crop_type)) %>% 
  rename(vars = crop_type) %>%  complete(hh_id, vars, fill = list(val_bl = 0)) |>   
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) ) |> 
  summarise( 
    mean_acre_all_BL   = mean(val_bl, na.rm = TRUE), # full sample
    mean_acre_IN_BL    = mean(val_bl[in_project == 1], na.rm = TRUE),
    mean_acre_OUT_BL   = mean(val_bl[in_project == 0], na.rm = TRUE),
    .by = vars
  )|> 
  mutate(vars=ifelse(vars=="Rainfed_acre","Traditional crop","Target crop"))



### c. Combine baseline ACREAGE and ADOPTION summaries  ----

crop_summary_BL <- 
  crop_acre_summary_BL |> rbind(crop_type_acre_summary_BL) |> 
  left_join(crop_adopters_summary_BL |> rbind(crop_type_adopters_summary_BL))

print(crop_summary_BL)
# ........................................................................
##   2: Endline Period (2022)  ----
# ........................................................................

### Reshape crop data ----
# Complete zero-acreage combinations across seasons and merge sample status
df_22_cultivation <- 
  df_tableB_1 |> rename(val_22 = acre_crop, vars = crop_common) %>% 
  complete(season, hh_id, vars, fill = list(val_22 = 0)) %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) )

# Count unique households: Full sample, treatment and control
n_total_22 <- n_distinct(df_22_cultivation$hh_id)
n_out_22   <- n_distinct(df_22_cultivation$hh_id[df_22_cultivation$in_project == 0])
n_in_22    <- n_distinct(df_22_cultivation$hh_id[df_22_cultivation$in_project == 1])

### a. ADOPTION RATE (GROWER SHARE) ----

# ADOPTION | CROPS
# Aggregate grower adoption shares per crop
crop_adopters_summary_22 <- df_22_cultivation |> select(-season) %>% mutate( val_22 = +(val_22 != 0)) |> distinct() %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) ) %>% 
  group_by(vars) %>%
  summarise(
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    pct_growers_all_22 = num_growers_all / n_total_22,
    num_growers_IN  = sum(val_22 > 0 & in_project == 1, na.rm = TRUE),
    pct_growers_IN_22  = num_growers_IN / n_in_22,   
    num_growers_OUT = sum(val_22 > 0 & in_project == 0, na.rm = TRUE),
    pct_growers_OUT_22 = num_growers_OUT / n_out_22
  ) %>% select(vars, pct_growers_all_22 ,pct_growers_IN_22, pct_growers_OUT_22 )

# ADOPTION | CROP CATEGORY
# Aggregate grower adoption shares by crop category (Traditional vs Target)
crop_type_adopters_summary_22 <- df_tableD %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) ) %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    pct_growers_all_22 = num_growers_all / n_total_22,
    num_growers_IN  = sum(val_22 > 0 & in_project == 1, na.rm = TRUE),
    pct_growers_IN_22  = num_growers_IN / n_in_22,
    num_growers_OUT = sum(val_22 > 0 & in_project == 0, na.rm = TRUE),
    pct_growers_OUT_22 = num_growers_OUT / n_out_22
  ) %>% select(vars, pct_growers_all_22 ,pct_growers_IN_22, pct_growers_OUT_22 )|> 
  mutate(vars=ifelse(vars=="Rainfed_adopted","Traditional crop","Target crop")) 


### b. CULTIVATED ACREAGE (MEAN AREA) ----

# ACRE | CROPS
# Aggregate mean acreage per crop, filtered to peak-grower season
crop_acre_summary_22 <- df_22_cultivation %>%
  summarise( 
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    mean_acre_all_22   = mean(val_22, na.rm = TRUE), # full sample
    mean_acre_IN_22    = mean(val_22[in_project == 1], na.rm = TRUE),
    mean_acre_OUT_22   = mean(val_22[in_project == 0], na.rm = TRUE),
    .by = c(season, vars)
  )|> group_by(vars) %>%
  slice_max(num_growers_all, n = 1, with_ties = FALSE) %>%
  ungroup()  %>% select(-season, -num_growers_all) |> 
  arrange(desc(mean_acre_all_22 ))

# ACRE | CATEGORY
# Aggregate mean household acreage by crop category (Traditional vs Target)
crop_type_acre_summary_22 <- df_tableC |> 
  summarise(val_22 = sum(val_22),.by = c(hh_id, vars)) |>   
  left_join(rmtl_16_18_22_sample %>% select(hh_id, in_project) ) |> 
  summarise( 
    mean_acre_all_22   = mean(val_22, na.rm = TRUE), # full sample
    mean_acre_IN_22   = mean(val_22[in_project == 1], na.rm = TRUE),
    mean_acre_OUT_22   = mean(val_22[in_project == 0], na.rm = TRUE),
    .by = vars)|> 
  mutate(vars=ifelse(vars=="Rainfed_acre","Traditional crop","Target crop"))


### c. Combine endline ACREAGE and ADOPTION summaries  ----
crop_summary_22 <- 
  crop_acre_summary_22 |> rbind(crop_type_acre_summary_22) |> 
  left_join(crop_adopters_summary_22 |> rbind(crop_type_adopters_summary_22))




# ........................................................................
##   Merge  ----
# ........................................................................

# Merge Endline and Baseline metrics into a single comparison table

crop_summary_22BL <- 
  crop_acre_summary_22 |> rbind(crop_type_acre_summary_22) |> 
  left_join(crop_acre_summary_BL |> rbind(crop_type_acre_summary_BL)) |> 
  left_join(crop_adopters_summary_22 |> rbind(crop_type_adopters_summary_22)) |> 
  left_join(crop_adopters_summary_BL |> rbind(crop_type_adopters_summary_BL)) 

crop_summary_22BL$vars[crop_summary_22BL$vars=="Bengal_gram" ] <- "Chichpeas"
crop_summary_22BL$vars[crop_summary_22BL$vars=="Oilseeds" ] <- "Oil seeds"
crop_summary_22BL$vars[crop_summary_22BL$vars=="Onions" ] <- "Onion"
crop_summary_22BL$vars[crop_summary_22BL$vars=="Sorghum_jowar" ] <- "Sorghum (Jowar)"
names(crop_summary_22BL)[names(crop_summary_22BL) == "vars"] <- "Crop"

new_row <- tibble(
  Crop               = "Total N",
  mean_acre_all_22   = n_total_22,
  mean_acre_IN_22    = n_in_22,
  mean_acre_OUT_22   = n_out_22,
  mean_acre_all_BL   = n_total_BL,
  mean_acre_IN_BL    = n_in_BL,
  mean_acre_OUT_BL   = n_out_BL,
  pct_growers_all_22 = n_total_22,
  pct_growers_IN_22  = n_in_22,
  pct_growers_OUT_22 = n_out_22,
  pct_growers_all_BL = n_total_BL,
  pct_growers_IN_BL  = n_in_BL,
  pct_growers_OUT_BL = n_out_BL
)

crop_summary_Paper <- bind_rows(new_row, crop_summary_22BL)


# ==============================================================================
# DATA FRAME [df_for_ttest]: Balance Test   ----
# ==============================================================================
#
#  Data frame of baseline indicators for balance tests

library(tidyverse)

### ---- Data frame 1

DF1_balanc <- 
  # Generate HH index
  rmtl_baseline2016 %>% select(hh_id) |> left_join(hh_head_ids) |> 
  # Head of HH gender [Male]
  # Head of HH age
  # Literate HH head
  # Education level [1-6 Index]: HH Head
  left_join(hh_head_bl) %>% select(-id_member) |> 
  # Share of literate HH members (%)
  left_join(hh_members_bl |> summarise(hh_pct_literate =mean(literate,na.rm=T),.by = hh_id)) |> 
  # Education level [1-6 Index]: HH second generation
  left_join(edu_level_2nd_gen_15) |> 
  # Share of household members enrolled in private school (%)
  left_join(edu_private_15) |>
  # No. of members in the HH 
  left_join(rmtl_baseline2016 %>% select(hh_id, C1) %>% rename(total_hh_mmbr=C1)) |> 
  # HH Has a BPL Ration Card
  # No. of rooms in the house
  left_join(rmtl_baseline2016 %>% select(hh_id,B2, B8, B9) %>% 
              rename(total_rooms=B2, hh_BPL_Card=B8, Govt_Transfers = B9)) |> 
  pivot_longer(-hh_id, names_to = "vars",values_to = "val_bl") |> 
  # HH head primary occupation in own farm: # Rainy season # Rest of the year
  rbind(hh_haed_Main_Occupation) 

### ---- Data frame 2

DF2_balanc <- 
  # Generate HH index
  rmtl_baseline2016 %>% select(hh_id) |>
  # # Water source | Canal
  # # Water source | Openwell
  left_join(water_source_bl |> select(hh_id,water_source_Openwell ,water_source_Canal)) |>
  # Flood irrigation
  # Practices drip irrigation
  left_join(irrigation_BL_to_22 %>% select(hh_id,ir_use_BL, flood_use_BL)) |>
  # No. of plots owned
  left_join(rmtl_baseline2016 %>% transmute(hh_id, total_plot = D3) )%>%
  # Farm equipment (count, see notes)
  # Livestock (count)
  left_join(df_bl_assets |> select(hh_id ,machinery_total, livestock_total)) |>
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_bl")

### ---- Data frame 3
DF3_balanc <- 
  # Gross Cropped Area (GCA): target crops
  # Gross Cropped Area (GCA): traditional crops
  # Rabi target crops (acre)
  # Rabi traditional crops (acre)
  # Kharif target crops (acre)
  # Kharif traditional crops (acre)
  # HH Cultivates target crops (Binary)
  # HH Cultivates traditional crops (Binary)
  rbind(df_tableBbl, df_tableC_bl, df_tableD_bl) |>
  # Inputs (thousand Rs. per acre): Irrigation # Mechanization # Labor
  rbind(df_inputs_type_bl, 
        # Annual income (thousand Rs.): Farming income - Per HH/Per acre
        # # Land holding (acre)
        df_revenue_income_bl,
        # Govt. Transfers # Off-farm earnings 
        df_income_NonCrop_bl |> rename(vars = income_type, val_bl = income_NonCrop) )%>%
  group_by(vars) %>%
  mutate( val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl)) %>% ungroup() |> 
  # Labor: On-Farm Labor (annual person-days) # Off-farm work (% of HH members) # Off-farm work females 
  rbind(labor_days_BL |> mutate( val_bl = ifelse(val_bl > quantile(val_bl, 0.99, na.rm = TRUE), NA, val_bl))
  ) 

### ---- Data frame 4 
DF4_balanc <- 
  # Migration (Total HH members): Seasonal # Permanent
  migrants_work_bl %>%  
  # Off-farm work (percent of HH members) # Off-farm work, females
  left_join(df_work_offFarm_bl) %>% left_join(df_work_gender_bl) %>% 
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_bl")


### ---- df_for_ttest Combine data frames -----

df_for_ttest = 
  rbind(DF1_balanc, 
        DF2_balanc, 
        DF3_balanc,
        DF4_balanc
  ) |> left_join(hh_baseline)









# END ----------------------------------------------------












