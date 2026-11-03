# 15.09.2016

# ______________________________________________________________________________

# PROJECT: [Project Name / Manuscript Title]
# SCRIPT:  03_main_regressions.R
# PURPOSE: Estimates primary econometric specifications (Tables 3 & 4 in Paper).
# AUTHOR:  [Name / Anonymous for Peer Review]
# DATE:    [Date / Year]
# R VER:   R version 4.x
# ______________________________________________________________________________

library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

# 2.1. Regional Agricultural Practices (desc)
df_tableBbl
df_tableC_bl
df_tableD_bl
df_tableBbl_1
irrigation_BL_to_22
bl13_source_irrigate
rmtl_baseline2016
df_income_f3_bl
df_income_NonCrop_bl
land_holding_bl
df_income_f12_bl

# TABLE A1: Crop Cultivation Area and Adoption Rates
df_tableBbl_1
df_tableB_1
df_tableC
df_tableD
df_tableD_bl
rmtl_16_18_22_sample

2.5. Summary statistics and balance tests

# 3.1. Empirical Strategy

# 3.2.1. Agricultural Outcomes 
crop_BL
plots_crop_2022

# 3.2.2. Income Effects 
df_return_invest
df_income_NonCrop
df_inputs
df_labor_days
# Assets
rmtl_srvy22
rmtl_baseline2016
a_plots_size
rmtl_cntrl_vars

# 3.2.3. Education and Migration
rmtl_srvy22
rmtl_baseline2016

# 3.3. Drip Irrigation Utilization 
irrigation_BL_to_22
rmtl_cntrl_vars
crop_acre_drip_22
crop_acre_drip_BL
a_source_irri
bl13_source_irrigate

3.4. Cost Benefit Analysis 
# 4.1. Predictors of Drip Usage
4.2. Barriers to implementation

#===============================================================================
#  Regional Agricultural Practices  ----
#===============================================================================

library(tidyverse)

# Ave. land cultivated per hh in Rabi: 7.88 acres
# Average land cultivated per hh in Kharif: 4.97 acres
df_summary <- df_tableBbl %>%
  mutate(vars = str_remove(vars, "^[^_]+_[^_]+_")) %>%
  group_by(hh_id, vars) %>%
  summarise(val_bl = sum(val_bl, na.rm = TRUE), .groups = "drop") %>%
  group_by(vars) %>%
  summarise(mean_val_bl = mean(val_bl, na.rm = TRUE), .groups = "drop")


# % of gross cultivated area under "target crops": 20%
df_tableC_bl %>%
  group_by(vars) %>%
  summarise(val_bl = sum(val_bl, na.rm = TRUE), .groups = "drop") |> 
  mutate(pct_cultivated_acre_crop_type = val_bl/ sum(df_tableC_bl$val_bl, na.rm = TRUE) )


# % of households growing "target crops": 42%
# Households relying on rain (rainfed cultivation): 92%
df_tableD_bl %>% group_by(vars) %>% summarise(mean_val_bl = mean(val_bl, na.rm = TRUE), .groups = "drop")


# Share of Rabi area under chickpea: 60%
# Share of Rabi area under sorghum: 37%
# Share of Kharif area under mung: 18%
# Share of Kharif area under toor: 16%
# Share of Kharif area under cotton: 2%
# Share of Rabi area under sunflower: 16%


# Total farms
total_unique_farmers <- n_distinct(df_tableBbl$hh_id)

# Total cultivated area per season
season_totals <- df_tableBbl_1 %>% group_by(season) %>%
  summarise(season_total_acres = sum(acre_crop, na.rm = TRUE), .groups = "drop")


df_tableBbl_1 %>%
  group_by(season, crop_common) %>%
  summarise(total_acres = sum(acre_crop, na.rm = TRUE),
            total_farmers = n_distinct(hh_id),
            farmer_participation_pct = (total_farmers / total_unique_farmers),
            .groups = "drop") %>%
  left_join(season_totals, by = "season") %>%
  mutate(pct_of_season_land = (total_acres / season_total_acres) ) %>%
  group_by(crop_common) %>%
  slice_max(total_farmers, n = 1, with_ties = FALSE) %>%
  ungroup()  %>%
  select(crop_common, pct_of_season_land, farmer_participation_pct)


# Households using flood irrigation: 7%
# Households using drip irrigation: 1.2%
irrigation_BL_to_22  |> count(ir_method_BL ) |> mutate(pct = round(n/sum(n),2))

# Households relying on borewells: 3.7%
# Households relying on shared open wells: 2.6%
# Households relying on canals: 2.2%
bl13_source_irrigate |> 
  select(hh_id,irri_source_5y ) |> distinct() |>  filter(!is.na(irri_source_5y )) |> 
  count(irri_source_5y) |> mutate(pct = round(n/total_unique_farmers, 3))


# hh owning assets traction assets: 32% (tractor: 14% # bullocks: 22%)
df_bl_assets <-  rmtl_baseline2016 %>% select(hh_id,E6_1,E7_1,E8_1, E9_1,E10_1) |> pivot_longer(-hh_id, names_to = "name", values_to = "val_bl") |> mutate(val_bl=ifelse(val_bl>=1,1,0))

# hh owning traction assets: 32%
df_bl_assets %>% filter(name %in% c("E10_1","E7_1")) |> group_by(hh_id) |>  summarise(val_bl=sum(val_bl, na.rm=T)) |> mutate(val_bl=ifelse(val_bl>=1,1,0)) |>   summarise(mean(val_bl, na.rm=T))

# hh owning tractor: 14% # bullocks: 22%
# E10_1=tractor, E7_1=Bullock
df_bl_assets %>% filter(name %in% c("E10_1","E7_1")) |> group_by(name) |>  summarise(mean(val_bl, na.rm=T))

# Households owning farm animals: 51% (half)
df_bl_assets %>% filter(name != "E10_1" ) |> group_by(hh_id) |>  summarise(val_bl=sum(val_bl, na.rm=T)) |> mutate(val_bl=ifelse(val_bl>=1,1,0)) |>   summarise(mean(val_bl, na.rm=T))


# Average annual household agricultural income: ~$860 (57,800 Rs.)
# Average agricultural income per acre: $106 (7,100 Rs.)
# Farms relying exclusively on agricultural revenue: ~20%
# Farms depending on off-farm income: 80% (the remainder)
# Average total annual household income: ~$1,482 (99,600 Rs.)
df_income_f3_bl %>% mutate(vars = "Farming_income") |>  
  rename(val_bl= income_bl) |> select(hh_id, vars, val_bl) |> 
  rbind( df_income_NonCrop_bl |> rename(vars=income_type, val_bl= income_NonCrop) )|>  mutate(val_bl=ifelse(val_bl>=1,1,0)) |> 
  pivot_wider(
    names_from = vars, 
    values_from = val_bl) |> 
  mutate(both=k_income_independent+ k_income_assistance, 
         income=ifelse(both==2,"Both",
                       ifelse(k_income_independent==1,"Off-farm income",
                              ifelse(k_income_assistance ==1,"External income sources","Farming only" )))) |> 
  count(income) |> mutate(pct = n/sum(n))

df_income_f3_bl %>% rename(farming_income_bl= income_bl) |> 
  left_join(land_holding_bl) %>%  
  mutate(reve_acre_bl=farming_income_bl/hh_acre_bl) %>% 
  left_join(df_income_f12_bl) |> select(-hh_acre_bl ) |> 
  pivot_longer(-hh_id, names_to = "vars",values_to = "val_bl")%>%
  group_by(vars) %>%
  mutate( val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl)) |>  
  group_by(vars) |> summarise(mean(val_bl,na.rm=T))


# TABLE A1: Crop Cultivation Area and Adoption Rates ----

library(tidyverse)

# -------------------------------------------------------------------------
#  Step 1: Baseline Period (2016) ----
# ........................................................................

### Reshape crop data ----
#   Complete zero-acreage combinations across seasons and merge sample status
df_bl_cultivation <- 
  df_tableBbl_1 |> rename(val_bl = acre_crop, vars = crop_common)%>% select(-crop_type ) %>% 
  complete(season, hh_id, vars, fill = list(val_bl = 0)) %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) )

# Count unique households: Full sample, treatment and control
n_total_BL <- n_distinct(df_bl_cultivation$hh_id)
n_out_BL   <- n_distinct(df_bl_cultivation$hh_id[df_bl_cultivation$sample == 0])
n_in_BL    <- n_distinct(df_bl_cultivation$hh_id[df_bl_cultivation$sample == 1])

### 1. ADOPTION RATE (GROWER SHARE) ----

# ADOPTION | CROPS
# Aggregate grower adoption shares per crop
crop_adopters_summary_BL <- df_bl_cultivation |> select(-season) %>% mutate( val_bl = +(val_bl != 0)) |> distinct() %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    pct_growers_all_BL = num_growers_all / n_total_BL,
    
    # Inside the project
    num_growers_IN  = sum(val_bl > 0 & sample == 1, na.rm = TRUE),
    pct_growers_IN_BL  = num_growers_IN / n_in_BL,
    
    # Outside the project
    num_growers_OUT = sum(val_bl > 0 & sample == 0, na.rm = TRUE),
    pct_growers_OUT_BL = num_growers_OUT / n_out_BL
  ) %>% select(vars, pct_growers_all_BL ,pct_growers_IN_BL, pct_growers_OUT_BL )


# ADOPTION | CROP CATEGORY
# Aggregate grower adoption shares by crop category (Traditional vs Target)
crop_type_adopters_summary_BL <- df_tableD_bl %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) ) %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    pct_growers_all_BL = num_growers_all / n_total_BL,
    num_growers_IN  = sum(val_bl > 0 & sample == 1, na.rm = TRUE),
    pct_growers_IN_BL  = num_growers_IN / n_in_BL,
    num_growers_OUT = sum(val_bl > 0 & sample == 0, na.rm = TRUE),
    pct_growers_OUT_BL = num_growers_OUT / n_out_BL
  ) %>% select(vars, pct_growers_all_BL ,pct_growers_IN_BL, pct_growers_OUT_BL ) |> 
  mutate(vars=ifelse(vars=="Rainfed_adopted","Traditional crop","Target crop"))

### 2. CULTIVATED ACREAGE (MEAN AREA) ----

# ACRE | CROPS
# Aggregate mean acreage per crop, filtered to peak-grower season
crop_acre_summary_BL <- df_bl_cultivation %>%
  summarise( 
    num_growers_all = sum(val_bl > 0, na.rm = TRUE),
    mean_area_all_BL   = mean(val_bl, na.rm = TRUE), # full sample
    mean_area_IN_BL    = mean(val_bl[sample == 1], na.rm = TRUE),
    mean_area_OUT_BL   = mean(val_bl[sample == 0], na.rm = TRUE),
    .by = c(season, vars)
  )|> group_by(vars) %>%
  slice_max(num_growers_all, n = 1, with_ties = FALSE) %>%
  ungroup()  %>% select(-season, -num_growers_all)

# ACRE | CATEGORY
# Aggregate mean household acreage by crop category (Traditional vs Target)
crop_type_acre_summary_BL <- df_tableBbl_1 |> 
  summarise(val_bl = sum(acre_crop),.by = c(hh_id, crop_type)) %>% 
  rename(vars = crop_type) %>%  complete(hh_id, vars, fill = list(val_bl = 0)) |>   left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) ) |> summarise( 
    mean_area_all_BL   = mean(val_bl, na.rm = TRUE), # full sample
    mean_area_IN_BL    = mean(val_bl[sample == 1], na.rm = TRUE),
    mean_area_OUT_BL   = mean(val_bl[sample == 0], na.rm = TRUE),
    .by = vars
  )|> 
  mutate(vars=ifelse(vars=="Rainfed_acre","Traditional crop","Target crop"))



### 3. Combine baseline ACREAGE and ADOPTION summaries  ----

crop_summary_BL <- 
  crop_acre_summary_BL |> rbind(crop_type_acre_summary_BL) |> 
  left_join(crop_adopters_summary_BL |> rbind(crop_type_adopters_summary_BL))

print(crop_summary_BL)

# -------------------------------------------------------------------------
## Step 2: Endline Period (2022) ----
# ........................................................................

### Reshape crop data ----
# Complete zero-acreage combinations across seasons and merge sample status
df_22_cultivation <- 
  df_tableB_1 |> rename(val_22 = acre_crop, vars = crop_common) %>% 
  complete(season, hh_id, vars, fill = list(val_22 = 0)) %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) )

# Count unique households: Full sample, treatment and control
n_total_22 <- n_distinct(df_22_cultivation$hh_id)
n_out_22   <- n_distinct(df_22_cultivation$hh_id[df_22_cultivation$sample == 0])
n_in_22    <- n_distinct(df_22_cultivation$hh_id[df_22_cultivation$sample == 1])

### 1. ADOPTION RATE (GROWER SHARE) ----

# ADOPTION | CROPS
# Aggregate grower adoption shares per crop
crop_adopters_summary_22 <- df_22_cultivation |> select(-season) %>% mutate( val_22 = +(val_22 != 0)) |> distinct() %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) ) %>% 
  group_by(vars) %>%
  summarise(
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    pct_growers_all_22 = num_growers_all / n_total,
    num_growers_IN  = sum(val_22 > 0 & sample == 1, na.rm = TRUE),
    pct_growers_IN_22  = num_growers_IN / n_in,   
    num_growers_OUT = sum(val_22 > 0 & sample == 0, na.rm = TRUE),
    pct_growers_OUT_22 = num_growers_OUT / n_out
  ) %>% select(vars, pct_growers_all_22 ,pct_growers_IN_22, pct_growers_OUT_22 )

# ADOPTION | CROP CATEGORY
# Aggregate grower adoption shares by crop category (Traditional vs Target)
crop_type_adopters_summary_22 <- df_tableD %>%
  left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) ) %>%
  group_by(vars) %>%
  summarise( # Full sample
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    pct_growers_all_22 = num_growers_all / n_total_22,
    num_growers_IN  = sum(val_22 > 0 & sample == 1, na.rm = TRUE),
    pct_growers_IN_22  = num_growers_IN / n_in_22,
    num_growers_OUT = sum(val_22 > 0 & sample == 0, na.rm = TRUE),
    pct_growers_OUT_22 = num_growers_OUT / n_out_22
  ) %>% select(vars, pct_growers_all_22 ,pct_growers_IN_22, pct_growers_OUT_22 )|> 
  mutate(vars=ifelse(vars=="Rainfed_adopted","Traditional crop","Target crop")) 


### 2. CULTIVATED ACREAGE (MEAN AREA) ----

# ACRE | CROPS
# Aggregate mean acreage per crop, filtered to peak-grower season
crop_acre_summary_22 <- df_22_cultivation %>%
  summarise( 
    num_growers_all = sum(val_22 > 0, na.rm = TRUE),
    mean_area_all_22   = mean(val_22, na.rm = TRUE), # full sample
    mean_area_IN_22    = mean(val_22[sample == 1], na.rm = TRUE),
    mean_area_OUT_22   = mean(val_22[sample == 0], na.rm = TRUE),
    .by = c(season, vars)
  )|> group_by(vars) %>%
  slice_max(num_growers_all, n = 1, with_ties = FALSE) %>%
  ungroup()  %>% select(-season, -num_growers_all)

# ACRE | CATEGORY
# Aggregate mean household acreage by crop category (Traditional vs Target)
crop_type_acre_summary_22 <- df_tableC |> 
  summarise(val_22 = sum(val_22),.by = c(hh_id, vars)) |>   left_join(rmtl_16_18_22_sample %>% select(hh_id, sample) ) |> summarise( 
    mean_area_all_22   = mean(val_22, na.rm = TRUE), # full sample
    mean_area_IN_22   = mean(val_22[sample == 1], na.rm = TRUE),
    mean_area_OUT_22   = mean(val_22[sample == 0], na.rm = TRUE),
    .by = vars)|> 
  mutate(vars=ifelse(vars=="Rainfed_acre","Traditional crop","Target crop"))

### 3. Combine endline ACREAGE and ADOPTION summaries  ----

crop_summary_22 <- 
  crop_acre_summary_22 |> rbind(crop_type_acre_summary_22) |> 
  left_join(crop_adopters_summary_22 |> rbind(crop_type_adopters_summary_22))






# -------------------------------------------------------------------------
## Step 3: Synthesis (Endline & Baseline)  ----
# ........................................................................

# Merge Endline and Baseline metrics into a single comparison table

crop_summary_22BL <- left_join(crop_summary_22, crop_summary_BL)

print(crop_summary_22BL)

crop_summary_22BL |> kableExtra::kbl() |> kableExtra::kable_classic()








#===============================================================================
#  SUMMARY STATISTICS and BALANCE TESTS ----
#===============================================================================

# Cultivated land [8 items]
dt_BL_cult_acre <- df_tableC_bl %>% rbind(df_tableBbl) %>%
  group_by(vars) %>%
  mutate(
    val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl)
  ) %>% ungroup()

dt_BL_cultivated_land <- bind_rows(dt_BL_cult_acre, df_tableD_bl) 




# T-TEST DF    |||||||||||||||| ----

df_to_ttest_bl <- 
  dt_BL_cultivated_land
  bind_rows( ) |> 

  left_join(
  rmtl_baseline2016 |> select(hh_id,in1_out0,south1_north0)
)

# T-TEST TABLE |||||||||||||||| ----
# library(rstatix)
tbl_t1 <- df_reg_bl  %>%
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in1_out0, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars,estimate2,estimate1, p.value) %>% 
  rename(In = estimate2 ,Out = estimate1) 

tbl_t2 <- 
  df_reg_bl  %>%  filter(south1_north0 ==1) %>% 
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in1_out0, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars, estimate2,estimate1, p.value) %>% 
  rename(In_south = estimate2 ,Out_south = estimate1, p_south=p.value ) 

tbl_t1 %>% full_join(tbl_t2) %>% 
  kableExtra::kbl() %>% kableExtra::kable_paper()


















#===============================================================================
# IMPACT ESTIMATION                                                          ####
#===============================================================================
# Environment Setup: Data Preparation   ----
# ------------------------------------------------------------------------------
#  A |  Agriculture           ----

# 1. Baseline Data (crop_BL)
# Matching a crop to its growing season, similar to what was documented in the Endline survey.
# Divide acreage equally across intercropped plots (plot_acre / n).
# Align seasons: Kharif takes 2015; Rabi takes the maximum across 2014-15 and 2015-16.
# Classify crops into WaterIntensive_acre vs. Rainfed_acre.

df_tableBbl_1 <- crop_BL %>%   # in impact1.R 
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

# 2. Baseline: Sum acreage by season and crop type; fill uncultivated pairs with 0.
df_tableBbl <- df_tableBbl_1 |> 
  summarise(val_bl = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

# 3. Endline: Divide intercropped acreage and aggregate by crop water intensity.
df_tableB_1 <- plots_crop_2022 %>%   # in DF.22.R 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif"))  %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop"
  )

# 4. Endline: Join with baseline values.
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

# 5. Full agricultural year totals (summing across seasons).
df_tableC <- 
  df_tableB |> mutate(vars = str_remove(vars, "_Kharif|_Rabi")) |> 
  summarise(val_22 = sum(val_22,na.rm = T),
            val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars)
  ) |> arrange(hh_id)


# 6. Trim top 0.5 percentile of values within each variable to reduce outlier influence.
df_tableA_995 <- df_tableC %>% rbind(df_tableB) %>%
  group_by(vars) %>%
  mutate(
    val_bl = ifelse(val_bl > quantile(val_bl, 0.995, na.rm = TRUE), NA, val_bl),
    val_22 = ifelse(val_22 > quantile(val_22, 0.995, na.rm = TRUE), NA, val_22)
  ) %>% ungroup()

# 7. Construct binary adoption indicators (1 if acreage > 0, 0 otherwise).
df_tableD <- df_tableC %>%
  mutate(
    vars = sub("acre", "adopted", vars),
    val_22 = +(val_22 != 0),
    val_bl = +(val_bl != 0)
  )

# 8. Combine outcomes into a single dataset
Impact_Agriculture <- bind_rows(df_tableA_995, df_tableD)

# ______________________________________________________________________________

#  B |  Income Module               ----

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

# ______________________________________________________________________________

#       Assets                ----

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

# ______________________________________________________________________________

#  C |  Education & Migration ----

## 1. Household literacy rates ----
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
  summarise(literacy_15=mean(literate,na.rm=T)) %>% 
  mutate(literacy_15 = ifelse(literacy_15 <= 0,NA,literacy_15))




## 2. Second-generation educational attainment ----
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

# Baseline: Compute household-level indicator
# Reshape education level (c7)
c7_15 <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("c7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
c7_15$id_member <- sub("^C7_(\\d{1,2})","C_\\1",c7_15$id_member )

# Reshape relationship to household head (c5)
C5_15 =
  rmtl_baseline2016 %>% select(hh_id,starts_with("c5" ), -contains("os") ) %>% 
  pivot_longer(
    !hh_id, names_to = "id_member", values_to = "relation_HH_head") 
C5_15$id_member <- sub("^C5_(\\d{1,2})","C_\\1",C5_15$id_member )

# Join datasets, filter for second generation, and compute household mean
edu_level_2nd_gen_15 <- 
  left_join (c7_15,C5_15) %>% 
  mutate(edu_gen_2nd=ifelse(relation_HH_head %in% c(3,9,11:13),edu_level,NA) ) %>% 
  group_by(hh_id) %>% 
  summarise(edu_gen2_15=mean(edu_gen_2nd,na.rm=T)) %>% 
  mutate(edu_gen2_15 = ifelse(edu_gen2_15<0,NA,edu_gen2_15)) # transfer NaN to NA



## 3. Private school enrollment ----
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

# Baseline: Compute household-level indicator
# Reshape institution type (c10), classify private vs. public, and aggregate to household level
edu_private_15 = 
  rmtl_baseline2016 %>% select(hh_id,starts_with("C10" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "institut")%>% 
  mutate(institut = ifelse(!institut %in% c(1,2) ,0,institut), Private =ifelse(institut==2,1,0), public=ifelse(institut==1,1,0)) %>%
  group_by(hh_id) %>% 
  summarise(n_Private =sum(Private ), n_public=sum(public)) %>% 
  mutate(private_school = ifelse(n_Private  > 0, 1, ifelse(n_public > 0, 0, NA))) %>% 
  select(hh_id,private_school)


## 4. Work migration ----
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

# Baseline: Compute household-level indicator

# Seasonal work migration
# Count of seasonal migrants by work location outside district/state (C21).
# Non-migrant (0): 1 = Home village; 2 = Other village; 3 = Urban center within district
# Migrant (1):     4 = Urban center outside district; 5 = Other destination
C21 <- rmtl_baseline2016 %>% select(hh_id,starts_with("C21")) %>% 
  pivot_longer(-hh_id,names_to = "name",values_to = "val") %>% 
  mutate(val=ifelse(val %in% c(4,5),1,0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_seasonal =sum(val)) 

# Permanent work migration
# Count of permanent migrants whose primary reason for departure was work (c34).
# Coded 1 if moved for employment (1); 0 if marriage (2), education (3), or remaining responses
c34 <- rmtl_baseline2016 %>% 
  select(hh_id, starts_with("C34")) %>% 
  pivot_longer(-hh_id, names_to = "name", values_to = "why_left") %>%
  mutate(why_left = ifelse(!is.na(why_left) & why_left == 1, 1, 0)) %>% 
  group_by(hh_id) %>% summarise(migrants_work_permanent=sum(why_left))

# Join seasonal and permanent work migration counts
migrants_work_bl <- left_join(c34,C21)

## 5. Off-Farm work ----
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

# Baseline: Compute household-level indicator
# Main income-earning activities in the rainy season (c13)
c13 <- rmtl_baseline2016 %>% select(hh_id, starts_with("C13"))  %>% 
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "c13", values_to = "ans") %>% 
  separate(c13, into = c("c13", "rank", "hhm"), sep = "_", convert = F
  )

# Main income-earning activities in the rest of the year (c17)
c17 <- rmtl_baseline2016 %>% select(hh_id, starts_with("C17"))  %>% 
  select(hh_id, matches("^[^_]+_[^_]+_[^_]+$")) %>% 
  pivot_longer(-hh_id, names_to = "c17", values_to = "ans") %>% 
  separate(c17, into = c("c17", "rank", "hhm"), sep = "_", convert = F
  )

df_work_offFarm_bl <- 
  rbind(c13 %>% select(-c13), c17 %>% select(-c17)) %>% 
  select(hh_id,hhm, ans) %>%
  filter(!is.na(ans)) %>% 
  mutate(val= ifelse(ans %in% c(9:12),1,0 )) %>% 
  select(hh_id, hhm,val ) %>% distinct() %>% 
  group_by(hh_id) %>% summarise(val = sum(val),n=n()) %>% 
  mutate(offFarm_work_pct=val/n) %>% 
  select(hh_id,offFarm_work_pct)



## 6. Gender Off-Farm work ----
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

# Baseline: Compute household-level indicator
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
  mutate(woman_offFarmwork_pct_bl=val/n) %>% 
  select(hh_id,woman_offFarmwork_pct )


##    Impact_Social ----
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
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_22")

# Combined panel of social indicators across both rounds
Impact_Social <- df_social_22 %>% left_join(df_social_bl)

# ______________________________________________________________________________

#  D |  Drip Irrigation Utilization  ----

## Drip adoption rate ----
#  Drip adoption by year,baseline, 2017–2022 and plot in project
df_drip_use_Y_1to6 <- irrigation_BL_to_22 %>% select(hh_id, starts_with("drip"),-drip_use_BL) %>%pivot_longer(-hh_id, names_to = "vars", values_to = "val_22")
df_im_in_project <- rmtl_cntrl_vars %>% select(hh_id,im_in_project) %>% pivot_longer(-hh_id, names_to = "vars", values_to = "val_22")
df_drip_use_bl <- irrigation_BL_to_22 %>% select(hh_id, drip_use_BL) %>% rename(val_bl=drip_use_BL)

# Merge databases
df_drip_17_22 <- rbind(df_drip_use_Y_1to6,df_im_in_project) %>% left_join(df_drip_use_bl)


## Irrigated land  ----
#   Share of drip area and drip irrigated acre.
#   Merge Endline and Baseline databases
df_drip_acre_pct <- 
  crop_acre_drip_22 %>% left_join(crop_acre_drip_BL) %>% # in impact.1.R
  rename_with(~ c("hh_id", "vars", "val_22", "val_bl"))

## Water sources  ----
# Any source and project source (2016–2022)

# Endline: 
df_source_irri_22 <- a_source_irri %>%
  mutate(source_ramthal= ifelse(source_ramthal=="ramthal",1,0)) %>%
  mutate(across(c(source_borwell, source_canal, source_pond), ~ if_else(.x %in% c("ramthal", "rain"), 0, 1))) |> 
  mutate(any_source = if_else(source_borwell + source_canal + source_pond > 0, 1, 0)) |> 
  select(hh_id,source_ramthal  , any_source) |> 
  pivot_longer(cols = c(source_ramthal, any_source),names_to = "vars", values_to = "val_22")

# Baseline: 
df_source_irri_BL <- 
  bl13_source_irrigate |> mutate(ir_source_bl= ifelse(is.na(ir_source_bl ),0,1)) |> 
  summarise(val_bl=sum(ir_source_bl),.by=hh_id )|> mutate(val_bl= ifelse(val_bl==0,0,1)) |> 
  mutate(vars=c("any_source"))
df_source_irri_BL = df_source_irri_BL |> bind_rows(df_source_irri_BL |> mutate(val_bl=0, vars="source_ramthal"))


## Merge Irrigated land and Water sources databases ----
df_drip_land_and_water_source <- 
  df_drip_acre_pct %>%
  mutate(val_22=ifelse(vars=="drip_pct" & val_22==1,NA,val_22) ) %>%
  rbind( df_source_irri_22 %>% left_join(df_source_irri_BL) )
  
# ______________________________________________________________________________

# ------------------------------------------------------------------------------
# Regression Estimation and Table Export                                    ----
# ------------------------------------------------------------------------------
#   Dataframe for Analysis   ----

# Combine all modules and attach household controls
df_reg_22 <- bind_rows(
  Impact_Agriculture,
  Impact_Economic,
  df_assets,
  Impact_Social_raw,
  df_drip_17_22,
  df_drip_land_and_water_source
  ) %>% 
  left_join(rmtl_cntrl_vars)

#   Econometric Specifications  ----
# fml_bl: Regress baseline outcome on treatment indicator.
# fml_a:  Regress endline outcome on treatment indicator without controls.
# fml_b:  ANCOVA model: regress endline outcome on treatment, controlling for baseline outcome and household covariates.
# fml_c:  Spatial RDD model: regress endline outcome on treatment, boundary distance, interaction, baseline outcome, and household covariates.

fml_bl <- val_bl ~ in_project
fml_a <- val_22 ~ in_project
fml_b <- val_22 ~ in_project + val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge
fml_c <- val_22 ~ in_project + dist_Km_boundary + in_project * dist_Km_boundary + val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge

# Model grouping lists for mapping functions
formula_BL <- list(model_BL = fml_bl)
formulas_all <- list(model_a1 = fml_a, model_b1 = fml_b)
formulas_south <- list(model_b2 = fml_b, model_c2 = fml_c)

#   Functions for regression formulas  ---- 

# 1. Baseline Model Estimation (Full Sample)
#    Compute control group baseline means by outcome variable in wide format.
Intercept_bl <- 
  df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_bl,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(metric="(Intercept)", model_name="Model_BL") %>% select(model_name, metric,everything() ) 

# Fit baseline models across outcomes, extract treatment coefficients and fit statistics.
final_table_bl <-
  map_dfr(formula_BL, function(fml) {
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

# 2. Endline Model Estimation | Full Sample (Model A and B)
#    Compute control group endline means by outcome variable for full sample.
control_mean <- 
  df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 

# Fit unadjusted and ANCOVA models across outcomes and reshape extracted metrics to wide format.
final_table <- 
  map_dfr(formulas_all, function(fml) {
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

# 3. Endline Model Estimation | Southern Boundary Sample
#    Restricted to within 1.5 Km of the boundar (Model B and C)
df_reg_22_south <- 
  df_reg_22 %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1600, NA,dist_Km_boundary)
  ) %>% filter(cardinal_direction =="south")

# Compute control group endline means across outcomes for the restricted southern sample.
control_mean_south <- 
  df_reg_22_south %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )

# Fit ANCOVA and spatial discontinuity models on the southern sample and reshape extracted metrics to wide format.
final_table_south <- 
  map_dfr(formulas_south, function(fml) {
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

# 4. Output Table Compilation
#    Combine all results into a single output table
table_results <- Intercept_bl |> rbind(final_table_bl) |> 
  rbind(control_mean) |> rbind(final_table) |> 
  rbind(control_mean_south) |> rbind(final_table_south)

#   Print Results Table       ----
# Split final results by module

table_agri   <- table_results %>% select(model_name, metric, any_of(unique(Impact_Agriculture$vars)))

table_economic <- table_results %>% 
  select(model_name, metric, any_of(unique(Impact_Economic$vars))) %>% 
  rename(
    Farming_income_HH    = income,
    Input_irrigation     = input_irrigation,
    Input_labor          = input_labor,
    Input_mechanization  = input_mechanization,
    External_support     = k_income_assistance,
    Off_Farm_earnings    = k_income_independent,
    Total_days           = labor_days_total,
    Farming_income_acre  = revenue_per_acre
  )

table_assets <- table_results %>% select(model_name, metric, any_of(unique(df_assets$vars)))

table_social <- table_results %>% 
  select(model_name, metric, any_of(unique(Impact_Social$vars))) %>% 
  rename(
    `Household literacy`        = literacy_rates,
    `2nd generation education`  = edu_gen2,
    `Private school enrollment` = private_school,
    `Seasonal migration`        = migrants_work_seasonal,
    `Permanent migration`       = migrants_work_permanent,
    `Off-farm work`             = offFarm_work_pct,
    `Female off-farm work`      = woman_offFarmwork_pct
  )

table_drip_17_22 <- table_results %>% 
  select(model_name, metric, any_of(unique(df_drip_17_22$vars))) %>% 
  rename(
    `Year 1 2017`                     = drip_use_2017,
    `Year 2 2018`                     = drip_use_2018,
    `Year 3 2019`                     = drip_use_2019,
    `Year 4 2020`                     = drip_use_2020,
    `Year 5 2021`                     = drip_use_2021,
    `Year 6 2022`                     = drip_use_2022,
    `Use drip at least once 2017-22`  = drip_use_6y,
    `Reported project participation`  = im_in_project
  )

table_drip_land_and_water_source <- table_results %>% 
  select(model_name, metric, any_of(unique(df_drip_land_and_water_source$vars))) %>% 
  rename(
    `Water source: Other than Ramthal`                   = any_source,
    `Land under drip-irrigation: Acre`                   = drip_acre,
    `Land under drip-irrigation: % of the cultivated land` = drip_pct,
    `Water source: Gov. System (Ramthal)`                = source_ramthal
  )

# ==============================================================================
# Predictors of Drip Adoption -----

df1 <-
  irrigation_BL_to_22 %>% 
  select(hh_id, drip_use_6y, flood_use_6y, ir_use_BL)

df3 <-
  rmtl_cntrl_vars %>%
  select(hh_id, in_project, 
         hh_haed_age, hh_haed_gendar, literate_hh_pct, 
         total_acre16,total_livestock,
  ) %>% mutate(own_livestock_01=ifelse(total_livestock == 0,0,1))

df5 <-
  rmtl_baseline2016 %>%
  select(hh_id, F5_year, F6_year, F7_year, F10_year, F11_year) %>%
  pivot_longer(-hh_id ,names_to = "income_type_f",values_to = "income22")  %>%   
  group_by(hh_id) %>% 
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop") %>% 
  mutate(income_NonCrop=ifelse(income_NonCrop == 0,0,1) 
  )

df6 <-
  rmtl_baseline2016 %>%
  select(hh_id,I34) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  setNames(c("hh_id", "attended_to_trainings")
  )

df_project <-
  hh_2022 %>% rename(in_project =in1_out0 ) %>%
  left_join(df1) %>%
  left_join(df3) %>%
  left_join(df5) %>%
  left_join(df6) %>%
  filter(in_project == 1) %>%
  mutate(total_acre16=ifelse(total_acre16 > 39, NA, total_acre16 ),
         total_acre16=total_acre16/10, hh_haed_age=hh_haed_age/10)


# descriptive mean /SD
df_project %>% 
  select(-flood_use_6y) %>% mutate(irrigation_method="Drip") %>% rename( use_6y=drip_use_6y) %>% 
  rbind(
    df_project %>% select(-drip_use_6y) %>% mutate(irrigation_method="Flood") %>% rename( use_6y=flood_use_6y)
    ) %>%   
  filter(!is.na(use_6y)) |> 
  mutate(use_6y= ifelse(use_6y == 1, "yes","no") 
         ) %>%
  summarise(
    hh_haed_age = mean(hh_haed_age,na.rm=T)*10,
    hh_haed_gendar= mean(hh_haed_gendar,na.rm=T),
    literate_hh_pct= mean(literate_hh_pct,na.rm=T),
    attended_to_trainings =mean(attended_to_trainings,na.rm=T),
    own_livestock_01 =mean(own_livestock_01,na.rm=T),
    income_NonCrop =mean(income_NonCrop,na.rm=T),
    ir_use_BL =mean(ir_use_BL,na.rm=T),
    total_acre16=mean(total_acre16,na.rm=T)*10,.by = c(irrigation_method, use_6y)
  ) %>%
  pivot_longer(cols = -c(irrigation_method, use_6y), names_to = "name", values_to = "val") %>%
  mutate(val= ifelse(!name %in% c("hh_haed_age", "total_acre16"),val*100, val) ) %>%
  pivot_wider(names_from = c(irrigation_method, use_6y), values_from = val, names_sep = "_") %>% 
  select(name, Drip_yes, Drip_no, Flood_yes, Flood_no) %>%
  kableExtra::kable() %>%kableExtra::kable_minimal()


#  lm Model: Adoption (Ever used drip/flood)
model_DI_6y <-
  lm(drip_use_6y ~ hh_haed_age + hh_haed_gendar + literate_hh_pct + attended_to_trainings + own_livestock_01+income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )

model_F_6y <-
  lm(flood_use_6y ~ hh_haed_age + hh_haed_gendar + literate_hh_pct + attended_to_trainings + own_livestock_01+income_NonCrop + ir_use_BL + total_acre16,
     data = df_project )

sjPlot::tab_model(model_DI_6y, model_F_6y ,show.se = T,digits = 4,show.stat  = F)

# coef plot # 700X450
models_list <- list("Drip   16-22" = model_DI_6y,"Flood 16-22" = model_F_6y)
jtools::plot_summs(models_list, colors = c("#026bb9","#619847"), point.size = 4,line.size = 1.2, point.shape = 19) + theme(panel.grid.major.x = element_blank()) + geom_text(aes(label = round(estimate,2) ),size=4,position = position_dodge(1.5))

# ==============================================================================
# Barriers to implementation ----


