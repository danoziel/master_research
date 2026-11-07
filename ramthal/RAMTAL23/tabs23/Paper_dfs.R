# 
#|     Subdata to paper analysis
#|    ==================================

library(haven)


# ==============================================================================
# Household irrigation adoption methods [df:irrigation_BL_to_22] ----
# ==============================================================================

# Irrigation adoption by method over the study period: baseline 2016 - endline 2022 (binary indicators)

## Years 2021-2022 ----
# Database: 2022 Endline survey

priority <- c("drip", "flood", "sprinkler", "rain")

ir_methods_2021_2022 <- a_irri_rain_method %>%
  mutate(irri_method = case_match(irri_method, c("furrows", "hose") ~ "flood",.default = irri_method
  ),irri_method = factor(irri_method, levels = priority)
  ) %>%
  arrange(hh_id, irri_method) %>%
  group_by(hh_id) %>% slice(1) %>%
  rename(ir_method_2021_2022 = irri_method) %>%
  select(hh_id, ir_method_2021_2022) %>%
  ungroup()

irrigation_2021_22 <- a_irri_rain_method %>%
  distinct(hh_id, season, irri_method) %>%
  mutate(
    is_drip  = irri_method == "drip",
    is_flood = irri_method %in% c("flood", "furrows", "hose"),
    is_irri  = irri_method != "rain",
    s_2021   = season %in% c("kharif_2021", "rabi_2021_22"),
    s_2022   = season %in% c("kharif_2022", "rabi_2021_22")
  ) %>%
  group_by(hh_id) %>%
  summarise(
    drip_use_2021    = as.integer(any(is_drip & s_2021)),
    drip_use_2022    = as.integer(any(is_drip & s_2022)),
    drip_use_2021_22 = as.integer(any(is_drip)),
    flood_use_2021    = as.integer(any(is_flood & s_2021)),
    flood_use_2022    = as.integer(any(is_flood & s_2022)),
    flood_use_2021_22 = as.integer(any(is_flood)),
    ir_use_2021    = as.integer(any(is_irri & s_2021)),
    ir_use_2022    = as.integer(any(is_irri & s_2022)),
    ir_use_2021_22 = as.integer(any(is_irri)),.groups = "drop"
  )  %>% left_join(ir_6methods_2021_2022 ) 

## Years 2018-2020 ----
# Database: 2022 Endline survey

# Inspect original survey value labels to verify numeric coding of irrigation methods
attr(rmtl_srvy22$l48a_y_18_1, "labels")

irrigation_2018_2020 <- rmtl_srvy22 %>%
  select(hh_id, starts_with("l48a_y"), -contains("other")) %>%
  pivot_longer(
    cols = -hh_id,
    names_to = "observation",
    values_to = "irri_method"
  ) %>%
  separate(observation, into = c("L48a", "y", "year_ir", "crop15"), sep = "_") %>%
  mutate(
    year_ir   = as.numeric(year_ir) + 2000,
    irri_code = as.numeric(irri_method)
  ) %>%
  group_by(hh_id, year_ir) %>%
  summarise(
    # Primary method hierarchy: drip (3) > flood (1, 2, 6) > sprinkler (4) > rain
    ir_method = case_when(
      any(irri_code == 3, na.rm = TRUE)          ~ "drip",
      any(irri_code %in% c(1, 2, 6), na.rm = TRUE) ~ "flood",
      any(irri_code == 4, na.rm = TRUE)          ~ "sprinkler",
      TRUE                                       ~ "rain"
    ),
    drip_use  = as.integer(any(irri_code == 3, na.rm = TRUE)),
    flood_use = as.integer(any(irri_code %in% c(1, 2, 6), na.rm = TRUE)),
    ir_use    = as.integer(any(irri_code %in% c(1, 2, 3, 4, 6), na.rm = TRUE)),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from = year_ir,
    values_from = c(ir_method, drip_use, flood_use, ir_use),
    names_glue = "{.value}_{year_ir}"
  )

## Year 2017 ----
# Database: 2018 MIDline survey
# irri_method:  1 Flood | 2 Furrows | 3 Drip | 4 Sprinkler | 5 Manual | 6 Hose |
irrigation_2017 <- 
  ml18_irri_methods %>% select(hh_id,irri_method) |> 
  rename(ir_method_2017=irri_method  ) %>%
  mutate(
    drip_use_2017=ifelse(ir_method_2017==3,1,0),
    flood_use_2017  =ifelse(ir_method_2017 %in% c(1,2,5,6),1,0),
    ir_use_2017 = ifelse(ir_method_2017 %in% c(1:6),1,0)
  ) 

## Year Baseline 2016 ----
# Database: 2016 Baseline survey

priority <- c("drip", "flood", "sprinkler", "rain")

irrigation_BL <- BL_2015_16_crop_IRsource_IRmethod %>%
  select(hh_id, irri_method) %>%
  mutate(
    irri_clean = tolower(irri_method),
    irri_clean = case_match( irri_clean, c("manual", NA) ~ "flood",.default = irri_clean),
    irri_clean = factor(irri_clean, levels = priority)
  ) %>%
  arrange(hh_id, irri_clean) %>%
  group_by(hh_id) %>% slice(1) %>%
  rename(ir_method_BL = irri_clean) %>%
  mutate(
    drip_use_BL  = as.integer(ir_method_BL == "drip"),
    flood_use_BL = as.integer(ir_method_BL == "flood"),
    ir_use_BL    = as.integer(ir_method_BL != "rain")
  ) %>%
  select(hh_id, ir_method_BL, drip_use_BL, flood_use_BL, ir_use_BL) %>%
  ungroup()


# irrigation_BL_to_22  ----

irrigation_BL_to_22_OLD <-  irrigation_BL_to_22

# Merge all periods and compute aggregate indicators across years
irrigation_BL_to_22 <- list(
  irrigation_2021_22,
  irrigation_2018_2020,
  irrigation_2017,
  irrigation_baseline
) %>%
  purrr::reduce(full_join, by = "hh_id") %>%
  mutate(
    # 1 if used in ANY period, 0 otherwise (treats NA as FALSE)
    flood_use_6y = as.integer(if_any(starts_with("flood_use"), ~ .x == 1 & !is.na(.x))),
    drip_use  = as.integer(if_any(starts_with("drip_use"),  ~ .x == 1 & !is.na(.x))),
    ir_use_6y = as.integer(if_any(starts_with("ir_use"),    ~ .x == 1 & !is.na(.x)))
  ) %>% 
  left_join(rmtl_srvy22 %>% select(mm5,hh_id)) %>%
  mutate(drip_use_6y = case_when( 
    drip_use == 1 | mm5 == 1 ~ 1L,
    is.na(drip_use) & is.na(mm5) ~ NA_integer_, T ~ 0L
    ))





# drip_irrigated_acre ----
# 
# rmtl_baseline2016
# BL_plotAcre
# bl_crop_plot_3s
# a_plots_size

a_plots_revenue
a_plots_crop

# Impact: ecomonic  ----


# df_return_invest        ----

land_holding_bl <- BL_plotAcre %>% group_by(hh_id) %>% summarise(hh_acre_bl=sum(plot_acre,na.rm = T))

df_income_f3_bl <- 
  rmtl_baseline2016  %>% 
  select(hh_id,F3_source, F3_month ,F3_year ) |> 
  mutate(F3_year=ifelse(F3_year==0,NA,F3_year),
         income_bl=(F3_month + F3_year)/1000) |> 
  select(hh_id,income_bl )

df_revenue_22 <- 
  a_plots_revenue %>% group_by(season,hh_id,plotID) %>% 
  summarise(plotRevenue=sum(plotRevenue,na.rm = T),.groups="drop") %>% 
  filter(season !="KHA22") %>% 
  group_by(hh_id) %>% summarise(revenue22=sum(plotRevenue,na.rm = T)/1000)


bv_revenue_income_22bl <- 
  df_revenue_22 %>%
  left_join(df_income_f3_bl) %>% 
  left_join(land_holding_bl) %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,total_acre16)) |> 
  mutate(
    reve_acre_22= revenue22/total_acre16,
    reve_acre_bl=income_bl/hh_acre_bl
  )

df_return_invest <- 
  bv_revenue_income_22bl %>% 
  select(hh_id,income_bl, revenue22, reve_acre_bl, reve_acre_22) %>% 
  rename(income_22=revenue22,
         revenue_per_acre_22=reve_acre_22,
         revenue_per_acre_bl=reve_acre_bl) %>%
  pivot_longer(
    cols = -hh_id,
    names_to = c("economic_vars", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) %>% rename(val_22 = `22`, val_bl = bl)

# df_income_NonCrop       ----

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
  summarise(income_NonCrop = sum(income22,na.rm = T)/1000,.groups = "drop")%>% 
  rename(val_bl=income_NonCrop)

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
  rename(val_22=income_NonCrop)

df_income_NonCrop <- df_income_NonCrop_22 %>% left_join(df_income_NonCrop_bl) %>% rename(vars=income_type) 

# df_inputs     ----

land_cult_2022 <-
  a_plots_crop %>% 
  left_join(a_plots_size) %>% 
  select(hh_id, season, plotID, acres) %>% distinct()  %>% 
  group_by(hh_id,season) %>% summarise(acres=sum(acres,na.rm = T),.groups = "drop") %>% 
  filter(season != "KHA22") %>% 
  group_by(hh_id) %>% summarise(cult_acre_22=sum(acres,na.rm = T),.groups = "drop")

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

df_inputs <- df_inputs_type_22 %>% left_join(df_inputs_type_bl)



# df_labor_days ----


labor_days_22 <-
  rmtl_srvy22 %>% 
  select(hh_id, starts_with("l76")) %>% 
  select(!where(is.character)) %>% 
  pivot_longer(cols = -hh_id,names_to = "season",values_to = "days_22") %>% 
  filter(!is.na(days_22)) %>% 
  mutate(
    season = stringr::str_remove(season, "_hh.*"),
    season = stringr::str_replace(season, "l76_[a-zA-Z_]+(?=[0-9])", "season_")
  ) %>% 
  filter(season != "season_3") %>% 
  group_by(hh_id) %>% 
  summarise(labor_days_total = sum(days_22, na.rm = TRUE),.groups = "drop") %>%
  pivot_longer(cols = -hh_id,names_to = "vars",values_to = "val_22"
  )

labor_days_BL <-
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with("D43_uasge"),-ends_with("_0")) %>% 
  pivot_longer(cols = -hh_id,names_to = c("observation"),values_to = "val_bl") %>% 
  separate(observation, into = c("D" ,"pn","season", "plotID"), sep = "_") %>%
  filter(val_bl >= 0,season != "3" ) %>% 
  select(hh_id ,season ,plotID ,val_bl) %>%   
  group_by(hh_id) %>%
  summarise(labor_days_total=sum(val_bl)) %>% 
  left_join(rmtl_baseline2016 %>% select(hh_id,D3) ) %>% 
  mutate(labor_days_total=labor_days_total*D3) %>% 
  mutate(across(everything(), ~na_if(., 0))) %>% 
  select(hh_id ,# labor_days_perAcre ,
         labor_days_total) %>% 
  pivot_longer(-hh_id, names_to = "vars", values_to = "val_bl"
  ) 

df_labor_days <- labor_days_22 %>% left_join(labor_days_BL) 








