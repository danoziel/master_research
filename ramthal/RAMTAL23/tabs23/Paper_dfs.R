# 
#|     Subdata to paper analysis
#|    ==================================

library(haven)

irrigation_BL_to_22
drip_irrigated_acre

rmtl_baseline2016
BL_plotAcre
bl_crop_plot_3s
a_plots_size

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








