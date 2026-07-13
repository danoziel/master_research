
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

# balance test - t-test ----
library(rstatix)
tbl_t1 <- 
  df_reg_22  %>%
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project,cardinal_direction)
            ) %>% 
  group_by(vars) %>%
  do(tidy(t.test(val_bl ~ in_project, data = .))) %>% ungroup() %>% 
  mutate(across(c(estimate2, estimate1, p.value), ~ round(., 3))) %>% 
  select(vars,estimate2,estimate1, p.value) %>% 
  rename(In = estimate2 ,Out = estimate1) 

tbl_t2 <- 
  df_reg_22  %>%
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
# PLOT bin ----

## total farms on distanc to boundry X-axis  ----
df_reg_22 %>% count(vars)

bin <- 0.25
bin_unit <- "Km"
rmtl_cntrl_vars %>% 
  filter(!is.na(dist_Km_boundary)) %>%
  mutate(dist_bin_numeric = floor(dist_Km_boundary / bin) * bin) %>%
  mutate(dist_bin = paste0(dist_bin_numeric, " : ", dist_bin_numeric + bin)) %>%
  summarise(
    n=n(),
    in_project = first(in_project),.by = c(dist_bin_numeric, dist_bin)
  ) %>% mutate(n_group=sum(n),.by = in_project, pct=n/n_group)%>%
  mutate(project_group = ifelse(in_project == 1, "In", "Out")) %>%
  arrange(dist_bin_numeric) %>% 
  mutate(dist_bin = factor(dist_bin, levels = unique(dist_bin))) %>% 
  ggplot(
    aes(x = dist_bin, y = pct, fill = project_group)) +
  geom_col(color = "white") + scale_fill_manual(values = c("Out" = "gray60", "In" = "steelblue")) +
  labs(title = paste0("Bin=", bin, bin_unit ), fill="Groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  

## Vars on distanc to boundry X-axis  ----
#  > bin Km
bin <- 0.25
bin_unit <- "Km"
df_land_bins <- df_reg_22 %>%
  filter(!is.na(dist_Km_boundary)) %>% select(-c(5:26,28:54)) %>%
  mutate(dist_bin_numeric = floor(dist_Km_boundary / bin) * bin) %>%
  mutate(dist_bin = paste0(dist_bin_numeric, " : ", dist_bin_numeric + bin)
         ) %>%
  summarise(
    mean_val = mean(val_22, na.rm = T),sum_val = sum(val_22, na.rm = T),
    in_project = first(in_project),.by = c(dist_bin_numeric, dist_bin)
  ) %>% ungroup() %>%
  mutate(project_group = ifelse(in_project == 1, "In", "Out")) %>%
  arrange(dist_bin_numeric) %>% 
  mutate(dist_bin = factor(dist_bin, levels = unique(dist_bin))
         )
#  > bin PLOT    
ggplot(df_land_bins, aes(x = dist_bin, y = mean_val, fill = project_group)) +
  geom_col(color = "white") + scale_fill_manual(values = c("Out" = "gray60", "In" = "steelblue")) +
  labs(title = paste0("22 Bin=", bin, bin_unit ), fill="Groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)
                          ) + ylim(0, .25)

#
## PLOT bin nHH  Xaxis as boundry ----
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


## plot treemap ----


df_pct_0807 <- df_tableD |>left_join(hh_2022) |>  summarise(HH_freq = mean(val_22,na.rm=T)*100,.by = c(vars,in1_out0)) |> filter(vars=="WaterIntensive_adopted")
pct_in  <- round(df_pct_0807$HH_freq[df_pct_0807$in1_out0==1])
pct_out  <- round(df_pct_0807$HH_freq[df_pct_0807$in1_out0==0])

pct_in  <- df_pct_0807$HH_freq[df_pct_0807$in1_out0==1]
pct_out <- df_pct_0807$HH_freq[df_pct_0807$in1_out0==0]

df_crop_treemap22 <-     
  plots_crop_2022 |> filter(season != "kharif_2022") |> 
  select(hh_id, season, plotID, crop_common,plot_crop,acres) |>
  left_join(a_irri_rain_method|>select(hh_id, season,plotID,plot_crop,irri_method) ) |>
  mutate(crop_common=ifelse(crop_common=="Horticulture" ,"Vegetables",crop_common),
         crop_common=ifelse(crop_common=="Oilseeds" ,"Oil crop",crop_common)) |>  
  mutate(ir_label = ifelse(irri_method == "rain","Rainfed","Irrigated")) |> 
  filter (crop_common %in% c("Chillies","Onions","Sunflower","Oil crop", "Sugarcane", # "Wheat",
                             "Vegetables")) |> select(hh_id,crop_common,ir_label) |> distinct(
  ) |> 
  left_join(rmtl_16_18_22_sample %>% select(hh_id,sample)) |> 
  add_count(sample, wt = n_distinct(hh_id), name = "n_group") |>
  count(crop_common,ir_label,sample,n_group) |>
  mutate(pct=n/n_group*100) |> 
  select(sample,ir_label,pct,crop_common) 
  


library(treemapify)

df_crop_treemap22 |> 
  rbind( df_crop_treemap22 |> summarise(pct=sum(pct),.by = c(sample,ir_label)) |> filter(ir_label =="Irrigated" ) |> mutate(crop_common="Irrigated crops",ir_label ="Rainfed")
         ) |> mutate(
           pct=ifelse(sample==1, pct*pct_in/100 ,pct*pct_out/100)
           ) |> 
  mutate( sample = ifelse(sample==1,"In","Out"),
          ir_label = paste0(sample, "-", ir_label), 
          box_label = paste0(crop_common, "\n", round(pct, 1),"%")
          ) |> 
  ggplot(aes(area = pct, fill = crop_common, subgroup = box_label)) +
  geom_treemap(color = "#FCFBF9", size = 2) +
  geom_treemap_subgroup_text(place = "center", grow = FALSE, color = "black", size = 11) +
  facet_wrap(~ ir_label) +
  scale_fill_manual(values = c(
    "Sunflower" = "#D4A373", "Oil crop" = "#E9D8A6",# "Wheat" = "#", 
    "Sugarcane" = "#EE9B00", "Vegetables" = "#BB9E01",
    "Chillies" = "#BB3E03" , "Onions" = "#CA6702","Irrigated crops" ="lightblue3"
  )) +
  labs(
    title = paste0("Percentage of farmers cultivating water-intensive crops:\nInside Project ", round(pct_in), "% and Outside Project ", round(pct_out), "%")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "gray40", size = 12),
    panel.spacing = unit(.5, "lines")
  )
  
# +++++++++ df_tableBbl +++++++++++++++++++++++++++++++++++++++++++++  ----

df_tableBbl_1 <- crop_BL %>%   # in impact1.R 
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(acre_crop = sum(acre_crop, na.rm = T), .groups="drop") %>% 
  pivot_wider( names_from=season, values_from=acre_crop, values_fill = NA) %>% 
  mutate( Rabi = pmax(rabi_2014_15, rabi_2015_16, na.rm = TRUE), Kharif=kharif_2015
  )%>%
  select(hh_id, crop_common, Kharif, Rabi) %>%
  pivot_longer(cols = c(Kharif, Rabi), names_to = "season", values_to = "acre_crop"
               ) |> filter(!is.na(acre_crop))

toor_oppositesBL <- df_tableBbl_1 %>%
  filter(crop_common == "Toor") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
jowar_oppositesBL <- df_tableBbl_1 %>%
  filter(crop_common == "Sorghum_jowar") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
Sugarcane_oppositesBL <- df_tableBbl_1 %>%
  filter(crop_common == "Sugarcane") %>%
  mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))

df_tableBbl <- 
  bind_rows(df_tableBbl_1, toor_oppositesBL) %>% 
  bind_rows(jowar_oppositesBL) %>%  bind_rows(Sugarcane_oppositesBL) %>%  
  mutate(season = case_when(crop_common == "Greengram" ~ "Kharif" ,T~ season )) %>%  
  mutate(season = case_when(crop_common == "Bengal_gram" ~ "Rabi" ,T~ season )
  ) %>%  
  mutate(crop_type = case_when( crop_common %in% c(
    "Chillies","Onions","Sunflower","Oilseeds", "Sugarcane","Wheat", "Horticulture","Vegetables"
  ) ~ "WaterIntensive_acre", TRUE ~ "Rainfed_acre")
  ) %>% 
  summarise(val_bl = sum(acre_crop),.by = c(season, hh_id, crop_type)
  ) %>% 
  mutate(vars = paste(crop_type, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0))

# +++++++++ df_tableB   ++++++++++++++++++++++++++++++++++++++++++++++  ----

df_tableB_1 <- plots_crop_2022 %>%   # in DF.22.R 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif"))  %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop"
  )

# toor_opposites <- df_tableB_1 %>%
#   filter(crop_common == "Toor") %>%
#   mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
# jowar_opposites <- df_tableB_1 %>%
#   filter(crop_common == "Sorghum_jowar") %>%
#   mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))
# Sugarcane_opposites <- df_tableB_1 %>%
#   filter(crop_common == "Sugarcane") %>%
#   mutate(season = if_else(season == "Kharif", "Rabi", "Kharif"))

df_tableB <- df_tableB_1 |> 
  # bind_rows(toor_opposites) %>% bind_rows(jowar_opposites) %>%  bind_rows(Sugarcane_opposites) %>%  
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

#_______ DF crop year ____ [df_tableC_]  ----

df_tableC <- 
  df_tableB |> mutate(vars = str_remove(vars, "_Kharif|_Rabi")) |> 
  summarise(val_22 = sum(val_22,na.rm = T),
            val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars)
  ) |> arrange(hh_id)

#
# +++++++++ df_tableE   ++++++++++++++++++++++++++++++++++++++++++++++ ----
df_tableE2 <- df_tableB |>
  separate_wider_delim(
    cols = vars, delim = "_", 
    names = c("crop_type","unit","season")
  ) |> 
  mutate(vars = paste(season,unit, sep = "_")) |> 
  summarise(val_22 = sum(val_22,na.rm = T),
                   val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars)
  ) |> arrange(hh_id)


df_tableE1 <- df_tableE2 |> mutate(vars="Y202122_acre") |> 
  summarise(val_22 = sum(val_22,na.rm = T),
            val_bl = sum(val_bl,na.rm = T),.by = c(hh_id,vars)
  ) |> arrange(hh_id)# |> left_join(hh_2022)


#




#
# +++++++++ df_tableD   ++++++++++bcrop adopt ++++++++++++++++++++++++++++++++++++ ----


df_tableD <- df_tableC %>%
  mutate(
    vars = sub("acre", "adopted", vars),
    val_22 = +(val_22 != 0),
    val_bl = +(val_bl != 0)
  )
#
# ..........................................................
df_tableD |>left_join(hh_2022) |>  summarise(mean(val_22,na.rm=T),.by = c(vars,in1_out0))|> as.data.frame()
# ..........................................................




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
df_tableE2  %>% count(vars)
df_tableE1  %>% count(vars)


df_220626 <- df_tableE1  %>% filter(vars=="Y202122_acre") %>% left_join(rmtl_cntrl_vars) |> 
  mutate(
    val_22=ifelse(vars=="Y202122_acre" & val_22 > 33, NA,val_22))
    
df_220626_1 <- df_tableE1  %>% filter(vars=="Y202122_acre") %>% left_join(rmtl_cntrl_vars) %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1600, NA,dist_Km_boundary)
  )%>% filter(cardinal_direction =="south") |> 
  mutate(
    val_22=ifelse(vars=="Y202122_acre" & val_22 > 33, NA,val_22))

mA <-  lm(val_22  ~ in_project, df_220626 )
mB <-  lm(val_22  ~ in_project +hh_haed_age+val_bl + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626 )

m2 <-  lm(val_22  ~ in_project +hh_haed_age+val_bl + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626_1 )
m3 <-  lm(val_22  ~ in_project + dist_Km_boundary+in_project*dist_Km_boundary +val_bl+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,df_220626_1)

summary(mA)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(mB)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]

summary(m2)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(m3)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]

sjPlot::tab_model(mA,mB, m2,m3, terms = c("in_project","dist_Km_boundary"), show.se = F,digits = 3, show.stat  = F  ,show.ci = F)

# library(rstatix)
df_220626 %>% t_test(val_22 ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)
df_220626 %>% t_test(val_bl ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)


df_tableD %>% count(vars)
df_tableB %>% count(vars)
df_tableC  %>% count(vars)
df_tableE2  %>% count(vars)
df_tableE1  %>% count(vars)

df_tableA  %>% count(vars)
# _____________{df_tableA[]  cultivated land__________ ----

df_tableA <- 
  df_tableC %>% 
  rbind(df_tableB) %>% 
  rbind(df_tableD) %>% 
  rbind(df_tableE2) %>% 
  rbind(df_tableE1 ) |> 
  mutate(
    val_22=ifelse(vars=="Y202122_acre" & val_22> 79 ,NA,val_22),
    
    val_22=ifelse(vars=="Rabi_acre" & val_22> 46 ,NA,val_22),
    val_22=ifelse(vars=="Kharif_acre" & val_22> 56 ,NA,val_22), 
    
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
    val_bl=ifelse(vars=="Y202122_acre" & val_bl> 65, NA,val_bl),  # 18.1
    
    val_bl=ifelse(vars=="WaterIntensive_acre" & val_bl> 26, NA,val_bl),  # 18.1
    val_bl=ifelse(vars=="Rainfed_acre" & val_bl > 40, NA,val_bl),
    val_bl=ifelse(vars=="Rainfed_acre_Rabi" & val_bl >= 40, NA,val_bl),
    val_bl=ifelse(vars=="WaterIntensive_acre_Rabi" & val_bl> 22,NA,val_bl),
    val_bl=ifelse(vars=="WaterIntensive_acre_Kharif" & val_bl> 15,NA,val_bl),
    val_bl=ifelse(vars=="Rainfed_acre_Kharif" & val_bl> 16,NA,val_bl)
  ) 
#
df_tableA %>% count(vars)

df_reg_22 <- df_tableA %>% left_join(rmtl_cntrl_vars)

df_reg_22  %>% count(vars)
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

df_reg_22 <- 
  df_assets |> 
  rbind(df_land_holding) |> 
  mutate(val_22=ifelse(vars=="land_holding" & val_22 > 39 ,NA,val_22)
         ) |> left_join(rmtl_cntrl_vars)

#
library(dplyr)
library(tidyr)
library(DT)

df_assets |> left_join(rmtl_cntrl_vars) |> 
  filter(in_project == 1, cardinal_direction=="south",dist_to_boundary_m > 1600,
    vars %in% c("Tractor","Thresher","Seed_drill") ) |> 
  select(hh_id, vars, val_22) %>% 
  pivot_wider(names_from = vars, values_from = val_22) %>%
  select(-hh_id) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) |>   datatable(caption = "IN" )







#  inputs .................................   ----
df_inputs <- 
  df_inputs_type_22 %>% left_join(df_inputs_type_bl) %>% 
  mutate(
    val_bl=ifelse(vars =="input_mechanization" & val_bl > 3.9, NA,val_bl)
  )
df_inputs %>% count(vars)

#  labor_days  ............................   ----
labor_days <- 
  family_labor_22 %>% left_join(labor_days_BL) %>% 
  mutate( val_bl=ifelse(vars=="labor_days_total" & val_bl > 2780 ,NA,val_bl))

labor_days %>% count(vars)


#  Economic  table reg outcome  ...........   ----

df_return_invest %>% count(economic_vars)
df_income_NonCrop%>% count(vars)
df_inputs %>% count(vars)
labor_days %>% count(vars)

df_reg_22 <- 
  df_return_invest%>% rename(vars=economic_vars) %>% 
  rbind(df_income_NonCrop) %>% 
  filter(vars != c("k_income_assistance","k_income_independent")) %>%
  rbind(df_inputs) %>% 
  rbind(labor_days) %>% 
  left_join(rmtl_cntrl_vars) 

df_reg_22 %>% count(vars)


#  Social  table reg outcome  ...........   ----

df_social_22 %>% count(social_vars)
df_social_bl %>% count(social_vars)

df_reg_22 <- 
  df_social_22 %>% 
  left_join(df_social_bl) %>% rename(vars=social_vars) |> 
  filter(!vars %in% c("woman_offFarmwork_freq", "offFarm_work_freq")) %>% 
  left_join(rmtl_cntrl_vars
  )

df_reg_22 %>% count(vars)


# 1. reg model     ----

library(dplyr)
library(purrr)
library(broom)
library(tidyr)

fml_a <- val_22 ~ in_project
fml_b <- val_22 ~ in_project + val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge
fml_c <- val_22 ~ in_project + dist_Km_boundary + in_project * dist_Km_boundary + val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge

formulas_all <- list(model_a1 = fml_a, model_b1 = fml_b)
formulas_south <- list(model_b2 = fml_b, model_c2 = fml_c)
#
df_reg_22 %>% count(vars)
#
# [final_table] ________ Entire Sample __ [df_reg_22] ______________________ ----

control_mean <- df_reg_22 %>% filter(in_project==0) %>% group_by(vars) %>% summarise(Mean = mean(val_22,na.rm=T)) %>% pivot_wider (names_from = "vars", values_from = "Mean") %>% mutate(metric="control_mean") 

final_table <- map_dfr(formulas_all, function(fml) {
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

final_table_south <- map_dfr(formulas_south, function(fml) {
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
#
# [___] _______________________________________________________________  -----
# Print  _______________________________________________________________  -----
library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>%
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample") %>% kable_styling()%>%
  row_spec(c(1,6,12,17), background = "gray90") %>% 
  row_spec(c(3,8,14,19), bold = T) %>%
  row_spec(c(11,22), background = "gray70") %>%
  footnote(c(" "))


final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>% filter(metric %in% c("estimate","std.error","p.value","control_mean")  
  ) |>
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample"
  ) %>% kable_classic() |> 
  row_spec(c(1,4,8,11,14), background = "gray90") %>% 
  row_spec(c(3,6,10,13,16), bold = T) %>%
  row_spec(c(7,17), background = "gray70")





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



# Print Farming economic ________________________________________________  -----

library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>%select(model_name,metric,
              Farming_income, revenue_per_acre,
              k_income_assistance_mhh   ,k_income_independent_mhh,
              input_irrigation,	input_mechanization,	input_labor,	
              labor_days_total, labor_days_perAcre
  ) %>% 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample") %>% kable_styling()%>%
  row_spec(c(1,6,12,17), background = "gray90") %>% 
  row_spec(c(3,8,14,19), bold = T) %>%
  row_spec(c(11,22), background = "gray70") %>%
  footnote(c(" "))



# Print Assets ________________________________________________________  -----
library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) |> 
  select(model_name,metric,
         land_holding,Livestock_n,
         Farm_equipments,Tractor,Thresher,Seed_drill, # Plough
         Cars,Motorcycles, # Cycles
         Fridge, TV ) |> 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample") %>% kable_styling()%>%
  row_spec(c(1,6,12,17,22), background = "gray90") %>% 
  row_spec(c(3,8,14,19,24), bold = T) %>%
  row_spec(c(11,27), background = "gray70") %>%
  footnote(c(" "))


final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>% filter(metric %in% c("estimate","std.error","p.value","control_mean")  
  ) |> select(model_name,metric,
              WaterIntensive_acre,Rainfed_acre,
              WaterIntensive_acre_Rabi,Rainfed_acre_Rabi,
              WaterIntensive_acre_Kharif,Rainfed_acre_Kharif,
              WaterIntensive_adopted, Rainfed_adopted
  ) |> 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample"
  ) %>% kable_classic() |> 
  row_spec(c(1,4,8,11,14), background = "gray90") %>% 
  row_spec(c(3,6,10,13,16), bold = T) %>%
  row_spec(c(7,17), background = "gray70")

											


# Print table [cultivated land]_____________________________________________________________  -----

library(kableExtra)

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>% select(model_name,metric,
               WaterIntensive_acre,Rainfed_acre,
               WaterIntensive_acre_Rabi,Rainfed_acre_Rabi,
               WaterIntensive_acre_Kharif,Rainfed_acre_Kharif,
               WaterIntensive_adopted, Rainfed_adopted,
               Kharif_acre, Rabi_acre,Y202122_acre
  ) |> 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample") %>% kable_styling()%>%
  row_spec(c(1,6,12,17), background = "gray90") %>% 
  row_spec(c(3,8,14,19), bold = T) %>%
  row_spec(c(11,22), background = "gray70") %>%
  footnote(c(" "))


final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>% filter(metric %in% c("estimate","std.error","p.value","control_mean")  
               ) |> select(model_name,metric,
               WaterIntensive_acre,Rainfed_acre,
               WaterIntensive_acre_Rabi,Rainfed_acre_Rabi,
               WaterIntensive_acre_Kharif,Rainfed_acre_Kharif,
               WaterIntensive_adopted, Rainfed_adopted,
               Kharif_acre, Kharif_acre,Y202122_acre
  ) |> 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample"
      ) %>% kable_classic() |> 
  row_spec(c(1,4,8,11,14), background = "gray90") %>% 
  row_spec(c(3,6,10,13,16), bold = T) %>%
  row_spec(c(7,17), background = "gray70")








# Print Social ________________________________________________________ ------

final_table %>% 
  rbind(control_mean %>% mutate(model_name="Model_A1",metric="control_mean") %>% select(model_name, metric,everything() ) 
  ) %>% 
  rbind( final_table_south ) %>% 
  rbind(  
    control_mean_south %>% mutate(model_name="Model_A2",metric="control_mean") %>% select(model_name, metric,everything() )
  ) %>%select(model_name,metric,
              literacy_rates, edu_gen2, private_school,	
              migrants_work_seasonal,	migrants_work_permanent,	
              offFarm_work_pct, woman_offFarmwork_pct
  ) %>% 
  kbl(caption = "model a1 & b1 = Entire sample / model b2 & c2 & cc2 = South sample") %>% kable_styling()%>%
  row_spec(c(1,6,12,17), background = "gray90") %>% 
  row_spec(c(3,8,14,19), bold = T) %>%
  row_spec(c(11,22), background = "gray70") %>%
  footnote(c(" "))


# Coeff plot ----

fml <- val_22~in_project+ dist_Km_boundary+in_project * dist_Km_boundary + val_bl+ hh_haed_age+ hh_haed_gendar+
  hh_haed_edu_level+total_acre16 + housing_str321 +job_income_sourceS + govPnsin_scheme + rent_property+
  livestock_dairy + Bullock + Tractor + Plough + Thresher + Seed_drill + Motorcycle + Fridge

library(modelsummary)
library(ggplot2)
library(dplyr)
library(purrr)

models <- map(split(df_social, df_social$social_vars), ~ lm(fml, data = .x))

plot_data <- modelplot(models, draw = FALSE) %>% filter(term == "in_project") %>% mutate(model = case_when(model == "literacy_rates" ~ "Literacy \nrate", model == "edu_gen2" ~ "Education \nlevel: \n2nd Gen ", model == "private_school" ~ "Education: \nPrivate \nschool", model == "migrants_work_seasonal" ~ "Seasonal \nmigration", model == "migrants_work_permanent" ~ "Permanent \nmigration", model == "offFarm_work_pct" ~ "Off-farm \nwork", model == "woman_offFarmwork_pct" ~ "Off-farm \nwork. \nWomen", TRUE ~ model), model = factor(model, levels = c("Literacy \nrate", "Education \nlevel: \n2nd Gen ", "Education: \nPrivate \nschool", "Seasonal \nmigration", "Permanent \nmigration", "Off-farm \nwork", "Off-farm \nwork. \nWomen")), group = case_when(model %in% c("Literacy \nrate", "Education \nlevel: \n2nd Gen ", "Education: \nPrivate \nschool") ~ "Education & Literacy", TRUE ~ "Migration & Work"), rounded_est = round(estimate, 3))

ggplot(plot_data, aes(x = model, y = estimate, color = group)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") + geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.8) + geom_point(size = 2.5) + geom_text(aes(label = rounded_est), hjust = -0.25, vjust = -0.5, family = "serif", size = 3.5, show.legend = FALSE) + scale_color_manual(values = c("Education & Literacy" = "#4A6B82", "Migration & Work" = "#8C6D53")) + labs(x = "", y = "Coefficient Estimate") + theme_minimal(base_family = "serif") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black", size = 0.5), axis.line.y = element_blank(), legend.position = "none")






