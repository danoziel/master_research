
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

df_bv <- bv_22bl_A %>% left_join(rmtl_cntrl_vars)
#
m1 <-  lm(revenue22  ~ in_project + income_bl+ dist_Km_boundary +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
         df_bv )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
summary(lm(revenue22 ~ in_project, df_bv))
#
bv_22bl %>% t_test(income_bl ~ in_project, detailed = T) %>% select(p,everything())

####

crop_acre_22 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")

land_holding_bl <- BL_plotAcre %>% group_by(hh_id) %>% summarise(hh_acre_bl=sum(plot_acre,na.rm = T))

land_holding_2022= # M s / > 9.1330   [1] 9.3892
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(val_22=sum(acres,na.rm = T)) %>% 
  mutate(ecomony_vars= "land_holding" ) %>% 
  select(hh_id, ecomony_vars, val_22) %>% 
  left_join(
    land_holding_bl
  ) %>% rename(val_bl=hh_acre_bl ) %>% 
  mutate(val_22= ifelse(val_22 > 41 ,NA,val_22),
         val_bl=ifelse(val_bl > 39 ,NA,val_bl)
  )

# ----

land_2022=
  crop_acre_22 %>%
  group_by(hh_id) %>%
  summarise(acre_cult = sum(acre_cult, na.rm = TRUE),
            acre_ir = sum(acre_ir, na.rm = TRUE),
            acre_drip= sum( acre_drip , na.rm = TRUE)
  ) %>% pivot_longer(-hh_id, names_to = "variable", values_to = "value")

land_crop_2022=
  crop_acre_22 %>%
  filter(crop %in% c( 
     "Toor","Sorghum_jowar","Sugarcane", "Sunflower",
     "Bengal_gram","Greengram","Maize","Oilseeds","VegetablesANDFruits"   
    )) %>% 
  group_by(hh_id,crop) %>%
  summarise(value   = sum( acre_cult, na.rm = TRUE),.groups="drop" ) %>% 
  rename(variable=crop )

rbind(land_holding_2022, land_2022) %>% 
  rbind(land_crop_2022 )

df100 <- 
  hh_2022 %>% 
  mutate(Group=ifelse(in1_out0==1,"IN","OUT")) %>% rename(in_project=in1_out0) %>% 
  select(hh_id, Group) %>% 
  left_join( 
    rbind(land_holding_2022, land_2022) %>% 
    rbind(land_crop_2022 )
            ) %>% 
  filter(!is.na(variable)) %>% 
  group_by(Group,variable) %>% 
  summarise(mean = mean( value, na.rm = TRUE),
            sd = sd( value, na.rm = TRUE),.groups="drop" )

df100%>% 
  pivot_wider(
    names_from = Group , 
    values_from = c(mean,sd),
    names_sep = "_" ) %>% kable() %>% kable_paper()






rmtl_srvy22 %>% 
  select(farmers_hh,hh_id, starts_with( "l56")) %>% 
  select(farmers_hh,hh_id,ends_with(c("1","_2","3","4","5"))) %>% 
  pivot_longer(-c(farmers_hh,hh_id),names_to = "Q",values_to = "ans" ) %>% 
  filter(ans == 1) %>%    
  separate(Q, into = c("part1", "season", "ans_num"), sep = "_"
  ) %>% 
  group_by(farmers_hh,season) %>% 
  mutate( unique_hhid = n_distinct(hh_id)
  ) %>% 
  group_by(unique_hhid ,farmers_hh,season,ans_num) %>% 
  summarise(SUM=sum(ans)) %>% 
  mutate(pct = SUM/unique_hhid 
  ) %>% 
  group_by(farmers_hh,ans_num) %>% 
  summarise(pct=mean(pct)*100) %>% ungroup() %>% 
  mutate(
    Crop_sale_to = case_when(
      ans_num=="1" ~ "Market",
      ans_num=="2" ~ "APMC",
      ans_num=="3" ~ "Other trader",
      ans_num=="4" ~ "Gov society/NGO",
      TRUE ~ "Private company")
  )%>% 
  mutate_at(3,round) %>% 
  kable() %>% kable_paper()

.................................................................
# Where is the crop sold?  

L56_sell_to <- 
  rmtl_srvy22 %>% 
  select(farmers_hh,hh_id, starts_with( c("l56_a","l56_b"  )  )) %>% 
  pivot_longer(-c(farmers_hh,hh_id),names_to = "Q",values_to = "ans" )%>%    
  separate(Q, into = c("part1", "ab", "season"), sep = "_"
  ) %>% 
  mutate(ans=ifelse(ans %in% c("none","0","-999","Middle Man","Pink","Shops"),NA, ans )) %>% 
  mutate(
    ans_clean = case_when(
      str_starts(ans, "Hun") ~ "Hungund",
      str_starts(ans, "Hub") ~ "Hubli",
      str_starts(ans, "Bag") ~ "Bagalkot",
      str_starts(ans, "Ami") ~ "Aminagad",
      str_starts(ans, "I") ~ "Ilkal",
      str_starts(ans, "l") ~ "Ilkal",
      str_starts(ans, "Kar") ~ "Karadi",
      str_starts(ans, "No") ~ NA,
      TRUE ~ ans)
  ) %>% 
  filter(!is.na(ans_clean)) %>% 
  select(farmers_hh,hh_id,ab,ans_clean) %>% distinct() %>% 
  count(farmers_hh,ab,ans_clean
  ) %>%  
  group_by(farmers_hh,ab) %>% 
  mutate(N=sum(n),pct = n/N) %>% 
  mutate(MtMc=ifelse(pct<0.04,"Else",ans_clean)
  ) %>% 
  group_by(farmers_hh,ab,MtMc) %>% 
  summarise (pct = sum(pct)*100) %>% ungroup()%>% 
  mutate_at(4,round,0)






# 
# 6TH YEAR ________________________________________________________________----


land_cult_2022 <-  # same as in impact.1.R script
  a_plots_crop %>% 
  left_join(a_plots_size) %>% 
  select(hh_id, season, plotID, acres) %>% distinct()  %>% 
  group_by(hh_id,season) %>% summarise(acres=sum(acres,na.rm = T),.groups = "drop") %>% 
  filter(season != "KHA22") %>% 
  group_by(hh_id) %>% summarise(cult_acre_22=sum(acres,na.rm = T),.groups = "drop")

cult_acre_2016 <- BL_plotCrop  %>% select( hh_id,plotID,season) %>% distinct() %>% 
  left_join(bl6_plotAcre %>% rename(plotID=plot_num)) %>% 
  filter(season != "rabi_2014_15") %>% 
  # filter(season != "rabi_2015_16") %>% 
  group_by(hh_id) %>% 
  summarise(cult_acre_BL=sum(plot_acre))

df_income_agri_f3 <- # f3_amt is total 3 seasons
  rmtl_srvy22 %>% select(hh_id, f3_amt) %>% 
  rename(income22=f3_amt) %>% 
  mutate(income22=income22/1000)

df_revenue_22 <- 
  a_plots_revenue %>% group_by(season,hh_id,plotID) %>% 
  summarise(plotRevenue=sum(plotRevenue,na.rm = T),.groups="drop") %>% 
  filter(season !="KHA22") %>% 
  group_by(hh_id) %>% summarise(revenue22=sum(plotRevenue,na.rm = T)/1000)

df_income_f3_bl <- 
  rmtl_baseline2016  %>% 
  select(hh_id, F3_year ) %>% rename(income_bl=F3_year) %>% 
  mutate(income_bl=income_bl/1000)


### DF
bv_22bl_A <- 
  df_income_agri_f3 %>%          # X income22
  left_join(df_revenue_22) %>%   # V revenue22 q0.98
  left_join(df_income_f3_bl) %>% # V income_bl q0.99
  left_join(land_cult_2022) %>% 
  left_join(cult_acre_2016) %>% 
  left_join(land_holding_2022[,c(1,3)] %>% rename(hh_acre22=value )) %>%
  left_join(rmtl_cntrl_vars %>% select(hh_id,total_acre16)) %>% 
  left_join(land_holding_bl
            ) %>% 
  mutate(income22_acre= income22/cult_acre_22, # 099 -1.947 s/ -0.9438 s 
         income22b_acre= income22/cult_acre_BL, # 099 -1.29 s/ -0.88 s 
         revenue22_acre= revenue22/cult_acre_22, # 099 -1.74 s/ -0.03 ns # 098-1.34 s/ +0.097 ns
         revenue22b_acre= revenue22/cult_acre_BL, # 099 -0.98 ns/ -0.07 ns # 098 -0.89 ns/ -0.14 ns
         income_bl_acre=income_bl/cult_acre_BL # X
  ) %>% 
  mutate(Hincome22_acre= income22/hh_acre22,
         Hincome22b_acre= income22/hh_acre_bl,# 099 +0.993 ns/ -0.6 ns # 098 X
         Hrevenue22_acre= revenue22/hh_acre22,
         Hrevenue22b_acre= revenue22/hh_acre_bl, # < 175
         Hincome_bl_acre=income_bl/hh_acre_bl, # V  q0.99
         reve_acre_22= revenue22/total_acre16, # < 189
         reve_acre_bl=income_bl/total_acre16 #
  ) %>% mutate(across(-hh_id, ~ if_else(.x == 0, NA_real_, .x))) 

#
bv_22bl_A %>% pivot_longer(-hh_id,names_to = "vars",values_to = "val") %>% 
  group_by(vars) %>% summarise(q99=quantile(val, probs = 0.99, na.rm=T))
#
#

df_return_invest <- 
  bv_22bl_A %>% 
  select(hh_id, hh_acre22, hh_acre_bl, income_bl, revenue22, 
         reve_acre_bl, reve_acre_22) %>% 
  rename(income_22=revenue22,
         revenue_per_acre_22=reve_acre_22,
         revenue_per_acre_bl=reve_acre_bl,
         hh_acre_22 = hh_acre22) %>%
  pivot_longer(
    cols = -hh_id,
    names_to = c("economic_vars", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) %>% rename(val_22 = `22`, val_bl = bl) %>% 
  mutate(
    val_22 = case_when(
      economic_vars == "hh_acre"          & val_22 > 41    ~ NA_real_,
      economic_vars == "income"           & val_22 >= 940   ~ NA_real_,
      economic_vars == "revenue_per_acre" & val_22 >= 175   ~ NA_real_,
      TRUE ~ val_22 ),
    val_bl = case_when(
      economic_vars == "hh_acre"          & val_bl > 39   ~ NA_real_,
      economic_vars == "income"           & val_bl > 317   ~ NA_real_,
      economic_vars == "revenue_per_acre" & val_bl > 44  ~ NA_real_,
      TRUE ~ val_bl ) )
df_return_invest$economic_vars[df_return_invest$economic_vars=="income"] <- "Farming_income"

df_return_invest %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project)) %>% 
  left_join(irrigation_BL_to_22 %>% select(hh_id,drip_use_2021)) %>% # Xdrip_use_2021_22 Xdrip_use_2022
  filter(economic_vars != "hh_acre", !is.na(drip_use_2021),!is.na(val_22 )) %>% 
  summarise(Mean=mean(val_22)*1000, n=n(),.by = c(economic_vars,in_project)) %>% arrange(economic_vars)

df_return_invest %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project)) %>% 
  left_join(irrigation_BL_to_22 %>% select(hh_id,drip_use_2021)) %>% 
  filter(economic_vars != "hh_acre", in_project==1,
         !is.na(drip_use_2021),!is.na(val_22 )
         ) %>% 
  summarise(Mean=mean(val_22)*1000,
            n=n(),.by = c(economic_vars, drip_use_2021)) %>% 
  arrange(economic_vars) %>% 
  mutate(N=sum(n),.by = economic_vars, n/N) %>% 
  kable() %>% kable_paper()
  
#
#

# FIRST YEAR ________________________________________________________________----
dt18 <- 
    rmtl_midline2018 %>% select(hh_id, f3_year ,f12_year)%>% 
    rename(agri_18=f3_year, total_18=f12_year)%>% 
    right_join(hh_2022) %>% 
    filter(agri_18 > 0, agri_18 < 1000000 ) %>% 
    
    left_join(irrigation_BL_to_22 ) %>% 
    left_join(rmtl_cntrl_vars) %>% 
    left_join(df_income_agri_BL) %>% mutate(incomeBL=val_BL*1000) %>%
    mutate(revenue_per_acre_18=agri_18 /total_acre16,
           revenue_per_acre_BL=incomeBL/total_acre16
    ) 
  
  # quantile(dt18$agri_18, probs = 0.995, na.rm = TRUE)
  m1 <-  lm(agri_18   ~ in_project + dist_Km_boundary +incomeBL+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
            data = dt18 )          
  sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
  lm(agri_18  ~ in_project, dt18)  
  
  # _______________________________________________________________
  #|   agri_18 < 1M   4510 / 8218**   59,016 Rs   N 1200 / 1384   |
  #    agri_18 < 2M  -586 / 6320     65,253 Rs   N 1206 / 1390   |
  # ______________________________________________________________|

gp=dt18 %>% group_by(farmers_hh) %>% summarise(agri_income =mean(agri_18 ,na.rm=T ),n=n())
gp
gp[1,2] - gp[2,2]
sum(gp$n)

# revenue per Acre 2017 ____________

# quantile(dt_rev_18$revenue_per_acre_18, probs = 0.99, na.rm = TRUE)

dt_rev_18 <- 
  rmtl_midline2018 %>% select(hh_id, f3_year ,f12_year)%>% 
  rename(agri_18=f3_year, total_18=f12_year)%>% 
  right_join(hh_2022) %>% 
  left_join(irrigation_BL_to_22 ) %>% 
  left_join(rmtl_cntrl_vars) %>% 
  left_join(df_income_agri_BL) %>% mutate(incomeBL=val_BL*1000) %>%
  mutate(revenue_per_acre_18=agri_18 /total_acre16,
         revenue_per_acre_BL=incomeBL/total_acre16
  ) %>% filter(revenue_per_acre_18 > 0, revenue_per_acre_18 < 98000   ) %>% 
  select(in_project,revenue_per_acre_18,everything())
# quantile(dt_rev_18$revenue_per_acre_18, probs = 0.99, na.rm = TRUE)

m1 <-  lm(revenue_per_acre_18   ~ in_project + dist_Km_boundary +revenue_per_acre_BL+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = dt_rev_18 )          
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
lm(revenue_per_acre_18  ~ in_project, dt_rev_18) 

# ____________________________________________________________________
#   agri_18 < 1M   1284 / 113   9,781 Rs   N 1200 / 1375              |          |
#| revenue_per_acre_18 [99%]   1094 / 696  8,551 Rs   N 1195 / 1369   |
# ____________________________________________________________________|

dt_rev_18 %>%
  group_by(farmers_hh) %>% 
  summarise(mean(revenue_per_acre_18,na.rm=T),n=n())

dt_rev_18 %>%
  group_by(farmers_hh,drip_use_2017 ) %>% 
  summarise(mean(revenue_per_acre_18,na.rm=T),n=n())





# BASLINE ----

# F3	"Farming own or rented land: Net profit (earning-expenses)."
# F12	Total (Rs.)
# F13	According to what you indicated, your total HH income is Rs. [     ].
# F14	What is you income expectation in 2 years from now? (Rs.)


rmtl_baseline2016 %>% select(hh_id, F3_year ,F12_year) %>% 
  rename(agri_BL=F3_year, total_BL=F12_year) 

rr=rmtl_midline2018 %>% select(hh_id, f3_year ,f12_year)%>% 
  rename(agri_18=f3_year, total_18=f12_year)%>% 
  right_join(rmtl_cntrl_vars) %>% 
  left_join(irrigation_BL_to_22)


rr  %>% 
  filter(total_18 > 0, total_18 < 6265000) %>% 
  group_by(farmers_hh,drip_use_2021_22 ) %>% 
  summarise(mean(total_18))

rr %>% left_join(hh_2022) %>% 
  filter(agri_18 > 0) %>% 
  group_by(farmers_hh) %>% summarise(mean(agri_18))

# PLOT X-axis as dist_Km_boundary  ----


df_tableA <-  # is [df_croping] from impact1.R
  crop_type_acre_22BL %>%  rename(val_22=acre_crop_22, val_bl= acre_crop_BL, vars=crop_type) %>% 
  rbind(crop_type_adpt %>% rename(val_22=crop_adopt_22, val_bl= crop_adopt_BL, vars=crop_type)) %>% 
  rbind(land_holding %>%rename(val_bl= val_BL, vars=agri_vars)) %>% 
  rbind(crop_type_acre_season_22BL %>% rename(val_bl= val_BL, vars=agri_vars)) %>% 
  left_join(rmtl_cntrl_vars) 

df_220626 <- df_tableA %>% 
  filter(vars=="land_holding") %>% 
  filter(vars=="land_holding", cardinal_direction == "south")  %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1500, NA,dist_Km_boundary)
  )

# > 0 49 18.35 / 1 85 19.30
# 30730401 #105 / 39305202# 29.875 / 106414 #84 / 100696 #30.800

bin <- 0.5
bin_unit <- "Km"
df_land_bins <- df_220626 %>% filter(!is.na(dist_Km_boundary)) %>%
  mutate(dist_bin_numeric = floor(dist_Km_boundary / bin_size) * bin_size) %>%
  mutate(dist_bin = paste0(dist_bin_numeric, " : ", dist_bin_numeric + bin_size)) %>%
  summarise(
    mean_dist = mean(dist_Km_boundary),mean_val = mean(val_22, na.rm = T),
    in_project = first(in_project),n=n(),.by = c(dist_bin_numeric, dist_bin)
  ) %>%
  ungroup() %>%
  mutate(project_group = ifelse(in_project == 1, "In", "Out")) %>%
  arrange(dist_bin_numeric) %>%
  mutate(dist_bin = factor(dist_bin, levels = unique(dist_bin)))


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

# יצירת הגרף
ggplot(df_land_bins, aes(x = dist_bin, y = mean_val, fill = project_group)) +
  geom_col(color = "white") + scale_fill_manual(values = c("Out" = "gray60", "In" = "steelblue")) +
  labs(title = paste0("Bin=", bin, bin_unit ), fill="Groups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#
# _____________ANALYSIS __________ ----
mA <-  lm(val_22  ~ in_project, df_220626 )
mB <-  lm(val_22  ~ in_project +hh_haed_age+val_bl + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          df_220626 )
m1 <-  lm(val_22  ~ in_project + dist_Km_boundary +val_bl+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          df_220626 )
m2 <-  lm(val_22 ~ in_project * dist_Km_boundary + in_project * I(dist_Km_boundary^2) +val_bl + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          df_220626 )

sjPlot::tab_model(mA,mB,m1,m2 ,  terms = c("in_project","dist_Km_boundary"), show.se = F,digits = 3, show.stat  = F  ,show.ci = F)
df_220626 %>% t_test(val_22 ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)
df_220626 %>% t_test(val_bl ~ in_project, detailed = T) %>% select(estimate1, estimate2,.y. ,p,n1,n2)

summary(mA)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(mB)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(m1)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]
summary(m2)$coefficients["in_project", c("Estimate", "Pr(>|t|)")]


df_machinery %>% count(vars)

df_220626 <- df_tableB %>% left_join(rmtl_cntrl_vars) %>%
  filter(vars == "Rainfed_acre_Kharif") %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 3100, NA,dist_Km_boundary),
         val_22=ifelse(val_22 > 28, NA,val_22),
         val_22=ifelse(cardinal_direction != "south",NA,val_22)
         )
# cardinal_direction!="center"

rmtl_cntrl_vars %>% 
  summarise(mean(dist_to_boundary_m,na.rm = T),n(),.by = c(in_project, cardinal_direction))

# END ----

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

df_tableBbl <- crop_BL %>%   # in impact1.R   filter(season != "kharif_2021") %>% 
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


#

#__________________+++++++++++++++++___________________________
df_crop_22=plots_crop_2022 %>%   # in DF.22.R 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(val_22 = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(vars = paste(crop_common, season, sep = "_")) %>% 
  select(hh_id, vars, val_22) %>% 
  complete(hh_id, vars, fill = list(val_22 = 0)) %>% left_join(rmtl_cntrl_vars) 

df = crop_BL %>%   right_join (hh_2022) %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(val_bl = sum(acre_crop,na.rm = T),.groups="drop") %>%  
  mutate(vars = paste(crop_common, season, sep = "_")) %>% 
  select(hh_id, vars, val_bl) %>% 
  complete(hh_id, vars, fill = list(val_bl = 0)) %>% left_join (hh_2022) %>% 
  group_by(vars) %>% t_test(val_bl ~ farmers_hh)

fml <- val_22   ~ in_project

fits <- df_crop_22 %>% group_by(vars) %>% nest() %>%
  mutate( model = map(data, ~ lm(fml, data = .x)), coefs = map(model, tidy), stats = map(model, glance))

fits %>% unnest(coefs) %>%  filter(term == "in_project") %>% 
  select(vars, estimate, std.error, p.value) %>% ungroup() %>% 
  left_join( fits %>% unnest(stats) %>% select(vars,nobs, r.squared) 
  ) %>% 
  pivot_longer(-vars, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = vars, values_from = value ) %>% kable() %>% kable_minimal()


