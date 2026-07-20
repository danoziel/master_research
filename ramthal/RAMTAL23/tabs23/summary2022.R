
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

df_bv <- df_return_invest %>% left_join(rmtl_cntrl_vars)
#
m1 <- lm(val_22 ~ in_project, df_bv |> filter(economic_vars != "Farming_income" ) )

m3 <-  lm(val_22  ~ in_project + dist_Km_boundary + in_project * dist_Km_boundary + val_bl +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          df_bv |> filter(economic_vars == "Farming_income" ) )
sjPlot::tab_model(m3 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
summary(lm(revenue22 ~ in_project, df_bv))
#
bv_22bl %>% t_test(income_bl ~ in_project, detailed = T) %>% select(p,everything())

#### Scenario D
#### Annual Cultivated Area (GCA) for HH Adopting Drip 2021
df_tableA |> left_join(irrigation_BL_to_22) |> 
  left_join(rmtl_cntrl_vars) |> 
  filter(vars=="Y202122_acre", in_project==1, !is.na(val_22) ) |> 
  summarise(mean(val_22),n=n(),.by =  drip_use_2021)

df_return_invest |> left_join(irrigation_BL_to_22) |> left_join(rmtl_cntrl_vars) |> 
  summarise(mean(val_22,na.rm=T),.by = c(economic_vars,in_project))

df_return_invest |> left_join(irrigation_BL_to_22) |> left_join(rmtl_cntrl_vars) |> filter(in_project==1, !is.na(val_22) ) |> summarise(mean(val_22),n=n(),.by = c(economic_vars,drip_use_2021)) |> arrange(economic_vars) |> group_by(economic_vars) |> mutate(N=sum(n),n/N ) |> 
  kableExtra::kable() |> kableExtra::kable_classic() 
crop_acre_22 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")

land_holding_bl <- BL_plotAcre %>% group_by(hh_id) %>% summarise(hh_acre_bl=sum(plot_acre,na.rm = T))




# croping patter
#1
DF020726 <- plots_crop_2022 %>%   
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% mutate(acre_crop=acres/n) %>% 
  group_by(season,hh_id,crop_common) %>% 
  summarise(acre_crop = sum(acre_crop,na.rm = T ),.groups="drop"
  ) |>   pivot_wider( names_from=season, values_from=acre_crop, values_fill = NA) |>mutate( Kharif = pmax(kharif_2022 , kharif_2021, na.rm = TRUE), Rabi = rabi_2021_22
  ) |> select(hh_id, crop_common, Kharif, Rabi)  %>%
  pivot_longer(cols = c(Kharif, Rabi), names_to = "season", values_to = "acre_crop"
  ) |> distinct() |> 
  summarise(acre_crop=mean(acre_crop,na.rm=T),.by=c(hh_id,crop_common)
            ) 
#2
DF020726_B <- DF020726 |> 
  complete(hh_id, crop_common , fill = list(acre_crop = 0)) |> 
  left_join(hh_2022) |> 
  summarise(acre_crop=mean(acre_crop,na.rm=T),.by=c(in1_out0,crop_common))
#3
DF020726 |> complete(hh_id, crop_common , fill = list(acre_crop = 0)) |> 
  summarise(Overall_sample=mean(acre_crop,na.rm=T),.by=c(crop_common)) |> 
  left_join(DF020726_B |> filter(in1_out0==1) |> rename(in_project=acre_crop) |> select(-in1_out0) ) |> 
  left_join(DF020726_B |> filter(in1_out0==0) |> rename(out_project=acre_crop)|> select(-in1_out0) ) |> 
  arrange(desc(Overall_sample )) |> 
  kbl(caption = "Cultivated acre for major crop | endline 2021-2022") |> kable_minimal()
  
# DF020726 |> filter(acre_crop<=0)
#4
DF020726_C <- DF020726 |> count(hh_id, crop_common) |> 
  complete(hh_id, crop_common , fill = list(n = 0)) |> 
  left_join(hh_2022) |> 
  summarise(pct=mean(n,na.rm=T),.by=c(in1_out0,crop_common))
#5
DF020726 |> count(hh_id, crop_common) |> 
  complete(hh_id, crop_common , fill = list(n = 0)) |> 
  summarise(Overall_sample=mean(n,na.rm=T),.by=crop_common) |> 
  left_join(DF020726_C |> filter(in1_out0==1) |> rename(in_project=pct) |> select(-in1_out0) ) |> 
  left_join(DF020726_C |> filter(in1_out0==0) |> rename(out_project=pct)|> select(-in1_out0) ) |> 
  arrange(desc(Overall_sample )) |> 
  kbl(caption = "% HH for major crop | endline 2021-2022") |> kable_minimal()

# BL

#1
DF020726_BL <- crop_BL %>%   # in impact1.R 
  group_by(hh_id, season, plot_num) %>% mutate(n=n()) %>% ungroup() %>% mutate(acre_crop=plot_acre/n) %>% 
  group_by(season, hh_id, crop_common) %>% 
  summarise(acre_crop = sum(acre_crop, na.rm = T), .groups="drop") %>% 
  pivot_wider( names_from=season, values_from=acre_crop, values_fill = NA) %>% 
  mutate( Rabi = pmax(rabi_2014_15, rabi_2015_16, na.rm = TRUE), Kharif=kharif_2015
  )%>%
  select(hh_id, crop_common, Kharif, Rabi) %>%
  pivot_longer(cols = c(Kharif, Rabi), names_to = "season", values_to = "acre_crop"
  ) |> distinct() |> 
  summarise(acre_crop=mean(acre_crop,na.rm=T),.by=c(hh_id,crop_common)
  ) 

#2
DF020726_BL_B <- DF020726_BL |> 
  complete(hh_id, crop_common , fill = list(acre_crop = 0)) |> 
  left_join(hh_2022) |> filter(!is.na(in1_out0)) |> 
  summarise(acre_crop=mean(acre_crop,na.rm=T),.by=c(in1_out0,crop_common)
            ) 

#3 
DF020726_BL |> complete(hh_id, crop_common , fill = list(acre_crop = 0)) |> 
  left_join(hh_2022) |> filter(!is.na(in1_out0)) |>
  summarise(Overall_sample=mean(acre_crop,na.rm=T),.by=c(crop_common)
            ) |> 
  left_join(DF020726_BL_B |> filter(in1_out0==1) |> rename(in_project=acre_crop) |> select(-in1_out0) ) |> 
  left_join(DF020726_BL_B |> filter(in1_out0==0) |> rename(out_project=acre_crop)|> select(-in1_out0) ) |> 
  arrange(desc(Overall_sample )) |> 
  kbl(caption = "Cultivated acre for major crop | Basline") |> kable_minimal()

# DF020726 |> filter(acre_crop<=0)
#4
DF020726_BL_C <- DF020726_BL |> count(hh_id, crop_common) |> 
  complete(hh_id, crop_common , fill = list(n = 0)) |> 
  left_join(hh_2022) |> 
  summarise(pct=mean(n,na.rm=T),.by=c(in1_out0,crop_common))
#5
DF020726_BL |> count(hh_id, crop_common) |> 
  complete(hh_id, crop_common , fill = list(n = 0)) |> 
  summarise(Overall_sample=mean(n,na.rm=T),.by=crop_common) |> 
  left_join(DF020726_BL_C |> filter(in1_out0==1) |> rename(in_project=pct) |> select(-in1_out0) ) |> 
  left_join(DF020726_BL_C |> filter(in1_out0==0) |> rename(out_project=pct)|> select(-in1_out0) ) |> 
  arrange(desc(Overall_sample )) |> 
  kbl(caption = "% HH for major crop | Basline") |> kable_minimal()






##




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
  select(hh_id, starts_with( "l56")) |> 
  select(hh_id,ends_with(c("1","_2","3","4","5")))


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


rmtl_srvy22 %>% 
  select(farmers_hh,hh_id, starts_with( "l56")) %>% 
  select(farmers_hh,hh_id,ends_with(c("1","_2","3","4","5"))) %>% 
  pivot_longer(-c(farmers_hh,hh_id),names_to = "Q",values_to = "ans" ) %>% 
  filter(ans == 1) %>%    
  separate(Q, into = c("part1", "season", "ans_num"), sep = "_"
  ) %>% 
  group_by(farmers_hh) %>% 
  mutate( unique_hhid = n_distinct(hh_id)
  ) |> ungroup() |> select(farmers_hh,hh_id, ans_num, unique_hhid ) |> distinct() |> 
  summarise(n=n(),.by = c(unique_hhid ,farmers_hh,ans_num)) |> mutate(pct=n/unique_hhid*100 )

rmtl_srvy22 %>% 
  select(hh_id, starts_with( "l56")) %>% 
  select(hh_id,ends_with(c("1","_2","3","4","5"))) %>% 
  pivot_longer(-c(hh_id),names_to = "Q",values_to = "ans" ) %>% 
  filter(ans == 1) %>%    
  separate(Q, into = c("part1", "season", "ans_num"), sep = "_"
  ) %>% 
  mutate( unique_hhid = n_distinct(hh_id)
  ) |> ungroup() |> select(hh_id, ans_num, unique_hhid ) |> distinct() |> 
  summarise(n=n(),.by = c(unique_hhid ,ans_num)) |> mutate(pct=n/unique_hhid*100 )


rmtl_srvy22 %>% select(hh_id, starts_with( "l56_a")) |> filter(l56_a_kha != "")

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
# 6TH YEAR __ןincome/revenue_________________________________________________----


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
bv_revenue_income_22bl <- 
  df_revenue_22 %>%
  left_join(df_income_f3_bl) %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,total_acre16)) |> 
  mutate(
    reve_acre_22= revenue22/total_acre16,
    reve_acre_bl=income_bl/total_acre16
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
  ) %>% rename(val_22 = `22`, val_bl = bl) %>% 
  mutate(
    val_22 = case_when(
      economic_vars == "income"           & val_22 >= 940   ~ NA_real_,
      economic_vars == "revenue_per_acre" & val_22 >= 175   ~ NA_real_,
      TRUE ~ val_22 ),
    val_bl = case_when(
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
  crop_type_adpt %>% 
  rename(val_22=crop_adopt_22, val_bl= crop_adopt_BL, vars=crop_type) %>% 
  rbind(land_holding %>%rename(val_bl= val_BL, vars=agri_vars)) %>% 
  filter(vars=="land_holding", cardinal_direction == "south")  %>% 
  mutate(dist_Km_boundary=ifelse(dist_to_boundary_m > 1500, NA,dist_Km_boundary)
  )