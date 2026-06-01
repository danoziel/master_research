
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
crop_acre_22 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")

quantile(land_2022$acre_ir_1, probs = 0.99)

land_holding_2022=
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  mutate(variable="land_holding") %>% 
  group_by(hh_id,variable) %>% 
  summarise(value=sum(acres,na.rm = T),.groups="drop" ) %>% 
  mutate(value=ifelse(value >40,NA,value ))
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

# ----
library(ggplot2)
library(treemapify)

# To whom is the crop sold  

L56_crop_sell %>% 
  mutate(
    Crop_sale_to = case_when(
      ans_num=="1" ~ "Market",
      ans_num=="2" ~ "APMC",
      ans_num=="3" ~ "Other trader",
      ans_num=="4" ~ "Gov society/NGO",
      TRUE ~ "Private company")
  ) %>% 
  mutate_at(3,round,3) %>% 
  ggplot(
    aes(area = pct, fill = Crop_sale_to, label = paste0(Crop_sale_to, "\n", pct*100, "%"))) +
  geom_treemap(color = "white",show.legend = T) +
  geom_treemap_text(colour = "black", 
                    place = "centre", grow = TRUE, 
                    family = "serif",reflow = TRUE,
                    size = .75) +
  facet_wrap(~farmers_hh) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 12, base_family = "serif") +
  labs(
    title = "Where the crop is sold",
    subtitle = "Percentage of produce sold in each market type"
  )

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






# -----------------------------------------

land_holding_2022=
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(hh_acre22=sum(acres,na.rm = T),.groups="drop" )

land_holding_bl <- BL_plotAcre %>% group_by(hh_id) %>% summarise(hh_acre_bl=sum(plot_acre,na.rm = T))


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



bv_22bl_A <- 
  df_income_agri_f3 %>%          # X income22
  left_join(df_revenue_22) %>%   # V revenue22 q0.98
  left_join(df_income_f3_bl) %>% # V income_bl q0.99
  left_join(land_cult_2022) %>% 
  left_join(cult_acre_2016) %>% 
  left_join(land_holding_2022) %>% 
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
         Hincome_bl_acre=income_bl/hh_acre_bl # V  q0.99
  ) %>% 
  mutate(across(-hh_id, ~ if_else(.x == 0, NA_real_, .x))) %>% 
  left_join(rmtl_cntrl_vars %>% select(hh_id,in_project))

bv_22bl_A%>% 
  pivot_longer(-hh_id,names_to = "vars",values_to = "val") %>% 
  group_by(vars) %>%
  summarise(q99=quantile(val, probs = 0.99, na.rm=T))

bv_22bl <- bv_22bl_A %>%
  mutate(across(-hh_id, ~ if_else(.x > quantile(.x, 0.99, na.rm = TRUE), NA_real_, .x))) %>% 
  left_join(rmtl_cntrl_vars)

m1 <-  lm(income22_acre  ~ in_project + income_bl+ dist_Km_boundary +hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          bv_22bl )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
summary(lm(income22_acre ~ in_project, bv_22bl))

bv_22bl %>% t_test(income_bl ~ in_project, detailed = T) %>% select(p,everything())
bv_22bl %>% t_test(income_bl_acre ~ in_project, detailed = T) %>% select(p,everything())
bv_22bl %>% t_test(Hincome_bl_acre ~ in_project, detailed = T) %>% select(p,everything())

df_return_invest <- bv_22bl_A %>% 
  select(hh_id,
         hh_acre22, # 40
         hh_acre_bl, # V
         income_bl, # 301
         revenue22, # 562
         Hincome_bl_acre, # 43.2
         Hrevenue22b_acre , # 175
         ) %>% 
  rename(income_22=revenue22,
         revenue_per_acre_22=Hrevenue22b_acre,
         revenue_per_acre_bl=Hincome_bl_acre,
         hh_acre_22 = hh_acre22) %>%
  pivot_longer(
    cols = -hh_id,
    names_to = c("economic_vars", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  rename(val_22 = `22`, val_bl = bl) %>% 
  
  mutate(
    val_22 = case_when(
      economic_vars == "hh_acre"          & val_22 > 41    ~ NA_real_,
      economic_vars == "income"           & val_22 >= 562   ~ NA_real_,
      economic_vars == "revenue_per_acre" & val_22 >= 175   ~ NA_real_,
      TRUE ~ val_22
    ),
    val_bl = case_when(
      economic_vars == "hh_acre"          & val_bl > 39   ~ NA_real_,
      economic_vars == "income"           & val_bl > 349   ~ NA_real_,
      economic_vars == "revenue_per_acre" & val_bl > 44  ~ NA_real_,
      TRUE ~ val_bl # 
    )
  ) 

df_economy <- df_return_invest %>% left_join(rmtl_cntrl_vars)



# library(rstatix)
df_economy %>%
  group_by(economic_vars) %>%
  t_test(val_bl ~ in_project,detailed = T) %>% 
  select(economic_vars, estimate ,estimate2, estimate1 ,p) %>% 
  rename("IN"= estimate2, "OUT"=estimate1)









# -----------------------------------------

rev2022 <- 
  a_plots_revenue %>% group_by(season,hh_id,plotID) %>% 
  summarise(plotRevenue=sum(plotRevenue,na.rm = T),.groups="drop") %>% 
  filter(season !="KHA22") %>% 
  group_by(hh_id) %>% summarise(income22=sum(plotRevenue,na.rm = T)) %>% ungroup() %>% 
  left_join(rmtl_cntrl_vars) %>% 
  left_join(df_income_agri_BL) %>% 
  mutate(incomeBL=val_BL, income22 =income22 /1000) %>% 
  mutate(revenue_per_acre_22=income22/total_acre16,
         revenue_per_acre_BL=incomeBL/total_acre16
  ) 

rev2022 %>% 
  select(hh_id,
         income22, revenue_per_acre_22,
         incomeBL, revenue_per_acre_BL        )

m1 <-  lm(income22  ~ in_project + dist_Km_boundary +incomeBL+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          rev2022 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
summary(lm(income22 ~ in_project, rev2022))
t.test(incomeBL ~ in_project, rev2022)



quantile(rev2022$income22, probs = 0.99) # 938984.2 
quantile(rev2022$income22, probs = 0.98) # 561960 
quantile(rev2022$incomeBL, probs = 0.99, na.rm = TRUE) # 317500
format(quantile(rev2022$incomeBL, probs = 0.98, na.rm = TRUE), 
       scientific = FALSE) # 300000
rev22 <- rev2022 %>% select(incomeBL,everything()) %>% 
  mutate(
    income22=ifelse(income22 > 938, NA,income22),
    incomeBL=ifelse(incomeBL > 317, NA,incomeBL),
  )
m1 <-  lm(income22  ~ in_project + dist_Km_boundary +incomeBL+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = rev22 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
summary(lm(income22 ~ in_project, rev22))
rev22 %>% t_test(incomeBL ~ in_project, detailed = T)
#__________________________________________________________
#|   98%    980 / 8300     94,356 Rs     N 1251 / 1471    |
#    99%  -4808 / 4621    102,818 Rs     N 1265 / 1486    |
# _______________________________________________________



rev22 %>% left_join(irrigation_BL_to_22 ) %>% 
  group_by(farmers_hh ) %>% summarise(mean(income22,na.rm=T),n=n())

rev22 %>% left_join(irrigation_BL_to_22 ) %>% 
  group_by(farmers_hh,drip_use_2021 ) %>% 
  summarise(mean(income22,na.rm=T),n=n())

# revenue_per_acre_22_________
quantile(rev2022$revenue_per_acre_22, probs = 0.99,na.rm = T) # 146793.3  
quantile(rev2022$revenue_per_acre_22, probs = 0.98,na.rm = T) # 100123.9 
quantile(rev2022$revenue_per_acre_BL, probs = 0.99, na.rm = TRUE) # 43216.16 
quantile(rev2022$revenue_per_acre_BL, probs = 0.98, na.rm = TRUE) # 35466.67 
rev_22 <- rev2022 %>% filter(income22 < 940, revenue_per_acre_22< 189)
m1 <-  lm(revenue_per_acre_22  ~ in_project + dist_Km_boundary +revenue_per_acre_BL+hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property+ livestock_dairy + Bullock + Tractor + Plough +Thresher + Seed_drill + Motorcycle + Fridge,
          data = rev_22 )
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F  ,show.ci = F)
lm(revenue_per_acre_22 ~ in_project, rev_22)
# _______________________________________________________
#|   98%   170 / 698   13,525 Rs   N 1235 / 1442         |
#    99%   173 / 557   13,525 Rs   N 1245 / 1453         |
# _______________________________________________________


rev_22 %>% group_by(farmers_hh ) %>% summarise(mean(revenue_per_acre_BL,na.rm=T),n=n())

rev_22 %>% left_join(irrigation_BL_to_22 ) %>% 
  group_by(farmers_hh ) %>% 
  summarise(mean(revenue_per_acre_22,na.rm=T)*1000,n=n())

rev_22 %>% left_join(irrigation_BL_to_22 ) %>% 
  group_by(farmers_hh,drip_use_2021 ) %>% 
  summarise(mean(revenue_per_acre_22,na.rm=T)*1000,n=n())



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





