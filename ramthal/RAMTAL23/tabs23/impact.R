library(dplyr)
library(haven)
library(tidyr)

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )

# CROPPING PATTERN                                ----

cp16=croppin_pattern16 %>% group_by(farmers_hh,crops) %>% summarise(prob16=mean(crop01)*100) %>% mutate_at(3,round)
cp16$prob16[cp16$prob16==0] <- 1

cp18=croppin_pattern18 %>% group_by(farmers_hh,crops) %>%summarise(prob18=mean(crop01)*100) %>% mutate_at(3,round)
cp18$prob18[cp18$prob18==0] <- 1

cp22=croppin_pattern22 %>% group_by(farmers_hh,crops) %>% summarise(prob22=mean(crop01)*100) %>% mutate_at(3,round)
#        TO PLOT !  Switching to new cp_prob ----
cp_prob=full_join(cp16,cp18) %>% full_join(cp22) %>% arrange( desc(prob16))
cp_prob$prob16[is.na(cp_prob$prob16)] <- 0
cp_prob %>% as.data.frame()



list_crop %>% count(crop_type) #3
list_crop %>% count(crop_family) #8
list_crop %>% count(crop_common ) #18
list_crop %>% count(common_n_family) #25
list_crop %>% count(common_n_family_other) # 26
list_crop %>% count(crop_name) #75


#🟣 SURVEY 22
#Fig. L39 in paper
a_plots_crop %>%   # [1578 HH, in929  out649 ]
    select(hh_id,season,plotID ,crop_family,crop_common )%>% left_join(a_plots_size[,c(1,2,7)]) %>% distinct() %>%
    left_join(hh_2022) %>% 
    group_by(farmers_hh,hh_id,season,crop_common) %>% summarise(acres_hh=sum(acres)) %>%  # "hh Season Crop acre" wise
#  as.data.frame() %>%  select(season, hh_id) %>% distinct() %>% count(season)
  group_by(farmers_hh,season,crop_common) %>% summarise (acre=mean(acres_hh),n=n()) %>%  #  "Season Crop acre" wise
  mutate(prt= ifelse(farmers_hh=="inside_ramthal",n/929,n/649) ) %>% 
  mutate(prtCropSeason = paste0(round(100 * prt, 0), "%"))%>% 
  filter(prtCropSeason != "0%",prt>0.10) %>% 
  arrange(crop_common)


  
seasons <- 
  a_plots_crop[,1] %>%distinct()  %>% mutate(kha=1,rabi=1,KHA22=1) %>% 
  pivot_longer(!hh_id , names_to = "season", values_to = "count") %>% 
  select(hh_id,season) %>%     left_join(hh_2022)

cropping_veg_21_21 <- 
  a_plots_crop %>%   # [1578 HH, in929  out649 ]
    select(hh_id,season,plotID ,crop_family  )%>% left_join(a_plots_size[,c(1,2,7)]) %>% distinct() %>%
  group_by(hh_id,season,crop_family) %>% summarise(acres_hh=sum(acres,na.rm = T)) %>% 
  filter(crop_family =="Vegetables") %>% ungroup() %>% 
  right_join(seasons) %>% 
  mutate(acres_hh=ifelse(is.na(acres_hh),0,acres_hh) ) %>% 
  group_by(farmers_hh,hh_id) %>% summarise(crop01=sum(acres_hh)) %>% 
  mutate(crop01=ifelse(crop01==0,0,1),crops="Vegetables") %>% 
  select(farmers_hh,hh_id,crops,crop01)
  
cropping_21_22 <- 
  a_plots_crop %>%   # [1578 HH, in929  out649 ]
  select(hh_id,season,plotID,crop_name  )%>% left_join(a_plots_size[,c(1,2,7)]) %>% distinct() %>%
  filter(crop_name %in% c(
    "Bengal_gram", "Sorghum_jowar", "Toor","Greengram","Maize","Wheat",
    "Pearl.millet_bajra","Onions","Sunflower","Cotton","Sugarcane","Chillies")) %>% 
  group_by(hh_id,season,crop_name) %>% summarise(acres_hh=sum(acres)) %>% 
  pivot_wider(names_from = crop_name, values_from = acres_hh) %>%   right_join(full_seasons) %>% 
  ungroup() %>% select(-season) %>% 
  pivot_longer(!c(farmers_hh,hh_id ), names_to = "crops", values_to = "acre") %>% 
  group_by(farmers_hh,hh_id,crops ) %>% summarise(crop01=sum(acre,na.rm = T)) %>% 
  mutate(crop01=ifelse( crop01==0,0,1 )) %>% ungroup()

#croppin_pattern22
croppin_pattern22 <- rbind(cropping_21_22,cropping_veg_21_21)
rm(cropping_21_22,cropping_veg_21_21,seasons)

# t test
t1 <- croppin_pattern22 %>% group_by(crops) %>% t_test(crop01  ~ farmers_hh , detailed = T) 
t10 = t1[c(1,4,8,5,7,13),]
t20 = t1[c(11,10,12,2,6,9,3),]
t_L39a <- rbind( t10,t20) %>% 
    rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
    select(crops,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_L39a,title = c("Table L39a | Share of households cultivating common crops and high-value crops in 2021-2022"),
             note = c("[L39] What crops are planted on this plot?","🟩" ))

# 🟠 MIDELINE

ml18_crop_plot_3s %>% filter(season!="summer_2018") %>% 
  rename(crop_code=crop_ml18) %>% 
  left_join(list_crop) %>% 
  select(farmers_hh,hh_id,crop_name) %>%distinct() %>% count(crop_name) %>% mutate(prt=n/1603) %>% filter(prt>.01)

ml18=ml18_crop_plot_3s[,1:2] %>% distinct()
cropping_17_18 <- 
  ml18_crop_plot_3s %>% filter(season!="summer_2018") %>% 
  rename(crop_code=crop_ml18) %>% 
  left_join(list_crop) %>% 
  select(farmers_hh,hh_id,crop_name) %>%distinct() %>% 
  filter(crop_name %in% c(
    "Bengal_gram", "Sorghum_jowar", "Toor","Greengram","Maize","Wheat",
    "Pearl.millet_bajra","Onions","Sunflower","Cotton","Sugarcane","Chillies")) %>% 
  mutate(crop01=1) %>% 
  pivot_wider(names_from = crop_name, values_from = crop01) %>% 
  right_join(ml18) %>% filter(!is.na(farmers_hh)) %>% 
  pivot_longer(!c(farmers_hh,hh_id), names_to = "crops", values_to = "crop01")
cropping_17_18[is.na(cropping_17_18)] <- 0

cropping_veg_17_18 <- 
  ml18_crop_plot_3s %>% filter(season!="summer_2018") %>% 
  rename(crop_code=crop_ml18) %>% 
  left_join(list_crop) %>% 
  select(farmers_hh,hh_id,crop_family) %>%distinct() %>% 
  filter(crop_family == "Vegetables") %>% 
  mutate(crop01=1) %>% 
  right_join(ml18)  %>% filter(!is.na(farmers_hh)) %>% 
  mutate(crops="Vegetables", crop01=ifelse(is.na(crop01),0,1)) %>% 
  select(farmers_hh,hh_id,crops,crop01 )
rm(ml18)

croppin_pattern18 <- rbind(cropping_17_18,cropping_veg_17_18 )
rm(cropping_17_18,cropping_veg_17_18 )


#🟡 BASELINE

hh_crop_plot= bl_crop_plot_3s[,1] %>% distinct()
ml16 =rmtl_baseline2016 %>% select(hh_id,farmers_hh) %>% right_join(hh_crop_plot) %>% filter(!is.na(farmers_hh) )
rm(hh_crop_plot)

cropping_16 <-
  bl_crop_plot_3s %>%  # filter(season!="rabi_2014_15") %>% 
  select(hh_id,crop_name) %>%distinct() %>% 
  filter(crop_name %in% c(
    "Bengal_gram", "Sorghum_jowar", "Toor","Greengram","Maize","Wheat",
    "Pearl.millet_bajra","Onions","Sunflower","Cotton","Sugarcane","Chillies")) %>% 
  mutate(crop01=1) %>% 
  pivot_wider(names_from = crop_name, values_from = crop01) %>% 
  right_join(ml16) %>% filter(!is.na(farmers_hh)) %>% 
  pivot_longer(!c(farmers_hh,hh_id), names_to = "crops", values_to = "crop01")
cropping_16[is.na(cropping_16)] <- 0

cropping_veg_16 <- 
  bl_crop_plot_3s %>%  # filter(season!="rabi_2014_15") %>%
  select(hh_id,crop_family) %>%distinct() %>% 
  filter(crop_family == "Vegetables") %>% 
  mutate(crop01=1) %>% 
  right_join(ml16) %>% filter(!is.na(farmers_hh)) %>% 
  mutate(crops="Vegetables", crop01=ifelse(is.na(crop01),0,1)) %>% 
  select(farmers_hh,hh_id,crops,crop01 )
rm(ml16)

croppin_pattern16 <- rbind(cropping_16,cropping_veg_16 )
rm(cropping_16,cropping_veg_16 )

cp16=croppin_pattern16 %>% group_by(farmers_hh,crops) %>% summarise(prob16=mean(crop01))
cp18=croppin_pattern18 %>% group_by(farmers_hh,crops) %>% summarise(prob18=mean(crop01))
cp22=croppin_pattern22 %>% group_by(farmers_hh,crops) %>% summarise(prob22=mean(crop01))
cp_prob=full_join(cp16,cp18) %>% full_join(cp22)

# REVENUE    -----------

# L78	Total revenue? [season-crop]

size_acre <- a_plots_size %>% select(hh_id,plotID,acres) %>% filter(!is.na(acres))

revenue_per_acre_SEASON =
a_plots_revenue %>% filter(plotRevenue>999) %>% 
  group_by(season,hh_id,plotID) %>% 
  summarise(seasonRevenue=sum(plotRevenue)) %>% 
  left_join(size_acre) %>% 
  group_by(season,hh_id) %>% 
  summarise(revenue=sum(seasonRevenue),totalAcre =sum(acres )) %>% 
  mutate(revenue_per_acre_season=revenue/totalAcre) %>% 
  left_join(hh_2022)

revenue_per_acre_SEASON$season[revenue_per_acre_SEASON$season=="rabi"] <- "rabi_2021_22"
revenue_per_acre_SEASON$season[revenue_per_acre_SEASON$season=="kha"] <- "kharif_2021"
revenue_per_acre_SEASON$season[revenue_per_acre_SEASON$season=="KHA22"] <- "kharif_2022"


revenue_per_acre_SEASON %>% filter(season =="kharif_2021") %>% rename(re=revenue_per_acre_season) %>% 
  summarize(across(re, compute_summary))
re_kha21_99= revenue_per_acre_SEASON %>% filter(season =="kharif_2021",revenue_per_acre_season<81266)
t1=re_kha21_99 %>% t_test(revenue_per_acre_season  ~ farmers_hh , detailed = T) %>% rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% select(season,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)

revenue_per_acre_SEASON %>% filter(season =="rabi_2021_22") %>% rename(re=revenue_per_acre_season) %>% summarize(across(re, compute_summary))
re_rabi_99= revenue_per_acre_SEASON %>% filter(season =="rabi_2021_22",revenue_per_acre_season<48259)
t2=re_rabi_99 %>% t_test(revenue_per_acre_season  ~ farmers_hh , detailed = T) %>% rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% select(season,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)

revenue_per_acre_SEASON %>% filter(season =="kharif_2022") %>% rename(re=revenue_per_acre_season) %>% summarize(across(re, compute_summary))
re_rabi_99= revenue_per_acre_SEASON %>% filter(season =="kharif_2021",revenue_per_acre_season<52398)
t3=re_rabi_99 %>% t_test(revenue_per_acre_season  ~ farmers_hh , detailed = T) %>% rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% select(season,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)

t_L78a <- rbind(t1,t2,t3) 
nice_table(t_L78a)

nice_table(t_L78a,title = c("Table L78a | Crop revenue", "Revenue per acre by season 2021-2022"),
           note = c("[L78]	Total revenue?","🟩" ))

# CROP SELLING		----
# L56	Season Level	Who did you sell crop to?  

L56 <- rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with( "L56")) 

L56_crop_sell <- 
  L56 %>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(hh_id,l56a ,(ends_with("_888"))))  %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) %>% 
  mutate(inside_ramthal=inside_ramthal/946,outside_ramthal=outside_ramthal/666) %>% 
  mutate_at(2:3,round,2)

# Table L56 | Who did the farmers sell their crop to?
#   Sold crops in APMC in Hungund and Ilkal
# 
# L56_crop_sell[c(2,7,12 ) ,]
# 
# season         Ramthal  OutsideRamthal
# kharif  2021   0.64     0.57
# rabi 2021-22   0.64     0.48
# kharif  2022   0.58     0.53



# YIELD Sold Kept Lost             ----					
# How much of the yield was [%]	# [percentage at Season-Crop]
# [L52] Sold # [L53] Kept for HH consumption # [L54] Lost in post-harves

L56 <- rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with( "L56")) 


yield_prt_season <- 
  yield_prt %>%  # in DF_22.R
  group_by(hh_id,farmers_hh,season) %>% 
  summarise(prt_sold=mean(prt_sold),prt_stored=mean(prt_stored),
            prt_consum=mean(prt_consum),prt_lost=mean(prt_lost)) %>% 
  ungroup() %>%   mutate(
    prt_sold = prt_sold / 100,
    prt_stored = prt_stored / 100,
    prt_consum = prt_consum / 100,
    prt_lost = prt_lost / 100
  )

yield_prt_season %>% group_by(season,farmers_hh) %>% summarise(prt_lost=mean(prt_lost))
  
yield_prt_season %>% group_by(season,farmers_hh) %>% 
  summarise(prt_sold=mean(prt_sold),prt_stored=mean(prt_stored),prt_consum=mean(prt_consum) ) 


# How much  of the yield % was [L54] Lost in post-harvest?
t01 <- yield_prt_season %>% group_by(season) %>% 
  t_test(prt_lost ~ farmers_hh, detailed = T )

t_L54 <- t01%>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high) 
nice_table(t_L54,title = c("Table L54 | Lost yield", "Percentage of yield lost in post-harvest"),
           note = c("How much  of the yield % was [L54] Lost in post-harvest?","🟩" ))

t01 %>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 0.4, lwd=1,
                  color =  c("darkolivegreen4","dodgerblue3",    "dodgerblue4")) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey55") +
  labs(title = "" ,x = "", y = "% of lost yield",
       subtitle = "Fig. L54 | Lost yield") + 
  theme_classic()+  theme(text = element_text(size = 12, family = "serif"))



# INPUTS                           ----
# costs of  [__].   Season wise
# L70 irrigation equipment  #  L71 Mechanization  #  L72 Fuel  #  L73 Labor

costesA <- rmtl_srvy22 %>% select(farmers_hh, hh_id, contains ("l70"),contains ("l71"),contains ("l72"),contains ("l73"))

costesB <- costesA %>% pivot_longer(cols = -c(farmers_hh, hh_id),names_to = c("observation"))
costesB$value[costesB$value ==-999 ] <- NA
costesB$observation <- sub("^l70","irriEquipment", costesB$observation) 
costesB$observation <- sub("^l71","mechanization", costesB$observation) 
costesB$observation <- sub("^l72","fuel", costesB$observation) 
costesB$observation <- sub("^l73","labor", costesB$observation) 

library(tidyr)
# seasons
costesC <- costesB %>% separate(observation, into = c("inputs", "season"), sep = "_") %>% rename(inputs_Rs=value)

inputs_acre_sns22 <- a_plots_crop %>% select(hh_id,season,plotID) %>% distinct() %>% left_join(a_plots_size,by=c("hh_id","plotID")) %>% group_by(hh_id,season) %>% summarise(sum_acre=sum(acres,na.rm = T)) %>% mutate(season=ifelse(season=="rabi","rab",season)) %>% left_join(costesC,by=c("hh_id","season")) %>% mutate(inputs_per_acre=inputs_Rs/sum_acre ) %>% mutate_at(7,round)
inputs_acre_sns22$season[inputs_acre_sns22$season=="rab"] <- "rabi_2021_22"
inputs_acre_sns22$season[inputs_acre_sns22$season=="kha"] <- "kharif_2021"
inputs_acre_sns22$season[inputs_acre_sns22$season=="KHA22"] <- "kharif_2022"

# year 2021-2022
costesD <- costes_seasons_22 %>% 
  filter(season != "KHA22") %>%
  group_by(farmers_hh,hh_id, inputs) %>%  
  summarise(inputs_Rs=sum(inputs_Rs,na.rm = T)) %>% ungroup() 

inputs_acre_yr22 <-a_plots_crop %>% 
  select(hh_id,season,plotID) %>% distinct() %>% 
  filter(season!="KHA22") %>% 
  left_join(a_plots_size,by=c("hh_id","plotID")
            ) %>% 
  group_by(hh_id) %>% 
  summarise(sum_acre=sum(acres,na.rm = T)
            ) %>% left_join(costesD) %>%
  mutate(inputs_per_acre=inputs_Rs/sum_acre ) %>% mutate_at(6,round)

rm(costesB, costesA)


# t test
t1 <-
  inputs_acre_sns22 %>% group_by(inputs,season) %>% t_test(inputs_per_acre  ~ farmers_hh , detailed = T) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,inputs,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

t2 <-
  inputs_acre_yr22%>% group_by(inputs) %>% t_test(inputs_per_acre  ~ farmers_hh , detailed = T) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(inputs,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) %>% 
  mutate(season="year 2021-22") %>% select(season,everything())

L7123 <- rbind(t2,t1)
nice_table(L7123,title = c("Table L7123 | Inputs", "Cost of equipment, mechanization, fuel and labor in 2021-2022"),
           note = c("[L70-L73] costs of ____","🟩" ))



# SEEDS                                                        ----
# season-crop

#🟣L57 Name the seed [  ] #🟡🟠D31.9 
#🟣L58 Is it normal or improved seeds? [1=normal | 2=improved] #🟡🟠D31.10 

attr(rmtl_srvy22$l58_kha_1, "labels") # normal=1 improved=2

seeds22_l57_l58=
  rmtl_srvy22 %>% 
  select(hh_id, farmers_hh, starts_with("l57"), starts_with("l58")  )%>% 
  mutate_at(vars(-hh_id), as.character) %>% 
  pivot_longer(-c(hh_id, farmers_hh), names_to = "season_crop", values_to = "seed")

seeds22A <- seeds22_l57_l58 %>% separate(season_crop, into = c("L" ,"season", "crop"), sep = "_")
seeds22A$season[seeds22A$season=="rab"] <- "rabi_2021_22"
seeds22A$season[seeds22A$season=="kha"] <- "kharif_2021"
seeds22A$season[seeds22A$season=="KHA22"] <- "kharif_2022"
seeds22A$crop <- sub("^(\\d{1,2})","crop\\1", seeds22A$crop)

seeds22 <- seeds22A %>% 
  pivot_wider(names_from = L , values_from = seed ) %>% 
  filter(l57 !="") %>% 
  rename(seed_name=l57 ) %>% 
  mutate(seed_improved= ifelse(l58==2,1,ifelse(l58==1,0,NA) ))

seeds22 # seeds season wise # [1,578 hh, in929/out649]

# prt Households using improved seeds by season 
seeds22_ses<-
  seeds22 %>%  # seeds season wise # [1,578 hh, in929/out649]
  group_by(hh_id,farmers_hh,season) %>% 
  summarise(seed_improved_total=sum(seed_improved,na.rm = T),total_seed=n()) %>% 
  mutate(seed_improved_prt=seed_improved_total/total_seed,
         seed_improved_yesno=ifelse(seed_improved_total>0,1,0 ))

seeds22_ses %>%  
  group_by(season,farmers_hh) %>% summarise(seed_improved_yesno=sum (seed_improved_yesno),n=n() ) %>% 
  mutate(seed_improved_yesno/n)

# prt Households using improved seeds in cropping year 2021-2022
seeds_21_22 <- 
  seeds22 %>%
  filter(season!="kharif_2022") %>% 
  group_by(hh_id, farmers_hh) %>% 
  summarise(seed_improved_total=sum(seed_improved ,na.rm = T),seed_total=n()) %>% 
  mutate(prt=seed_improved_total/seed_total,
         seed_improved_yesno=ifelse(seed_improved_total>0,1,0) )

seeds_21_22 %>% group_by(farmers_hh) %>% summarise(seed_improved_yesno=sum (seed_improved_yesno),n=n() ) %>% 
  mutate(seed_improved_yesno/n)

seeds_21_22 %>% group_by(farmers_hh) %>% summarise(seed_improved_prt=mean (prt),n=n() ) 

# T TEST improved seeds

# Improved seeds percent a HH 
t01 <- seeds_21_22[,c(2,5)] %>% t_test(prt  ~ farmers_hh , detailed = T) %>% 
  mutate(season="cropping year 2021-2022")

t_L58 <- rbind(t01) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_L58,title = c("Table L58 | Improved seeds", ""),
           note = c("Improved seeds percent per household","[L58] Is it normal or improved seeds?","🟩" ))


# % Households using improved seeds 
t03 <- seeds_21_22[,c(2,6)] %>% t_test(seed_improved_yesno ~ farmers_hh , detailed = T) %>% 
  mutate(season="cropping year 2021-2022")

t04 <- seeds22_ses %>% group_by(season) %>% t_test(seed_improved_yesno ~ farmers_hh , detailed = T)

t_L58 <- rbind(t03,t04) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_L58,title = c("Table L58 | Improved seeds","% Households using improved seeds" ),
           note = c("[L58] Is it normal or improved seeds?","🟩" ))


# ASSET                                           ----
assets22 # yesno_assets & total_assets # [1,612 hh, in946/out666] 

vars_e01 <- rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("e")) 

# LIVESTOCK # [E6Cows E7Bullock E9Goats&sheep]
vars_e01$e8=ifelse(vars_e01$e8>9,0,vars_e01$e8) # E8 Buffaloes

# FARM EQUIPMENT # [E10Tractor E11Plough E12Thresher E13Seed drill] ## REMOVE [E14 JCB]

# VEHICLES # [E15Cycles E16Motorcycles] ## REMOVE [E17	Cars]

# HH ITEMS # [E18Fridge] ## REMOVE [E19	Television]
vars_e01$e21=ifelse(vars_e01$e21<0,NA,vars_e01$e21) # E21	Silver (in grams)
vars_e01$e21=ifelse(vars_e01$e21==1000,NA,vars_e01$e21)

assets22 = vars_e01 %>% select(-c(e14,e17,e19)) %>% 
  mutate(total_assets=e6+e7+e8+e9+e10+e11+e12+e13+e15+e16+e18,
         total_livestock=e6+e7+e8+e9,
         total_farm_equipments=e10+e11+e12+e13) %>% 
  mutate(own_assets=ifelse(total_assets>0,1,0),
         own_livestock=ifelse(total_livestock>0,1,0),
         own_farm_equipments=ifelse(total_farm_equipments>0,1,0)) %>% 
  select(1:2,15:20)

assets22 %>% group_by(farmers_hh) %>% summarise(mean(yesno_livestock))
assets22 %>% group_by(farmers_hh) %>% summarise(mean(total_livestock))

# t test
t01 <- assets22 %>% t_test(own_assets ~ farmers_hh , detailed = T) 
t02 <- assets22 %>% t_test(total_assets ~ farmers_hh , detailed = T) 

t03 <- assets22 %>% t_test(own_livestock ~ farmers_hh , detailed = T) 
t04 <- assets22 %>% t_test(total_livestock ~ farmers_hh , detailed = T) 

t05 <- assets22 %>% t_test(own_farm_equipments ~ farmers_hh , detailed = T) 
t06 <- assets22 %>% t_test(total_farm_equipments ~ farmers_hh , detailed = T) 


t_E <- rbind(t01,t03,t05,t02,t04,t06) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_E,title = c("Table E | Assats ಆಸ್","% Households own assats/ total assat household own" ),
           note = c("[E6-E21] How many of this item does the household currently own? (0 if none)","🟨" ))


# INCOME                                   ----

vars_f01 <- rmtl_srvy22 %>% select(farmers_hh, hh_id,starts_with("f")) 
names(vars_f01)
names(vars_f01) <- c("farmers_hh","hh_id",
                     "Migrating_household_income_hh",
                     "Migrating_household_income_amt",
                     "Remittance_hh","Remittance_amt",
                     "revenue12_amt",
                     "lease_land_hh","lease_land_amt",
                     "Own_livestock_hh","Own_livestock_net_profit",
                     "non_agri_business_hh","non_agri_business_net_profit",
                     "Salaried_job_hh","Salaried_job_amt",
                     "Casual_work_hh","Casual_work_amt",
                     "Gov_pension_scheme_hh","Gov_pension_scheme_amt",
                     "Rent_property_hh","Rent_property_amt",
                     "Other_activities_hh","Other_activities_amt",
                     "income_2021","sum_income12months" 
) 
vars_f02=vars_f01
vars_f02[is.na(vars_f02)] <- 0
vars_f02[vars_f02==-999] <- NA

vars_f02$Migrating_household_income_amt=ifelse(vars_f02$Migrating_household_income_amt==6,NA,vars_f02$Migrating_household_income_amt)

vars_f02$Remittance_amt=ifelse(vars_f02$Remittance_amt<1999,NA,vars_f02$Remittance_amt)

vars_f02$sum_income12months=ifelse(vars_f02$sum_income12months<9999,NA,vars_f02$sum_income12months)

income_21_22 = 
  vars_f02 %>% 
  mutate(
    income_2022_withoutRevenue=sum_income12months-revenue12_amt,
    income_2022_withRevenue=sum_income12months,
    total_income_sources_2022=Migrating_household_income_hh+
                        Remittance_hh+lease_land_hh+ #Own_livestock_hh+
                        non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Gov_pension_scheme_hh+
                        Rent_property_hh+Other_activities_hh,
    external_income=Migrating_household_income_amt+ Remittance_amt+Gov_pension_scheme_amt ) %>% 
  mutate(external_income=ifelse( is.na(external_income),0,external_income))

rm(vars_f01,vars_f02)


# F13	What is the annual household income in 2021? (Rs.)
# F12	Total income NOT A QUESTION Software will sum F1-F11

income_21_22 %>% group_by(farmers_hh) %>% summarise( mean(income_2021,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withoutRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(income_2022_withRevenue,na.rm = T))
income_21_22 %>% group_by(farmers_hh) %>% summarize( mean(total_income_sources_2022,na.rm = T))

t01 <- income_21_22 %>% t_test(income_2022_withoutRevenue ~ farmers_hh , detailed = T) 
t02 <- income_21_22 %>% t_test(income_2022_withRevenue ~ farmers_hh , detailed = T) 
t03 <- income_21_22 %>% t_test(income_2021 ~ farmers_hh , detailed = T) 
t04 <- income_21_22 %>% t_test(total_income_sources_2022 ~ farmers_hh , detailed = T) 

t_F <- rbind(t01,t02,t03,t04) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

t_F$.y.[t_F$.y.=="income_2022_withoutRevenue"] <- "2022 without crop revenue"
t_F$.y.[t_F$.y.=="income_2022_withRevenue"] <- "2022 with crop Revenue"
t_F$.y.[t_F$.y.=="income_2021"] <- "2021 with crop Revenue"
t_F$.y.[t_F$.y.=="total_income_sources_2022"] <- "total income sources 2022 "

nice_table(t_F,title = c("Table F | Income ಆದಾಯ   " ,"Household income from the farm and from sources other than the farm" ),
           note = c("", "income_2021| F13	What is the annual household income in 2021? (Rs.)",
                    "income_2022_withoutRevenue| F13 minus F3(revenue12_amt)",
                    "income_2022_withRevenue| F12	Total income NOT A QUESTION Software will sum F1-F11",
                    "num_income_sources_2022| count F1-F11","🟨" ))

# External income
library(dplyr)

income_21_22 %>%
  select(hh_id, farmers_hh, Migrating_household_income_hh, Remittance_hh, Gov_pension_scheme_hh) %>%
  group_by(farmers_hh) %>%
  summarise(
    n = n(),
    migration = sprintf("%.1f%%", sum(Migrating_household_income_hh) / n * 100),
    remittances = sprintf("%.1f%%", sum(Remittance_hh) / n * 100),
    pensions_or_subsidie = sprintf("%.1f%%", sum(Gov_pension_scheme_hh) / n * 100)
  ) %>% kbl() %>% kable_material()




summary(income_21_22$Gov_pension_scheme_amt)


t05 <- income_21_22 %>% t_test(external_income ~ farmers_hh , detailed = T) 
t06 <- income_21_22 %>% t_test(Gov_pension_scheme_amt ~ farmers_hh , detailed = T) 

t_F129 <- rbind(t05,t06) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p)
t_F129$.y.[t_F129$.y.=="external_income"] <- "External income"
t_F129$.y.[t_F129$.y.=="Gov_pension_scheme_amt"] <- "Pension/subsidies [Gov/NGO]"

nice_table(t_F129,title = c("Table F1.2.9 | External income" ,"family assistance, Pensions and subsidies - Government/Non-Government" ),
           note = c("", "[F1]Income sent by seasonal migrating household members" , 
                    "[F2]Remittances (from permanent migrants)" , 
                    "[F9]Government pension or scheme" ,"🟨" ))

# HOUSEHOLD ROSTER  ----

attr(rmtl_srvy22$r3, "labels")

roster22A= 
# r1 How many household members live in this house?
# r3_ What is the household member status ?
# r26_ Since 2016, has [member's name] migrated from the village for work for a period of 6 months or more?
rmtl_srvy22 %>% select(hh_id, farmers_hh, r1 )
rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("r3_"))

#        Migration  ----
migration22=
  rmtl_srvy22 %>% 
  select(hh_id, farmers_hh, starts_with("r26_")) %>% 
  pivot_longer(-c(hh_id,farmers_hh), names_to = "mmbr", values_to = "migrated") %>% 
  filter(!is.na(migrated)) %>% 
  group_by(hh_id,farmers_hh) %>% 
  summarise(migrt_total=sum(migrated), n_hhm=n() ,migrt_prc= migrt_total/n_hhm) %>% 
  ungroup()

t01 <- migration22 %>% t_test(migrt_total  ~ farmers_hh , detailed = T) 
t02 <- migration22 %>% t_test(migrt_prc ~ farmers_hh , detailed = T) 

t_R26 <- rbind(t01,t02) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)
t_R26$.y.[t_R26$.y.=="migrt_prc"] <- "fraction members of a HH who migrates"
t_R26$.y.[t_R26$.y.=="migrt_total"] <- "Total HH members who migrates"

nice_table(t_R26,title = c("Table R26 | Migration","HH members who migrates Since 2016" ),
           note = c("", "[R26] Since 2016, has [member's name] migrated from the village for work for a period of 6 months or more?","n includes 0s"))







roster18A= rmtl_midline2018 %>% select(id, farmers_hh, c1, c1_exist, c27)
# C1	How many household members live in this house?
# PREVIOUS HOUSEHOLD MEMBERS: C27	How many people, who previously lived in the household in the past 10 years now live elsewhere?


roster16A=rmtl_baseline2016 %>% select(hh_id, farmers_hh, C1,C27)
# C1	How many household members live in this house?
# C27	How many people, who previously lived in the household in the past 10 years now live elsewhere?

# C18_	Where do they reside most of the time during the rest of the year?


#education      ----
#   
# r7	Are they literate?
# r8	What is their educational level? (0=NOT literate)

# The HH head is the same as in 2016, so there is no need to examine it

r7= rmtl_srvy22 %>% select(hh_id,starts_with("r7" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate") %>% 
  filter(!is.na(literate))
r7$id_member <- sub("^r7_(\\d{1,2})","r_\\1",r7$id_member )

r8= rmtl_srvy22 %>% select(hh_id, starts_with("r8"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
r8$id_member <- sub("^r8_(\\d{1,2})","r_\\1",r8$id_member )

attr(rmtl_srvy22$r8_1, "labels")

edu22 = 
  left_join(r7,r8)%>%
  mutate(edu_level=ifelse(is.na(edu_level),0 ,edu_level )) %>% 
  mutate(high_edu_level=ifelse(edu_level %in% c(4:6),1 ,0 )) %>% 
rm(r7,r8)

education122 =
  edu22 %>% 
  group_by(hh_id) %>% 
  summarise(
    n_hhm= n(),
    literate_pct_hh=mean(literate),    
    edu_hh_level_hh= mean(edu_level),
    high_edu_pct_hh= mean(high_edu_level)
  ) %>%  ungroup() %>% 
  mutate_at(3:5,round,2) %>% left_join(hh_2022)


t01 <- education122 %>% t_test(literate_pct_hh  ~ farmers_hh , detailed = T) 
t02 <- education122 %>% t_test(edu_hh_level_hh ~ farmers_hh , detailed = T) 
t03 <- education122 %>% t_test(high_edu_pct_hh ~ farmers_hh , detailed = T) 

t_R8 <- rbind(t01,t02,t03) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,Ramthal,Outside_Ramthal,t,df,p,conf.low,conf.high)

nice_table(t_R8,title = c("Table R8 | Education","Education among HH members" ),
           note = c("","[r7] Are they literate?",
                    "[r8] What is their educational level? (0=NOT literate)"))






# SOCIAL CAPITAL    ----


social_22= rmtl_srvy22 %>% select(hh_id, farmers_hh, starts_with("h"))
attr(rmtl_srvy22$h1, "labels") # 1=dissatisfied|2|3|4|5=Satisfied
attr(rmtl_srvy22$h2, "labels") # 1=important|2|3|4=Not


h1h2_h18h23 <- 
  social_22 %>% 
  select(hh_id, farmers_hh,h1:h2,h18:h23 ) %>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-hh_id) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) 



# 	From the following list, rank the top 3 most important factors 
# H3 to a determining agricultural success
# H4 determining whether a farmer will have water for irrigation

# H3
h3_common_factor=
  social_22 %>% select(hh_id, farmers_hh, starts_with("h3") )%>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(hh_id,h3__777)) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) %>% 
  mutate(inside_ramthal=inside_ramthal/946,outside_ramthal=outside_ramthal/666) %>% 
  mutate_at(2:3,round,2) %>% arrange(desc(inside_ramthal ))
  
h3_factors <- 
  social_22 %>% 
  select(hh_id, farmers_hh, h3 )%>% 
  separate(h3, into = c("rnk1" ,"rnk2", "rnk3"), sep = " ") 

h3_rnk1=
  h3_factors %>% count(farmers_hh,rnk1) %>% 
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>% filter(!rnk1 %in% c("","-777")) %>% 
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)
  
h3_rnk2=
  h3_factors %>% count(farmers_hh,rnk2) %>% 
  filter(!is.na(rnk2) ) %>%
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>%  
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h3_rank1 = h3_rnk1[1,2:3] %>% mutate(Factors ="h3_rank1 | Skill_n_knowledge" )
h3_rank2 = h3_rnk2[1,2:3] %>% mutate(Factors ="h3_rank2 | Access_to_gov_schemes" )


# H4
h4_common_factor=
  social_22 %>% select(hh_id, farmers_hh, starts_with("h4") )%>% 
  group_by(farmers_hh) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(hh_id,h4__777)) %>% 
  pivot_longer(!farmers_hh, names_to = "POV", values_to = "count") %>% 
  pivot_wider(names_from = farmers_hh, values_from = count) %>% 
  mutate(inside_ramthal=inside_ramthal/946,outside_ramthal=outside_ramthal/666) %>% 
  mutate_at(2:3,round,2) %>% arrange(desc(inside_ramthal ))

h4_factors <- 
  social_22 %>% 
  select(hh_id, farmers_hh, h4 )%>% 
  separate(h4, into = c("rnk1" ,"rnk2", "rnk3"), sep = " ") 

h4_rnk1=
  h4_factors %>% count(farmers_hh,rnk1) %>% 
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>% filter(!rnk1 %in% c("","-777")) %>% 
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h4_rnk2=
  h4_factors %>% count(farmers_hh,rnk2) %>% 
  filter(!is.na(rnk2) ) %>%
  mutate(prt=ifelse(farmers_hh=="inside_ramthal",n/946,n/666 )) %>% 
  select(-n) %>%  
  mutate_at(3,round,2) %>% 
  pivot_wider(names_from = farmers_hh, values_from = prt)

h4_rank1 = h4_rnk1[1,2:3] %>% mutate(Factors ="h4_rank1 | Skill_n_knowledge" )
h4_rank2 = h4_rnk2[1,2:3] %>% mutate(Factors ="h4_rank2 | Access_to_gov_schemes" )



####### graph

# h3_# h4
h3_H4_ranks <- 
  rbind(h3_rank1,h3_rank2,h4_rank1,h4_rank2) %>% 
  select(Factors,inside_ramthal,outside_ramthal)

h3_H4_ranks %>% mutate(POV=c("H3.1","H3.2","H4.1","H4.2" )) %>%
  select(POV,inside_ramthal,outside_ramthal) %>% 
  kbl() %>% kable_material()
  

Factors_of_agricultural_success
Factors_of_water_availability

# H1:H2
h1h2_h18h23[1:2,] %>% kbl() %>% kable_material()
h1h2_h18h23[1:2,] %>% select(-POV) %>% kbl() %>% kable_material()

#H18:H23
h1h2_h18h23[3:8,] %>% kbl() %>% kable_material()
h1h2_h18h23[3:8,] %>% select(-POV) %>% kbl() %>% kable_material()







######################    essantials    ----

# Function to compute summary statistics
compute_summary <- function(x) {
  c(
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    Q25 = quantile(x, 0.25),
    Q75 = quantile(x, 0.75),
    P9= quantile(x, 0.9),
    P95= quantile(x, 0.95),
    Min = min(x),
    Max = max(x)
  )
}
compute_summary_1_99 <- function(x) {
  c(
    Count = n(),
    Mean = mean(x),
    Median = median(x),
    sd = sd(x),
    P1 = quantile(x, 0.01),
    P99 = quantile(x, 0.99),
    Min = min(x),
    Max = max(x)
  )
}

# group by as variable
group_var <- yield22_acre$your_grouping_variable

# Apply the function to each group
summary_stats <- tapply(yield22_acre$kg_per_acre, group_var, compute_summary)

# Convert the result to a data frame
summary_df <- as.data.frame(do.call(rbind, summary_stats))


attr(a_rmtl_srvy22$l7_rank_3, "labels")

flat_vector <- unlist(L48[,-1], use.names = FALSE)
table(flat_vector, useNA = "always") 

# remove columns only NA or empty
select(where(~!all(.x=="")))
select(where(~!all(is.na(.x))))

# write.csv
write.csv(rmtl_In_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_In_groups.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv", row.names=FALSE)





















