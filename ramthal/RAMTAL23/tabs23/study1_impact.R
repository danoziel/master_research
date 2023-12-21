
# STUDY 1 IN OUT wise 12.11.2023 
library(dplyr)
library(tidyverse)
library(haven)

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(tidyr) 

# essentials ----


library(kableExtra)
df <- data.frame(df = c("a_irri_rain_method", "", ""),hh = c(1578, 1609, 1612),X3_season = c(4734, 4827, 4836))
kable(df, format = "html") %>%kable_styling()
rm(df)

scale_fill_manual(values=c(  "#E69F00","skyblue")) +
  theme_minimal()+  theme(text = element_text(family = "serif"))
theme(text = element_text(family = "serif", size = 16),axis.title = element_text(size = 18),axis.text = element_text(size = 14))
theme_sjplot() #library(sjPlot)
theme_nice() #library(jtools) 
select(where(~!all(is.na(.x)))) 

# colors         ----

"Aquaculture";     "dodgerblue" 
"Cultivated Land"; "#a1d99b"

"Saptari";                "darkolivegreen4"
"Rautahat\nBara Sarlahi"; "lightsalmon4" 

"Total Area Cultivated" :"steelblue2"
"Area Irrigated" :       "steelblue"

"Winter" : "dodgerblue4"
"Monsoon" :"dimgrey"
"Summer" : "darkolivegreen4"
# vars to test         ----


### Crop-Year	
# What crops were planted in [ 2018 2019 2020 ]?

### Season-Plot
# Labor

### Season Level
# INPUTS

### season-crop
# CROP SELLING
# SEEDS

# L48a # What is the method of irrigation?	single choice	crop wise
#     1	Flood
# 		2	Furrows
#     3	Drip
#     4	Sprinkler
#     5	Manual
#     6	Hose
#     -888	Other (specify)
# Crop-Plot-Season	for Qs: L39a	2021-22	
#             Crop	for Qs: L39b	2018 2019 2020


# essential df's       ----

## IRRIGATION ## ----

# irri_crop case 1000174 106896 102692 106956 108106 [100911] plot_3methods

# hh-season-plot  # To unite divided plots into one irrigation method per plot
#md <-
  a_irri_rain_method %>% # [1,578 hh_id]
# select( hh_id , season ,plotID ,irri_method) %>% distinct() # A tibble: 6,242
  group_by(hh_id , season, plotID)  %>%
  mutate(plot_3methods=ifelse(!irri_method %in% c("rain","drip" ) ,"irrigation" ,irri_method)
         ) %>% #select(hh_id,season,plotID, plot_3methods) %>% distinct() # A tibble: 6,239
  mutate(plot_2methods=ifelse(plot_3methods=="rain","rain","irrigated")
         )  # select(hh_id,season,plotID, plot_2methods) %>% distinct() # A tibble: 6,238 

full_seasons <- 
  md[,1] %>% distinct() %>% mutate(KHA22=1,kha=1,rab=1) %>% 
  pivot_longer(!hh_id , names_to = "season", values_to = "count") %>% 
  select(hh_id,season)

# Mapping plots ----
# Mapping plots that have more than one irrigation method per plot. Total | 15 plots [14 farmers] | 9 inside 6 outside | All irrigated|
ts=md %>% select(hh_id , season ,plotID,cropIrri,plot_3methods  ) %>% pivot_wider(names_from = cropIrri, values_from = plot_3methods)
ts$ex=ifelse(ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3 & ts$cropIrri_3==ts$cropIrri_4|
           ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3|ts$cropIrri_1==ts$cropIrri_2 ,1,NA)
ts$ex=ifelse(is.na(ts$cropIrri_2),1,ts$ex)

ts %>% left_join(a_sample) %>% filter(is.na(ex)) %>% count(farmers_hh )
#  | 15 plots [14 farmers] | 9 inside, 6 outside | All irrigated|
#  I leave them for analysis marked as irrigated/drip




#    hh wise md1 ----

#  [freq 6 methods irrigated] md_hh <- 
a_irri_rain_method %>% 
  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods = 
           ifelse("drip" %in% irri_method , "drip", 
                  ifelse(any(irri_method  == "furrows"), "furrows",
                         ifelse(any(irri_method  == "flood"), "flood",
                                ifelse(any(irri_method  == "sprinkler"), "sprinkler",
                                       ifelse(any(irri_method  == "hose"), "hose",
                                              
                         "rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() %>% 
  left_join(a_sample[,1:2])

# stat
md_hh %>% group_by(farmers_hh,hh_6methods) %>% summarise(num=n()) %>% 
  mutate(num/sum(num)) %>% mutate_at(4,round,2)

md_hh %>% filter(hh_6methods != "rain")%>% 
  group_by(farmers_hh,hh_6methods) %>% summarise(num=n())%>% 
  mutate(num/sum(num))%>% mutate_at(4,round,2)

791+529-1612
#  [YesNo irrigated] df md1 <- 
md %>%
  group_by(hh_id) %>%
  mutate(hh_3method = ifelse("drip" %in% plot_3methods , "drip", ifelse(any(plot_3methods  == "irrigation"), "irrigation", "rain"))) %>%
  ungroup() %>% 
  mutate(hh_2method=ifelse(hh_3method=="rain","rain","irrigated") )%>% 
#  right_join(full_seasons) %>% 
  left_join(a_sample [,1:2])

md1$hh_2method[is.na(md1$hh_2method)] <- "not cultivated"

#stat
md1 %>% select(hh_id,hh_3method,farmers_hh) %>% distinct() %>% group_by(farmers_hh) %>% count(hh_3method) %>% mutate(n/sum(n) )
md1 %>% select(hh_id,hh_2method,farmers_hh) %>% distinct() %>% group_by(farmers_hh) %>% count(hh_2method) %>% mutate(n/sum(n) )

md1 %>% select(season ,hh_id,hh_2method) %>%
  right_join(full_seasons) %>% 
  left_join(a_sample [,1:2])%>% distinct() %>% group_by(season,farmers_hh) %>% 
  count(hh_2method) %>% mutate(n/sum(n) )

#    irrigated land ----

# [total irrigated acres] ----
irri_land <- 
  md %>% #select(hh_id,season,plotID,plot_2methods)  %>% 
  left_join(a_plots_size) %>% 
  group_by(hh_id, season,plot_2methods) %>% summarise(acres_2methods=sum(acres)) %>%  
  right_join(full_seasons) %>% 
  left_join(a_sample [,1:2]) %>% 
  mutate(acres_1methods=ifelse(plot_2methods=="irrigated",acres_2methods,0) )

irri_land$plot_2methods[is.na(irri_land$plot_2methods)] <- "not cultivated"
irri_land$acres_2methods[is.na(irri_land$acres_2methods)] <- 0

#stat
irri_land %>% group_by(season,farmers_hh) %>% summarise(mean(acres_1methods,na.rm = T))
irri_land %>% group_by(season,plot_2methods,farmers_hh) %>% summarise(mean(acres_2methods)) 

md01 <-irri_land %>% group_by(season) %>%
  t_test(acres_1methods ~ farmers_hh , detailed = T) %>% add_significance() 

tablemd01=bind_rows(md01) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,.y. ,Inside_Ramthal,Outside_Ramthal,estimate,conf.low,conf.high,t,df,p,p.signif) %>% 
  mutate(`.y.` = "irri land")
nice_table(tablemd01)

tablemd01%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "total irrigated per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))


irri_prt_02 %>% arrange(hh_id)
irri_prt_02 %>% filter(hh_id %in% c(100019,1000011)) %>% arrange(hh_id)

#  land prt ----

acre_cult21.22 <- 
  md %>% left_join(a_plots_size) %>% 
  group_by(hh_id, season) %>% summarise(cultivated=sum(acres)) %>% 
  right_join(full_seasons)
acre_cult21.22$cultivated[is.na(acre_cult21.22$cultivated)] <- 0

acre_total21.22 <- 
  a_plots_size %>% filter(!plotStatus %in% c(1,6)) %>% 
  select(hh_id,plotID,acres) %>% group_by(hh_id) %>% summarise(tota_hh_acre=sum(acres,na.rm = T) )

irri_prt_01 <- 
  md %>%left_join(a_plots_size) %>% select(hh_id,season,plotID,plot_2methods,acres)  %>% 
  group_by(hh_id,season,plot_2methods) %>% summarise(acres_2methods=sum(acres)) 
  
# irrigated land prt  OF  cultivated land [season wise] ----

# includes seasons without cultivation [md21] ----
irri_prt_21 <- 
  irri_prt_01 %>% 
  filter(plot_2methods =="irrigated") %>% 
  right_join(acre_cult21.22) %>% 
  mutate(prt_irriCult=acres_2methods/cultivated)
irri_prt_21$prt_irriCult [is.na(irri_prt_21$plot_2methods) ] <- 0 # do I want 0s here? YES
irri_prt_21 <- irri_prt_21 %>% left_join(a_sample [,1:2])

# stat
irri_prt_21 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irriCult,na.rm = T ))

# includes seasons without cultivation
md021 <-irri_prt_21 %>% group_by(season) %>%
  t_test(prt_irriCult  ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(`.y.`="prt_all_season")

# only seasons with cultivation [md22] ----
irri_prt_22 <- 
  irri_prt_01 %>% right_join(acre_cult21.22) %>% 
  mutate(prt_irriCult=acres_2methods/cultivated)
irri_prt_22$prt_irriCult[irri_prt_22$prt_irriCult<1 & irri_prt_22$plot_2methods=="rain" ] <- NA
irri_prt_22$prt_irriCult[irri_prt_22$prt_irriCult==1 & irri_prt_22$plot_2methods=="rain" ] <- 0
irri_prt_22 <- irri_prt_22 %>%filter(!is.na(prt_irriCult)) %>% left_join(a_sample [,1:2])

# stat
irri_prt_22 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irriCult,na.rm = T ))

# only seasons with cultivation
md022 <-irri_prt_22 %>% group_by(season) %>%
  t_test(prt_irriCult  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_cult_season") 

# t test  md021 md022 ----
table2122=bind_rows(md021,md022) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,season ,Inside_Ramthal,Outside_Ramthal,estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table2122)

table2122%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))


# irrigated land prt  OF  total land [HH wise] ----

# all seasons 
irri_prt_31 <- 
  irri_prt_01 %>% right_join(acre_total21.22) %>% 
  filter(!is.na(season)) %>% 
  mutate(prt_irri=acres_2methods/tota_hh_acre) %>% 
  filter(plot_2methods  =="irrigated") %>% 
  right_join(full_seasons) %>% left_join(a_sample [,1:2])
irri_prt_31$prt_irri [is.na(irri_prt_31$plot_2methods) ] <- 0 # do I want 0s here? YES

irri_prt_31 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irri,na.rm = T ))

md031 <-irri_prt_31 %>% group_by(season) %>%
  t_test(prt_irri  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_all_seasons") 


irri_prt_32 <- 
  irri_prt_01 %>% right_join(acre_total21.22) %>% 
  filter(!is.na(season)) %>% 
  mutate(prt_irri=acres_2methods/tota_hh_acre) 
s_32=irri_prt_32[,1:2] %>% distinct()

irri_prt_32 <- irri_prt_32 %>%  right_join(s_32) %>% 
  filter(plot_2methods  =="irrigated") %>% 
  left_join(a_sample [,1:2])
irri_prt_32$prt_irri[is.na (irri_prt_32$prt_irri) ] <- 0

irri_prt_32 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irri,na.rm = T ))

# only seasons with cultivation
md032 <-irri_prt_32 %>% group_by(season) %>%
  t_test(prt_irri  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_cult_season") 

# t test  md021 md022 ----
table3132=bind_rows(md031,md032) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,season ,Inside_Ramthal,Outside_Ramthal,estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table3132)

table2122%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))


# cultivated land prt OF  total land [HH wise] ----


  





### t_test  plots | revenue     ----

total_acre  # total_num_plots | total_acre
plotNew # new_plot_num | new_plot_acre

### t_test plots_num_acre  ----

library(rstatix)
library(rempsyc)
t11 <- plots_num_acre %>% t_test(total_acre  ~ in1_out0 , detailed = T) %>% add_significance()
t12 <- plots_num_acre %>% t_test(total_num_plots   ~ in1_out0 , detailed = T) %>% add_significance()
t21 <- plotNew %>% t_test(new_plot_acre  ~ in1_out0 , detailed = T) %>% add_significance()
t22 <- plotNew %>% t_test(new_plot_num   ~ in1_out0 , detailed = T) %>% add_significance()


table100=bind_rows(t11,t12,t21,t22) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Inside_Ramthal,Outside_Ramthal,
         estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table100)
rm(t11,t12,t21,t22)


table100[c(1,3),] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))

table100[c(2,4),] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "total plots per HH",x = "", y = "number of plots") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))



### t_test    revenue_per_acre_SEASON | distance_km ----
a_sample %>% count(distance_km)
a_sample %>% count(distance_up_to_1km)
a_sample %>% count(distance_up_to_1.5km)


t33a <-revenue_per_acre_SEASON %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(distance= ifelse(season=="KHA22","k22",ifelse(season=="kha","k21","r22")))%>% 
  mutate(Distance= ifelse(season=="KHA22","Kharif.22",ifelse(season=="kha","Kharif.21","Rabi.21.22")))
  
t33b <-revenue_per_acre_SEASON %>% filter(distance_up_to_1km==1) %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(distance= ifelse(season=="KHA22","k22\n1km",ifelse(season=="kha","k21\n1km","r22\n1km")))%>% 
  mutate(Distance= ifelse(season=="KHA22","1km_Kharif.22",ifelse(season=="kha","1km_Kharif.21","1km_Rabi.21.22")))
  
t33c <-revenue_per_acre_SEASON %>% filter(distance_up_to_1.5km==1) %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(distance= ifelse(season=="KHA22","k22\n1.5km",ifelse(season=="kha","k21\n1.5km","r22\n1.5km")))%>% 
  mutate(Distance= ifelse(season=="KHA22","1.5km_Kharif.22",ifelse(season=="kha","1.5km_Kharif.21","1.5km_Rabi.21.22")))

t33 = rbind(t33a,t33b,t33c) 
  
table33=t33%>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(Distance,Inside_Ramthal,Outside_Ramthal,
         estimate,conf.low,conf.high,t,df,p,p.signif) %>% 
  mutate_at(2:4,round)%>% mutate_at(2:4, ~sub("\\.00", "", .))
nice_table(table33)

t33 %>%
  ggplot(aes(x = distance, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_pointrange(size = 0.4, lwd=1,
                  color =  c("darkolivegreen4","darkolivegreen4","darkolivegreen4",
                             "dodgerblue3",    "dodgerblue3",    "dodgerblue3",
                             "dodgerblue4",    "dodgerblue4",    "dodgerblue4")) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey55") +
  labs(title = "Sesonal Revenue t-test" ,x = "", y = "revenue_per_acre",
       subtitle = "Revenue per acre of households inside/outside Ramthal \nwithin 1-1.5 km of the project border") + 
  theme_classic()+  theme(text = element_text(size = 12, family = "serif"))


### t_test    revenue_per_acre_SEASON | water source ----
a_sample %>% count(distance_km)
a_sample %>% count(distance_up_to_1km)
a_sample %>% count(distance_up_to_1.5km)


t33a <-revenue_per_acre_SEASON %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(distance= ifelse(season=="KHA22","k22",ifelse(season=="kha","k21","r22")))%>% 
  mutate(Distance= ifelse(season=="KHA22","Kharif.22",ifelse(season=="kha","Kharif.21","Rabi.21.22")))

t33b <-revenue_per_acre_SEASON %>% filter(distance_up_to_1km==1) %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(distance= ifelse(season=="KHA22","k22\n1km",ifelse(season=="kha","k21\n1km","r22\n1km")))%>% 
  mutate(Distance= ifelse(season=="KHA22","1km_Kharif.22",ifelse(season=="kha","1km_Kharif.21","1km_Rabi.21.22")))

t33c <-revenue_per_acre_SEASON %>% filter(distance_up_to_1.5km==1) %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(distance= ifelse(season=="KHA22","k22\n1.5km",ifelse(season=="kha","k21\n1.5km","r22\n1.5km")))%>% 
  mutate(Distance= ifelse(season=="KHA22","1.5km_Kharif.22",ifelse(season=="kha","1.5km_Kharif.21","1.5km_Rabi.21.22")))

t33 = rbind(t33a,t33b,t33c) 

table33=t33%>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(Distance,Inside_Ramthal,Outside_Ramthal,
         estimate,conf.low,conf.high,t,df,p,p.signif) %>% 
  mutate_at(2:4,round)%>% mutate_at(2:4, ~sub("\\.00", "", .))
nice_table(table33)

t33 %>%
  ggplot(aes(x = distance, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_pointrange(size = 0.4, lwd=1,
                  color =  c("darkolivegreen4","darkolivegreen4","darkolivegreen4",
                             "dodgerblue3",    "dodgerblue3",    "dodgerblue3",
                             "dodgerblue4",    "dodgerblue4",    "dodgerblue4")) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey55") +
  labs(title = "Sesonal Revenue t-test" ,x = "", y = "revenue_per_acre",
       subtitle = "Revenue per acre of households inside/outside Ramthal \nwithin 1-1.5 km of the project border") + 
  theme_classic()+  theme(text = element_text(size = 12, family = "serif"))


# L48_irri_rain a_irri_method ----

t33a <-revenue_per_acre_SEASON %>% group_by(season) %>%
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(distance= ifelse(season=="KHA22","k22",ifelse(season=="kha","k21","r22")))%>% 
  mutate(Distance= ifelse(season=="KHA22","Kharif.22",ifelse(season=="kha","Kharif.21","Rabi.21.22")))

# irrigation1 rainfed0
t48 <-revenue_per_acre_SEASON %>% left_join(L48_irri_rain) %>%
  filter(irrigation1_rain0 ==1) %>%  group_by(season) %>%
  
  t_test(revenue_per_acre_season ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(distance= ifelse(season=="KHA22","k22\n1km",ifelse(season=="kha","k21\n1km","r22\n1km")))%>% 
  mutate(Distance= ifelse(season=="KHA22","1km_Kharif.22",ifelse(season=="kha","1km_Kharif.21","1km_Rabi.21.22")))

model <- lm(revenue_per_acre_season ~ farmers_hh+irrigation1_rain0, t48)
summary( model )



### t_test    income asset----

# t_test tresults
library(rstatix)
library(rempsyc)

# df income 1
vars_income <- vars_f03 %>% full_join(a_sample)

# df income 2
vars_income_revenue <-revenue_per_acre_YEAR21 %>%  select(hh_id,revenue_per_acre_2021) %>% 
  inner_join(vars_income)

# independent vars t_test
vars_income_revenue
t_f13<-vars_income %>% t_test(income_2021   ~  farmers_hh, detailed = T) %>% add_significance()
t_f6<- vars_income %>% t_test(income_2022~farmers_hh, detailed = T) %>% add_significance()
t_f1<- vars_income %>% t_test(num_income_sources_2022   ~ farmers_hh, detailed = T) %>% add_significance()

#_____# HH assistance 
# Migrating_household_income_hh +Remittance_hh + Gov_pension_scheme_hh
#_____# own assistance 
# lease_land_hh+Own_livestock_hh+non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Rent_property_hh+Other_activities_hh

t_e14 <- vars_asset %>% t_test(sum_assets    ~ farmers_hh, detailed = T) %>% add_significance()
t_e15 <- vars_asset %>% t_test(sum_livestock    ~ farmers_hh, detailed = T) %>% add_significance()
t_e16 <- vars_asset %>% t_test(sum_farm_equipments    ~ farmers_hh, detailed = T) %>% add_significance()
  
table200=bind_rows(t_f13,t_f6,t_f1,t_e14,t_e15,t_e16) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Inside_Ramthal,Outside_Ramthal,
         estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table200)
rm(t_f13,t_f6,t_f1,t_f_reve,t_e14,t_e15,t_e16)
# ggplot ----
table200[1:2,] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 0.3,color = "royalblue") +
  labs(x = "Variable", y = "Estimate") +
  theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))

table200[3:6,] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 0.3,color = "royalblue") +
  labs(x = "Variable", y = "Estimate") +
  theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))



#vars_IAR vars_IA Income Asset Revenue ----

# DFs: vars_income | vars_asset | revenue_per_acre_YEAR21 # [1,502]
vars_IAR <- 
  revenue_per_acre_YEAR21 %>%
  select(hh_id,plotRevenue,acres,revenue_per_acre_2021) %>% # [1,502 id+3]
  inner_join(vars_income) %>%   # [1,612 id+25+12]
  left_join(vars_asset[,c(1,14:16)] ) # [1,612 id+15]

# DFs: vars_income |vars_asset # [1,612]
vars_IA <- 
  vars_asset[,c(1,14:16)] %>% 
  inner_join(vars_income) 



 library(sjPlot) # for lm tab_model() 

# HH assistance 

model=lm(revenue_per_acre_2021 ~ farmers_hh+Migrating_household_income_hh+Remittance_hh+Gov_pension_scheme_hh,
         vars_IAR)
summary(model)
tab_model(model, show.se = TRUE)


model=lm(revenue_per_acre_2021 ~ farmers_hh+
           Migrating_household_income_amt+Remittance_amt+Gov_pension_scheme_amt,
         vars_IAR)
summary(model)
tab_model(model, show.se = TRUE)


# own assistance

model=lm(revenue_per_acre_2021~farmers_hh+lease_land_hh+Own_livestock_hh+non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Rent_property_hh+Other_activities_hh,
         vars_IAR)
summary(model)
tab_model(model, show.se = TRUE)


model=lm(revenue_per_acre_2021~farmers_hh+
           lease_land_amt+Own_livestock_net_profit+non_agri_business_net_profit+
           Salaried_job_amt+Casual_work_amt+Rent_property_amt+Other_activities_amt,
         vars_IAR)
summary(model)
tab_model(model, show.se = TRUE)


#assets
model=lm(revenue_per_acre_2021 ~ farmers_hh+sum_assets+sum_livestock+sum_farm_equipments,
         vars_IAR)
summary(model)
tab_model(model, show.se = TRUE)




# ------------------------
# INCOME ಆದಾಯ                                           ----
                ## DF vars_income    ----



vars_f01 <- a_rmtl_srvy22 %>% select(hh_id,starts_with("f")) 
names(vars_f01)
names(vars_f01) <- c( "hh_id",
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

                ## vars_f02        ----
# vars_f02 = INCOME section aggregate 0's NA's
vars_f02$Migrating_household_income_amt=ifelse(vars_f02$Migrating_household_income_amt<1999,NA,vars_f02$Migrating_household_income_amt)
vars_f02$Migrating_household_income_amt=ifelse(vars_f02$Migrating_household_income_hh==0,0,vars_f02$Migrating_household_income_amt)

vars_f02$Remittance_amt=ifelse(vars_f02$Remittance_amt<1999,NA,vars_f02$Remittance_amt)
vars_f02$Remittance_amt=ifelse(vars_f02$Remittance_hh==0,0,vars_f02$Remittance_amt)

vars_f02$lease_land_amt=ifelse(is.na(vars_f02$lease_land_amt),0,vars_f02$lease_land_amt)

# יש הרבה בעלי לייבסטוק שלא עשו פרופיט ולכן 0 נשאר 0 ולא אנ-איי 
vars_f02$Own_livestock_net_profit=ifelse(vars_f02$Own_livestock_net_profit<0,NA,vars_f02$Own_livestock_net_profit)
vars_f02$Own_livestock_net_profit=ifelse(is.na(vars_f02$Own_livestock_net_profit),0,vars_f02$Own_livestock_net_profit)

vars_f02$non_agri_business_net_profit=ifelse(is.na(vars_f02$non_agri_business_net_profit),0,vars_f02$non_agri_business_net_profit)

vars_f02$Salaried_job_amt=ifelse(vars_f02$Salaried_job_amt<=0,NA,vars_f02$Salaried_job_amt)
vars_f02$Salaried_job_amt=ifelse(vars_f02$Salaried_job_hh==0,0,vars_f02$Salaried_job_amt)

vars_f02$Casual_work_amt=ifelse(vars_f02$Casual_work_amt<=0,NA,vars_f02$Casual_work_amt)
vars_f02$Casual_work_amt=ifelse(vars_f02$Casual_work_hh==0,0,vars_f02$Casual_work_amt)

vars_f02$Gov_pension_scheme_amt=ifelse(is.na(vars_f02$Gov_pension_scheme_amt),0,vars_f02$Gov_pension_scheme_amt)

# יש הרבה בעלי רנטפרופרטי ללא סכום 
vars_f02$Rent_property_amt=ifelse(vars_f02$Rent_property_amt<=0,NA,vars_f02$Rent_property_amt)
vars_f02$Rent_property_amt=ifelse(vars_f02$Rent_property_hh==0,0,vars_f02$Rent_property_amt)

vars_f02$Other_activities_amt=ifelse(is.na(vars_f02$Other_activities_amt),0,vars_f02$Other_activities_amt)

vars_f02$sum_income12months=ifelse(vars_f02$sum_income12months<9999,NA,vars_f02$sum_income12months)

vars_f02$income_2021=ifelse(vars_f02$income_2021==0,NA,vars_f02$income_2021)

                ## vars_f03        ----
vars_f03 = 
  vars_f02 %>% 
  mutate(income_2022=Migrating_household_income_amt+Remittance_amt+
           lease_land_amt+Own_livestock_net_profit+non_agri_business_net_profit+
           Salaried_job_amt+Casual_work_amt+Gov_pension_scheme_amt+
           Rent_property_amt+Other_activities_amt) %>% 
  mutate(num_income_sources_2022=Migrating_household_income_hh+
           Remittance_hh+lease_land_hh+ #Own_livestock_hh+
           non_agri_business_hh+Salaried_job_hh+Casual_work_hh+Gov_pension_scheme_hh+
           Rent_property_hh+Other_activities_hh)
rm(vars_f01,vars_f02)


                # income_2021    ----
# F13	What is the annual household income in 2021? (Rs.)
#F12	Total income NOT A QUESTION Software will sum F1-F11

vars_income_2022 %>% group_by(farmers_hh) %>% summarise(income_2022=mean(income_2022,na.rm = T))

vars_income %>% group_by(farmers_hh) %>% summarize( mean(Salaried_job_hh == 1))
# ASSET ಆಸ್ತಿ                                                     ----

vars_asset = vars_e02 %>% full_join(a_sample)

           ## DF vars_asset ----
           ## vars_e01 ----
vars_e01 <- a_rmtl_srvy22 %>% select(hh_id,starts_with("e")) 

# LIVESTOCK # [E6Cows E7Bullock E9Goats&sheep]
vars_e01$e8=ifelse(vars_e01$e8>9,NA,vars_e01$e8) # E8 Buffaloes

# FARM EQUIPMENT # [E10Tractor E11Plough E12Thresher E13Seed drill] ## REMOVE [E14 JCB]

# VEHICLES # [E15Cycles E16Motorcycles] ## REMOVE [E17	Cars]

# HH ITEMS # [E18Fridge] ## REMOVE [E19	Television]
vars_e01$e21=ifelse(vars_e01$e21<0,NA,vars_e01$e21) # E21	Silver (in grams)
vars_e01$e21=ifelse(vars_e01$e21==1000,NA,vars_e01$e21)


           ## vars_e02 ----

vars_e02 = vars_e01 %>% select(-c(e14,e17,e19))

vars_e02$sum_assets <- rowSums(vars_e02[, -1], na.rm = TRUE) > 0
vars_e02$sum_livestock <- rowSums(vars_e02[,2:5])
vars_e02$sum_farm_equipments <- rowSums(vars_e02[,6:9])


# WATER                                                     ----

# infrastructure | use water | WUA member

        ## DF vars_wat1   ----
vars_wat1 <- 
  a_rmtl_srvy22 %>%
  select(hh_id,
         mm4, # Has any infrastructure (e.g. piping) been installed in your land at any time in the past?
         mm5, # Have you ever (even once) made any use of this water for irrigating your land?
         mw4, # Are you still making use of the water from the project to irrigate your land?
         ) %>%
  left_join(a_sample)

        ## distribution   ----
vars_wat1 %>% group_by(HH_project,mm4) %>% count()
   
vars_wat1 %>% group_by(in1_out0,mm4,mm5) %>% count()
vars_wat1 %>% filter(HH_south_north=="south") %>% count(HH_project,mm5)%>%group_by(HH_project) %>%  mutate(n/sum(n))


vars_wat1 %>% group_by(in1_out0,m59a) %>% count() %>% 
  group_by(in1_out0) %>% mutate(prc=n/sum(n)) %>% 
  select(1:2,4,3)

vars_wat1 %>% group_by(HH_project,mw4) %>% summarise(n=n())

vars_wat1 %>% filter(f13>0) %>% group_by(HH_project) %>%summarise(income_2022=mean(f13))
vars_wat1 %>% group_by(HH_project,f6) %>% count() # salaried_job
vars_wat1 %>% group_by(HH_project) %>%summarise(salaried_job=mean(f6_amt,na.rm = T))
        ## t.test         ----
library(broom)
t01 <- t.test(f13 ~ in1_out0, data = vars_wat1)
t01 <- tidy(t01, conf.int = TRUE)
nice_table(t01)
 
# PLOTS                                                     ----

plotStatus  
# 2 [Currently owned plots]
# 5 [Partial Sold]
# 1 [Sold/disposed]
# 6 [Refuse / Plot Not Exisit]

# Variables to check what they represent: ----
# what the diff "plot_size_acre_1" AND "plot_acre_1"
# 1  l46_y_18_acre_7
# 2  plot_acre_guntas_for_round 
# 3  plot_acre_guntas_div 
# 4  plot_acre_guntas #   plot_acre_sum

A=a_rmtl_srvy22 %>%
  select(plot_size_acre_1,plot_size_acre_2,plot_size_acre_3,
         plot_acre_sum,plot_acre_guntas_for_round,plot_acre_guntas_div,
         plot_acre_guntas,
         plot_acre_1,plot_acre_2,plot_acre_3,hh_id)



         ## total_plots1   ----
plotsN <- a_rmtl_srvy22[, c(1 ,grep("l_plot_status_", names(a_rmtl_srvy22)))]
plotsN[plotsN==1 ] <- NA #remove 1 [Sold/disposed]
plotsN[plotsN==6 ] <- NA #remove 6 [Refuse / Plot Not Exisit]
plotsN$total_prev_plots <- rowSums(!is.na(plotsN[, -1]))

L30 <- a_rmtl_srvy22 %>% select(hh_id,l30)

total_plots1=
  plotsN[,c(1,12)] %>%
  left_join(L30) %>%
  rename(new_plot=l30) %>% 
  mutate(total_hh_plots=total_prev_plots +new_plot) %>% 
  left_join(a_sample)

rm(L30,plotsN)

total_plots1  %>% group_by(farmers_hh) %>% summarise(total_plots=mean(total_hh_plots,na.rm = TRUE) )


         ## plots_num_acre [total_num_plots|total_acre]   ----

# df plots_num_acre = 
a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(total_num_plots=n(),total_acre=sum(acres,na.rm = T)) %>% 
  left_join(a_sample )

#stat
plots_num_acre %>%  
  group_by(farmers_hh) %>%
  summarise(total_num_plots =mean(total_num_plots),total_acre=mean(total_acre))


total_plots_normalized=
  total_plots2 %>% filter(!is.na(total_hh_plots)) %>% 
  group_by(HH_project,total_hh_plots) %>%
  count(total_hh_plots) %>%
  group_by(total_hh_plots) %>%
  mutate(Percent = n / sum(n) * 100)

total_plots_normalized %>% 
  filter(total_hh_plots<9,total_hh_plots!=0) %>%  # rm outlyers
  ggplot(aes(x = factor(total_hh_plots), y = Percent, fill = factor(HH_project ))) +
  geom_bar(stat = "identity") +
  labs(title = "", x = "Total HH's plots", y = " ") +
  scale_fill_manual(values=c("skyblue", "#E69F00"))+  
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()+
  theme(text = element_text(family = "serif", size = 10),axis.title = element_text(size = 18),axis.text = element_text(size = 14))



         ## plotNew                                         ----
#  New plots purchased since 2018 
hh =  a_sample[,1]
#  plotNew  =
a_plots_size %>% 
  filter(plotStatus=="new") %>% select(hh_id,acres) %>% 
  group_by(hh_id) %>% 
  summarise(new_plot_num=n(), new_plot_acre=sum(acres) ) %>% 
  full_join(hh) %>% 
  mutate(new_plot_num=ifelse(is.na(new_plot_num ),0,new_plot_num ),
         new_plot_acre=ifelse(is.na(new_plot_acre ),0,new_plot_acre )) %>% 
  full_join(a_sample)
rm(hh)

plotNew %>% full_join(a_sample) %>% group_by(farmers_hh) %>%
  summarise(new_plot_num=mean(new_plot_num ), new_plot_acre=mean(new_plot_acre) )
  

         ## revenue_per_acre_SEASON ----

size_acre <- a_plots_size %>% select(hh_id,plotID,acres) %>% filter(!is.na(acres))

# revenue_per_acre_SEASON =
  a_plots_revenue %>% filter(plotRevenue>999) %>% 
  group_by(season,hh_id,plotID) %>% 
  summarise(seasonRevenue=sum(plotRevenue)) %>% 
  left_join(size_acre) %>% 
  group_by(season,hh_id) %>% 
  summarise(revenue=sum(seasonRevenue),totalAcre =sum(acres )) %>% 
  mutate(revenue_per_acre_season=revenue/totalAcre) %>% 
  left_join(a_sample)

revenue_per_acre_SEASON %>% group_by(season,farmers_hh ) %>% summarise(revenue_per_acre_season=mean(revenue_per_acre_season))

# Labor                       ## ----
# INPUTS                      ## ----
# CROP SELLING                ## ----
### season-crop

# SEEDS                       ## -----
### season-crop

