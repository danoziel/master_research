############ STUDY 1 IN OUT wise 12.11.2023 

#| 🟡BASELINE 2016  | rmtl_baseline2016 #= baseline_RMTL
#| 🟠MIDELINE 2018  | rmtl_midline2018 #= mid2018_RMTL
#| 🟣MIDELINE 2022  | rmtl_srvy22 #= a_rmtl_srvy22

#|🟩Cultivation 🟦Water Ramthal  🟨Income,Assat  🟪Social

#|🟩 methods [L48a] 
#|🟩 source  [L7]
#|🟦 Infrastructure [mm4]
#|🟦 WATER USAGE [mm5]

#|⬛  ⬛  ⬛   ⬛  ⬛  ⬛  ⬛    ----

library(dplyr)
library(tidyr) 
library(haven)

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )
library(tidyverse)


#|⬛⬛⬛⬛⬛⬛⬛⬛ vars to test⬛⬛⬛⬛⬛⬛⬛


### Crop-Year	
# What crops were planted in [ 2018 2019 2020 ]?

### Season-Plot
# Labor

### Season Level
# INPUTS

### season-crop
# CROP SELLING
# SEEDS


#|⬛⬛⬛⬛⬛⬛⬛⬛ essentials⬛⬛⬛⬛⬛⬛⬛

full_seasons <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id)%>% mutate(kha=1,rab=1,KHA22=1) %>% 
  pivot_longer(!c(farmers_hh,hh_id) , names_to = "season", values_to = "count") %>% 
  select(farmers_hh,hh_id,season)


data.frame(
  df = c("rmtl_baseline2016", "rmtl_midline2018", "rmtl_srvy22"),
  o = c("in1_out0","hh_2022",""),
  farmers_hh = c("farmers_hh", "inside_ramthal","outside_ramthal")
) %>% kable() %>% kable_material()


# ᗷᗩSE 2016
# ᗰIᗪ 2018
# ᗰIᗪ22

# Base𝟭𝟲
# Mid𝟭𝟴
# MID𝟮𝟮

# 𝗕𝗮𝘀 2016
# 𝗠𝗶𝗱 2018
# 𝗠𝗶𝗱 22

#|🟥🟧🟨🟩🟦🟪⬛⬜🟫 
#|🟦🟩  ⿡⿢⿣📊🧮 

who1: overlap  ----
  #| Used  source [Gov_supply]

irri_prt_02 %>% arrange(hh_id)
irri_prt_02 %>% filter(hh_id %in% c(100019,1000011)) %>% arrange(hh_id)

add_significance(cutpoints = c(0,1e-04,0.001,0.01,0.05,1),symbols = c("****","***","**","*","ns"))
add_significance(cutpoints = c(0,0.12,1),symbols = c("*","ns"))


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


#|⬛⬛⬛⬛⬛⬛⬛⬛ colors⬛⬛⬛⬛⬛⬛⬛


"Aquaculture";     "dodgerblue" 
"Cultivated Land"; "#a1d99b"

"Saptari";                "darkolivegreen4"
"Rautahat\nBara Sarlahi"; "lightsalmon4" 

"Total Area Cultivated" :"steelblue2"
"Area Irrigated" :       "steelblue"

"Winter" : "dodgerblue4"
"Monsoon" :"dimgrey"
"Summer" : "darkolivegreen4"


#|⬛⬛⬛⬛⬛⬛⬛⬛ Mapping plots⬛⬛⬛⬛⬛⬛⬛


# Mapping plots that have more than one irrigation method per plot. Total | 15 plots [14 farmers] | 9 inside 6 outside | All irrigated|
ts=md %>% select(hh_id , season ,plotID,cropIrri,plot_3methods  ) %>% pivot_wider(names_from = cropIrri, values_from = plot_3methods)
ts$ex=ifelse(ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3 & ts$cropIrri_3==ts$cropIrri_4|
               ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3|ts$cropIrri_1==ts$cropIrri_2 ,1,NA)
ts$ex=ifelse(is.na(ts$cropIrri_2),1,ts$ex)

ts %>% left_join(a_sample) %>% filter(is.na(ex)) %>% count(farmers_hh )
#  | 15 plots [14 farmers] | 9 inside, 6 outside | All irrigated|
#  I leave them for analysis marked as irrigated/drip

#| ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛


#🟦    Water usage | Infrastructure [df wi45]                                 ----
# wi = WI = Water Infrastructure

wi45= a_rmtl_srvy22 %>% select(hh_id,mm4,mm5) %>% 
  left_join(a_sample[,1:2] ) %>%   
  rename(infrastructure_installed=mm4) %>% 
  mutate(water_usage=ifelse(mm5==0,"didnt_use_water","Use_water")) 

########### Descriptive stat
wi45 %>% count(farmers_hh,infrastructure_installed) %>% group_by(farmers_hh) %>% mutate(n/sum(n))
wi45 %>% count(farmers_hh,water_usage) %>% group_by(farmers_hh) %>% mutate(n/sum(n)) 


wi1 <-wi45 %>%t_test(infrastructure_installed ~ farmers_hh , detailed = T) %>% add_significance() 
wi2 <-wi45 %>% mutate(mm5=ifelse( is.na(mm5),0,mm5)) %>% t_test(mm5~ farmers_hh , detailed = T) %>% add_significance() 
wi3 <-wi45 %>%t_test(mm5~ farmers_hh , detailed = T) %>% add_significance() 

wi2$.y.[wi2$.y.== "mm5"] <- "water_used_2016_2022"
wi3$.y.[wi3$.y.== "mm5"] <- "water_used/infr"

### table_irri
table_wi=bind_rows(wi1,wi2,wi3) %>% 
  rename(`Inside \nRamthal`=estimate1,`Outside \nRamthal`=estimate2,t=statistic, prt_hh_2022=`.y.`) %>% 
  select(prt_hh_2022 ,`Inside \nRamthal`,`Outside \nRamthal`,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_wi)







#🟩   IRRI:  HH freq | methods [HH wise]                             ----


### DFs 🟡🟠🟣            ----

#🟣  DF MID22 ----
# How many households irrigate? 
#|🟩 [L48a] #What is the method of irrigation? 

hh_irrigation <- 
  a_irri_rain_method %>% 
  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods =  # make it  ONE irri method for ONE hh 
           ifelse("drip" %in% irri_method , "drip", 
                  ifelse(any(irri_method  == "furrows"), "furrows",
                         ifelse(any(irri_method  == "flood"), "flood",
                                ifelse(any(irri_method  == "sprinkler"), "sprinkler",
                                       ifelse(any(irri_method  == "hose"), "hose",
                                              "rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() %>% 
  mutate(hh_drip=ifelse(hh_6methods=="drip","drip",ifelse(hh_6methods=="rain","rain","irrigation"))) %>% 
  mutate(hh_irrigation=ifelse(hh_drip=="rain","rain","irrigation")) %>% 
  left_join(hh_2022 )










#🟡  DF BASELINE
#|🟩 [D12] # Has this plot been irrigated at least once during the last 5 years?

bl_hh_irrigate = rmtl_baseline2016 %>%  select(in1_out0,hh_id,starts_with("D12") ,-c(D12_11:D12_17 )) %>% 
  mutate(total_irriPlot = rowSums (.[names(.)[3:12]], na.rm = T)) %>%
  rename( hh_irrigate_BL= D12_) %>% 
  select(in1_out0,hh_id,hh_irrigate_BL )

#🟠  DF MIDELINE 2018
# "ml18_irri_methods" IN "mid2018.R"
"d15_a_:  1 Flood | 2 Furrows | 3 Drip | 4 Sprinkler | 5 Manual | 6 Hose | -888 Other (specify)"

#🟩  ###### (D13_A	Has this plot been irrigated at least once in the past year)?
#|  [D15_A] # What methods? (Mark all)

#🟩  ###### [d29]	Has this plot been irrigated at least once during the last 2 years?
d29 <- mid2018_RMTL %>% select(in1_out0,id, starts_with("d29"),-c("d29_share","d29_lease","d29_number"))
d29[d29==2] <- 0
d29[is.na(d29)] <- 0
d29 <- d29 %>% mutate(d_ = rowSums(.[names(.)[3:11]], na.rm = T))%>% 
  mutate(farmers_hh= ifelse(in1_out0==1, "inside_ramthal" ,"outside_ramthal" )) %>% 
  mutate(D29=ifelse(d_>0,1,0 ))

#⬛ rain/irrigation ----

#🟣 MID22 rain/irrigation
hh_irrigation %>% count(farmers_hh,hh_irrigation) %>% group_by(farmers_hh) %>%  mutate(n/sum(n)) 

mt01 <- 
  hh_irrigation %>% mutate(hh_irrigate_2021=ifelse(hh_6methods =="rain",0,1)) %>% 
  t_test(hh_irrigate_2021 ~ farmers_hh , detailed = T)

#🟡  BASELINE rain/irrigation
bl_hh_irrigate %>% count(in1_out0,hh_irrigate_BL) %>% group_by(in1_out0) %>%  mutate(n/sum(n)) 

mt_bl01 <-
  bl_hh_irrigate %>% rename(bl_last_5years=hh_irrigate_BL ) %>% 
  t_test(bl_last_5years ~ in1_out0 , detailed = T) 

#🟠 MIDELINE 2018 rain/irrigation 2 VARS
##A 29	Has this plot been irrigated at least once during the last 2 years?
d29 %>% count(farmers_hh,D29) %>%group_by(farmers_hh) %>%  mutate(n/sum(n))  

##B "ml18_irri_methods" = Has this plot been irrigated at least once in the past year?[d13a]
ml18_irri_methods %>%
  mutate(hh_irrigation = ifelse(irri==0,0,1 )) %>% count(in1_out0 , hh_irrigation) %>% 
  group_by(in1_out0) %>% mutate(grp=n/sum(n))

ml18_15a <- 
  ml18_irri_methods %>% mutate(hh_irrigate_past_year = ifelse(irri==0,0,1 )) %>% 
  t_test(hh_irrigate_past_year ~ farmers_hh , detailed = T)

ml18_29 <-  
  d29 %>%  rename(last_2years=D29 ) %>% 
  mutate(farmers_hh= ifelse(in1_out0==1, "inside_ramthal" ,"outside_ramthal" )) %>% 
  t_test(last_2years ~ farmers_hh , detailed = T)

######## table_2H
table_2H=bind_rows(mt_bl01,ml18_29,ml18_15a,mt01) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  # rename(In_rmtl=estimate1,Out_rmtl=estimate2,t=statistic) %>% 
  select(.y.,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

table_2H$.y.[table_2H$.y.== "bl_last_5years"] <- "2011-2016"
table_2H$.y.[table_2H$.y.== "last_2years"] <- "2016-2017"
table_2H$.y.[table_2H$.y.== "hh_irrigate_past_year"] <- "2018"
table_2H$.y.[table_2H$.y.== "hh_irrigate_2021"] <- "2021-2022"

nice_table(table_2H)

nice_table(table_2H,
           title = c("Table 2H", "fraction of hh who use irrigation"),
           note = c("", 
                    "Baseline2016: Has this plot been irrigated at least once during the last 5 years? [d12]",
                    "Midline 2018: Has this plot been irrigated at least once in the past year? [d13a,d15a]",
                    "Midline 2018: Has this plot been irrigated at least once during the last 2 years? [d29]",
                    "Midline 2022: What is the method of irrigation? [L48a]" ))



#⬛ hh_6methods     -----


#🟣 MID22 hh_6methods
hh_irrigation %>% count(farmers_hh,hh_6methods ) %>% group_by(farmers_hh) %>%  mutate(n/sum(n)) 

#🟡  BASELINE hh_6methods
# D21 start in "base16.R"
bl16_irri_methods %>% count(farmers_hh,irri_method) %>% 
  group_by(farmers_hh) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

#🟠 MIDELINE 2018 hh_6methods
ml18_irri_methods %>%filter(!is.na(farmers_hh))  %>% count(farmers_hh, irri_method) %>% 
  group_by(farmers_hh) %>% mutate(grp=n/sum(n)) %>% mutate_at(4,round,3)

#⬛ drip OF all  hh -----

#🟣 MID22 drip OF all
hh_irrigation %>% group_by(farmers_hh,hh_drip) %>% summarise(num=n()) %>% mutate(num/sum(num)) %>% mutate_at(4,round,2)

md22 <- 
  hh_irrigation %>% 
  mutate(drip_of_all=ifelse(hh_6methods =="drip",1,0)) %>% 
  t_test(drip_of_all ~ farmers_hh, detailed = T)
md22$.y.[md22$.y.== "drip_of_all"] <- "2021-2022  [mid22]"


#🟡  BASELINE drip OF all
bl16_irri_methods %>%
  mutate(hh_drip = ifelse(irri_method_num==3,1,0 )) %>% count(farmers_hh , hh_drip) %>% 
  group_by(farmers_hh) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

md16 <-
  bl16_irri_methods %>%
  mutate(drip_of_all = ifelse(irri_method_num==3,1,0 ))  %>% 
  t_test(drip_of_all ~ farmers_hh , detailed = T)
md16$.y.[md16$.y.== "drip_of_all"] <- "2011-2016  [bl]"

#🟠 MIDELINE 2018 drip OF all
ml18_irri_methods %>%
  mutate(hh_drip = ifelse(irri==3,1,0 )) %>% count(farmers_hh , hh_drip) %>% 
  group_by(farmers_hh ) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

md18 <- 
  ml18_irri_methods %>%
  mutate(drip_of_all = ifelse(irri==3,1,0 )) %>% 
  t_test(drip_of_all ~ farmers_hh , detailed = T)
md18$.y.[md18$.y.== "drip_of_all"] <- "2016-2018  [mid18]"

##### table_21H
table_21H=bind_rows(md16,md18,md22) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_21H)

nice_table(table_21H,
           title = c("Table 21H", "Drip irrigation users - Fraction of all hh"),
           note = c("bl[d21] | Mid18[d15a] | mid22[L48a]" ))




#⬛ drip OF irri   -----

#🟣 MID22 drip OF irri
hh_irrigation %>% filter(hh_6methods != "rain") %>% group_by(farmers_hh,hh_drip) %>% summarise(num=n()) %>% mutate(num/sum(num))

md22 <- 
  hh_irrigation %>% 
  filter(hh_6methods != "rain") %>%  
  mutate( drip_of_irrigation =ifelse(hh_6methods =="drip",1,0)) %>% 
  t_test( drip_of_irrigation ~ farmers_hh , detailed = T)
md22$.y.[md22$.y.== "drip_of_irrigation"] <- "2021-2022  [mid22]"


#🟡  BASELINE drip OF irri
bl16_irri_methods %>%
  filter(irri_method_num != 0) %>% 
  mutate(hh_drip = ifelse(irri_method_num==3,1,0 )) %>% count(farmers_hh , hh_drip) %>% 
  group_by(farmers_hh) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

md16 <- 
  bl16_irri_methods %>%
  filter(irri_method_num  != 0) %>% 
  mutate(drip_of_irrigation = ifelse(irri_method_num==3,1,0 ))  %>% 
  t_test(drip_of_irrigation ~ farmers_hh , detailed = T)
md16$.y.[md16$.y.== "drip_of_irrigation"] <- "2011-2016  [bl]"


#🟠 MIDELINE 2018 drip OF irri
ml18_irri_methods %>%
  filter(irri != 0) %>% 
  mutate(hh_drip = ifelse(irri==3,1,0 )) %>% count(farmers_hh , hh_drip) %>% 
  group_by(farmers_hh ) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

md18 <- 
  ml18_irri_methods %>%
  filter(irri != 0) %>% 
  mutate(drip_of_irrigation = ifelse(irri==3,1,0 )) %>% 
  t_test(drip_of_irrigation ~ farmers_hh , detailed = T)
md18$.y.[md18$.y.== "drip_of_irrigation"] <- "2016-2018  [mid18]"


###############  table_22H
table_22H=bind_rows(md16,md18,md22) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_22H,
           title = c("Table 22H", "Drip irrigation users - Fraction of hh who irrigate"),
           note = c("bl[d21] | Mid18[d15a] | mid22[L48a]" ))

# 🟩 IRRI SOURCE  [HH wise] ----
# `source` as sum of rank_1_2_3

# 🟡 BASELINE
# bl_source_irrigate2 in df16.R
#1	Canal #2	Tank #3 Open well #4	River/Pond/Lake # 5	Bore well

bl_source_irrigate %>% group_by(farmers_hh ,irri_source_bl) %>% 
  count() %>% 
  group_by(farmers_hh) %>% mutate(n/sum(n)) # %>% mutate_at(4,round,3)

bl16_source <-bl_source_irrigate %>% 
  mutate(sources_Not_rain =ifelse(irri_source_bl =="0",0,1)) %>% 
  mutate(Borewell =ifelse(irri_source_bl =="5",1,0))

sr10 <- bl16_source %>%t_test(sources_Not_rain ~ farmers_hh , detailed = T)
sr11 <- bl16_source %>%t_test(Borewell ~ farmers_hh , detailed = T)


#🟠 MIDELINE 2018
# ml18_source_irrigate  in df18.R
ml18_source_irrigate  %>% group_by(farmers_hh ,irri_source) %>% 
  count() %>% 
  group_by(farmers_hh) %>% mutate(n/sum(n))  %>% mutate_at(4,round,3)

ml18_source <-ml18_source_irrigate %>% 
  mutate(sources_Not_rain =ifelse(irri_source=="0",0,1)) %>% 
  mutate(Canal =ifelse(irri_source=="Canal",1,0)) %>% 
  mutate(River =ifelse(irri_source=="River(/pond)",1,0)) %>% 
  mutate(Canal_River =ifelse(irri_source %in% c("Canal", "River(/pond)") ,1 ,0)) %>% 
  mutate(Borewell =ifelse(irri_source =="Borewell",1,0))


sr20 <- ml18_source %>%t_test(sources_Not_rain ~ farmers_hh , detailed = T)
sr21 <- ml18_source %>%t_test(Borewell ~ farmers_hh , detailed = T)
sr22 <- ml18_source %>%t_test(Canal ~ farmers_hh , detailed = T)
sr23 <- ml18_source %>%t_test(River ~ farmers_hh , detailed = T)


# 🟣 MID 22
a_source_irri %>%
  group_by(farmers_hh,irri_source) %>% summarise(num=n()) %>% 
  mutate(num/sum(num)) # %>% mutate_at(4,round,2)

ml22_source <-a_source_irri %>% 
  mutate(GovSupply_source=ifelse(irri_source=="gov_supply",1,0)) %>% 
  mutate(sources_Not_rain =ifelse(irri_source=="rain",0,1))%>% 
  mutate(Canal =ifelse(irri_source=="canal",1,0)) %>% 
  mutate(Borewell =ifelse(irri_source =="borewell",1,0))

sr30 <- ml22_source %>%t_test(sources_Not_rain ~ farmers_hh , detailed = T)
sr31 <- ml22_source %>%t_test(Borewell ~ farmers_hh , detailed = T)
sr32 <- ml22_source %>%t_test(Canal ~ farmers_hh , detailed = T)
sr35 <- ml22_source %>%t_test(GovSupply_source ~ farmers_hh , detailed = T)

# sr3 <- source_artificial %>%t_test(sources_Not_rain ~ farmers_hh , detailed = T) %>% add_significance() 
# sr4 <- source_artificial %>%t_test(HH_GovSupply_rank1 ~ farmers_hh , detailed = T) %>% add_significance() 

############### tables

###  River GovSupply    table_2s   ----
table_2s=bind_rows(sr23,sr35) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(y  =c( "[ml18] River","[ml22] GovSupply" )  ) %>%  
  select(y  ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_2s,title = c("Table 2s", ""),
           note = c( "ml18,2017-2018,[D14_A] | ml22,2021-2022,[L7]" ))


###  sources_Not_rain   table_20s  ----
table_20s=bind_rows(sr10,sr20,sr30) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(sources_Not_rain  =c("2012-2016 [bl]","2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(sources_Not_rain  ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_20s,title = c("Table 20s", ""),
           note = c( "bl[d21] | ml18[D14_A] | ml22[L7]" ))

###  Borewell           table_21s      ----
table_21s=bind_rows(sr11,sr21,sr31) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(Borewell   =c("2012-2016 [bl]","2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(Borewell   ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_21s,title=c("Table 21s",""),note=c("bl[d21] | ml18[D14_A] | ml22[L7]"))

###  Canal              table_22s   ----
table_22s=bind_rows(sr22,sr32) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(Canal    =c("2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(Canal    ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_22s,title = c("Table 22s", ""), note = c( "ml18[D14_A] | ml22[L7]" ))




# WHO are the outside users ?? ----
#🟦🟩 who1: overlap  
#| Used  source [Gov_supply] / infrastructure [Installed] / water [used] 

WI= a_rmtl_srvy22 %>% select(hh_id,mm4,mm5) %>% #| 🟦
  left_join(a_sample[,1:2] )

WI5= WI %>% filter(mm5==1)

WIs=source_irri %>% #| 🟩 
  filter(irri_source_num == 5) 

###### df WI5s
WI5s=inner_join( WI5[,c(1,4)] , WIs[,1] ) %>% mutate(overlap=1)
WI5s %>% count(farmers_hh) %>% mutate(grp=c(946,666 )) %>% mutate(n/grp)

# inside_ramthal  213 overlap [23%]
# outside_ramthal  48 overlap [7%]

ovl= a_rmtl_srvy22 %>% 
  select(hh_id,mm4,mm5) %>%    #| 🟦
  left_join(source_irri) %>%   #| 🟩 
  right_join(hh_irrigation) %>% #| 🟩 
  left_join(a_sample[,1:2] )

ovl %>% count(farmers_hh,mm4,mm5,hh_irrigation)

ovl %>% count(farmers_hh,hh_irrigation,mm4,mm5)

ovl %>% count(farmers_hh,hh_drip, mm4,mm5 )

x=
  ovl %>% mutate(hh_6methods =ifelse(hh_6methods  %in% c("flood","furrows"),"furrows_flood",hh_6methods  )) %>% 
  count(farmers_hh,hh_6methods , irri_source  ) %>% 
  group_by(farmers_hh,hh_6methods) %>% 
  filter(!hh_6methods=="rain") %>%  
  group_by(farmers_hh)%>% mutate(ngrp=sum(n) ) %>% 
  mutate(prc=n/ngrp) %>% mutate_at(6,round,2) %>% 
  group_by(farmers_hh,hh_6methods) %>%  mutate(sum_method=sum(prc))



# 🟦  who2: mm2	gov projects but ramthal

# mm2		Is your land coming under such a government project? 
# 1	Ramrhal
# 2	Krishi Honda ( Farm pond)
# 3	Pradhan Mantri Krishi Sinchai Yojana(PMKSY)
# 4	Ganga kalyana
# 5	non

###### df Im_in_ramthal
Im_in_ramthal=a_rmtl_srvy22 %>% select(hh_id, mm4,mm5,mm2_1) %>% left_join(a_sample[,1:2]) 
Im_in_ramthal %>% count(farmers_hh,mm2_1,mm4) %>% 
  pivot_wider(names_from = farmers_hh, values_from = n) %>% 
  mutate(inside_ramthal/946,outside_ramthal/666)

Im_in_ramthal %>% count(farmers_hh,mm2_1,mm4) %>% 
  pivot_wider(names_from = mm4, values_from = n) %>% 
  mutate(inside_ramthal/946,outside_ramthal/666)

  
###### df but_ramthal
but_ramthal=
  a_rmtl_srvy22 %>% select(hh_id, mm5,starts_with("mm2")) %>% 
  filter(!mm2 %in% c(1,5)) %>% left_join(a_sample[,1:2]) 


###### DS
but_ramthal %>% count(farmers_hh) %>% mutate(grp=c(946,666 )) %>% mutate(n/grp)

# farmers_hh         n     `n/grp`   infrastructure  water_used
# --- --- --- --- --- --- --- --- --- -- --- ---  --- --- --- -
# inside_ramthal    10    0.0106     [1 hh]          [2 hh]
# outside_ramthal   13    0.0195     [2 hh]          [0 hh]


# 🟦  year first use
#  mw1a		If Yes, in which year did you first make use of the water? 

mw1a= a_rmtl_srvy22 %>% select(hh_id,starts_with("mw1a"))%>% # 🟦 
  left_join(a_sample[,1:2]) %>% 
  rename(year_1st_use =mw1a)

mw1a %>% group_by(farmers_hh,year_1st_use) %>% summarise(n=n())  %>% group_by(farmers_hh) %>%  mutate(pct=n/sum(n))
first_y_use= mw1a %>% group_by(farmers_hh,year_1st_use) %>% summarise(n=n()) %>% 
  filter(!is.na(year_1st_use))

#  first use🟦 of hh who uses irrigation methods 🟩 [L48a]
first_y_use1= 
  a_rmtl_srvy22 %>% select(hh_id, mm5,mw1a )%>% # 🟦 
  rename(year_1st_use =mw1a) %>% 
  filter(!is.na(year_1st_use)) %>% 
  left_join(hh_irrigation) %>% #🟩 
  filter(hh_irrigation == "irrigation") %>%
  left_join(a_sample[,1:2])

n_1st_y_use=first_y_use1 %>% count(farmers_hh,year_1st_use)
first_y_use1 %>% count(farmers_hh,year_1st_use) %>% group_by(farmers_hh) %>%  mutate(pct=n/sum(n)) 


zero_2022_outrmtl=n_1st_y_use[6,]
zero_2022_outrmtl$n[zero_2022_outrmtl$n ==3] <- 0
zero_2022_outrmtl$n=as.integer(zero_2022_outrmtl$n)
zero_2022_outrmtl$farmers_hh[zero_2022_outrmtl$farmers_hh =="inside_ramthal"] <- "outside_ramthal"

first_y_use <- 
  rbind(n_1st_y_use,zero_2022_outrmtl) %>% #group_by(farmers_hh) %>%mutate(sum(n))
  group_by(farmers_hh) %>%  mutate(pct=n/sum(n)) %>% 
  select(farmers_hh,year_1st_use,pct ) %>% 
  pivot_wider(names_from = farmers_hh, values_from = pct)




a_rmtl_srvy22 %>% select(hh_id, mm5,starts_with("mw1c"))
  
  

#🟩  irrigated land acre [HH-season] ----

#🟣 ----
irri_acre_plotID <- #  [HH-season-plot] # md # irrigation_drip_plot 
  a_irri_rain_method %>% # [1,578 hh_id] # [6,740 rows]
  select( hh_id ,season ,plotID ,irri_method) %>% distinct() %>%  # [6,242]
  group_by(hh_id , season, plotID)  %>%  # make it  ONE irri method for ONE plot 
  mutate(hh_6methods =  
           ifelse("drip" %in% irri_method , "drip", 
                  ifelse(any(irri_method  == "furrows"), "furrows",
                         ifelse(any(irri_method  == "flood"), "flood",
                                ifelse(any(irri_method  == "sprinkler"), "sprinkler",
                                       ifelse(any(irri_method  == "hose"), "hose",
                                              "rain"))))) ) %>%
  ungroup() %>% select(hh_id, season, plotID ,hh_6methods) %>% distinct() %>% 
  mutate(plot_irrigated= # plot is irrigate YESNO
           ifelse(hh_6methods == "rain",0,1)) %>%
  left_join(hh_2022) %>% left_join(a_plots_size[,c(1:2,7)]) %>% 
  mutate(acre_irrigate=ifelse(plot_irrigated ==1,acres,0 )) %>% 
  mutate(acre_drip=ifelse(hh_6methods =="drip", acres,0) )

#⬛ DF cultivated seasons ----

#🟣 ----
irri_acre_season <- #  irrigation = acres | rain = 0
  irri_acre_plotID %>% 
  group_by(farmers_hh,hh_id,season) %>% summarise(acre_irrigate =sum(acre_irrigate ))

irri_acre_season %>% group_by(season,farmers_hh) %>% summarise(mean(acre_irrigate))
  
#⬛ DF drip irrigation ----

#🟣 ----
drip_acre_season <-  # drip = acres | irrigation = 0 | rain = 0
  irri_acre_plotID %>% 
  group_by(farmers_hh, hh_id,season) %>% summarise(acre_drip=sum(acre_drip))
  
drip_acre_season %>% group_by(season,farmers_hh) %>% summarise(mean(acre_drip))


#❓  BASELINE ----
bl_irri_land %>% group_by(farmers_hh) %>% summarise(mean(acre_irri))

#❓ MIDELINE 2018 ----

########### ttest

# baseline
b_ir <-bl_irri_land %>% t_test(acre_irri  ~ farmers_hh , detailed = T) 
b_ir=b_ir %>%  rename(in_rmtl=estimate1,out_mtl =estimate2,t=statistic) %>% 
  select(.y. ,in_rmtl ,out_mtl,n1,n2,estimate,conf.low,conf.high,t,df,p) 
b_ir$.y.[b_ir$.y.=="acre_irri"] <- "__________________________Baseline 2016 "
nice_table(b_ir)




#2022
ir2 <-irri_land2 %>% group_by(season) %>% t_test(irrigated_acre ~ farmers_hh , detailed = T) %>% add_significance() 
ir3 <-irri_land3 %>% group_by(season) %>% t_test(irrigated_acre ~ farmers_hh , detailed = T) %>% add_significance() 
ir4 <-irri_land4 %>% group_by(season) %>% t_test(drip_acre ~ farmers_hh , detailed = T) %>% add_significance() 
ir5 <-irri_land5 %>% group_by(season) %>% t_test(drip_acre ~ farmers_hh , detailed = T) %>% add_significance() 
ir2$.y.[ir2$.y.=="irrigated_acre"] <- "irrigated (acre)/cult seasons"
ir3$.y.[ir3$.y.=="irrigated_acre"] <- "irrigated (acre)/all seasons"
ir4$.y.[ir4$.y.=="drip_acre"] <- "drip irrigate (acre)/all hh"
ir5$.y.[ir5$.y.=="drip_acre"] <- "drip irrigate (acre)/irri hh"

table_ir=bind_rows(ir2,ir3,ir4,ir5) %>% 
  rename(`Inside \nRamthal`=estimate1,`Outside \nRamthal`=estimate2,t=statistic) %>% 
  select(.y. ,season,`Inside \nRamthal`,`Outside \nRamthal`,n1,n2,estimate,conf.low,conf.high,t,df,p) 
table_ir$season[table_ir$season=="KHA22"] <- "Kharif 2022"
table_ir$season[table_ir$season=="kha"] <- "Kharif 2021"
table_ir$season[table_ir$season=="rab"] <- "rabi 2021-22"

table_2I=table_ir
nice_table(table_2I)

tablemd01%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "total irrigated per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))



# היסטוגרמה של חקלאים לפי אחוזי שטח מושקה  ----
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
  

####### irrigated land prt  OF  cultivated land [season wise] 

### includes seasons without cultivation [md21] 
irri_prt_21 <- 
  irri_prt_01 %>% 
  filter(plot_2methods =="irrigated") %>% 
  right_join(acre_cult21.22) %>% 
  mutate(prt_irriCult=acres_2methods/cultivated)
irri_prt_21$prt_irriCult [is.na(irri_prt_21$plot_2methods) ] <- 0 # do I want 0s here? YES
irri_prt_21 <- irri_prt_21 %>% left_join(a_sample [,1:2])

# stat
irri_prt_21 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irriCult,na.rm = T ))

# ttest
md021 <-irri_prt_21 %>% group_by(season) %>%
  t_test(prt_irriCult  ~ farmers_hh , detailed = T) %>% add_significance() %>% 
  mutate(`.y.`="prt_all_season")

###  cultivated seasons   [md22] 
irri_prt_22 <- 
  irri_prt_01 %>% right_join(acre_cult21.22) %>% 
  mutate(prt_irriCult=acres_2methods/cultivated)
irri_prt_22$prt_irriCult[irri_prt_22$prt_irriCult<1 & irri_prt_22$plot_2methods=="rain" ] <- NA
irri_prt_22$prt_irriCult[irri_prt_22$prt_irriCult==1 & irri_prt_22$plot_2methods=="rain" ] <- 0
irri_prt_22 <- irri_prt_22 %>%filter(!is.na(prt_irriCult)) %>% left_join(a_sample [,1:2])

# stat
irri_prt_22 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irriCult,na.rm = T ))

# ttest
md022 <-irri_prt_22 %>% group_by(season) %>%
  t_test(prt_irriCult  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_cult_season") 

# t test  md021 md022 
table2122=bind_rows(md021,md022) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,season ,Inside_Ramthal,Outside_Ramthal,estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table2122)

table2122%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))


####### irrigated land prt  OF  total land [HH wise]

### all seasons 
irri_prt_31 <- 
  irri_prt_01 %>% right_join(acre_total21.22) %>% 
  filter(!is.na(season)) %>% 
  mutate(prt_irri=acres_2methods/tota_hh_acre) %>% 
  filter(plot_2methods  =="irrigated") %>% 
  right_join(full_seasons) %>% left_join(a_sample [,1:2])
irri_prt_31$prt_irri [is.na(irri_prt_31$plot_2methods) ] <- 0 # do I want 0s here? YES

#stat
irri_prt_31 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irri,na.rm = T ))

#ttest
md031 <-irri_prt_31 %>% group_by(season) %>%
  t_test(prt_irri  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_all_seasons") 

###
irri_prt_32 <- 
  irri_prt_01 %>% right_join(acre_total21.22) %>% 
  filter(!is.na(season)) %>% 
  mutate(prt_irri=acres_2methods/tota_hh_acre) 
s_32=irri_prt_32[,1:2] %>% distinct()

irri_prt_32 <- irri_prt_32 %>%  right_join(s_32) %>% 
  filter(plot_2methods  =="irrigated") %>% 
  left_join(a_sample [,1:2])
irri_prt_32$prt_irri[is.na (irri_prt_32$prt_irri) ] <- 0

#stat
irri_prt_32 %>% group_by(season,farmers_hh) %>% summarise(mean(prt_irri,na.rm = T ))

### cultivated seasons
md032 <-irri_prt_32 %>% group_by(season) %>%
  t_test(prt_irri  ~ farmers_hh , detailed = T) %>% add_significance()%>% 
  mutate(`.y.`="prt_cult_season") 

# t test  md021 md022 
table3132=bind_rows(md031,md032) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,season ,Inside_Ramthal,Outside_Ramthal,estimate,conf.low,conf.high,t,df,p,p.signif)
nice_table(table3132)

table2122%>%
  ggplot(aes(x = season, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))


### t_test   plots | revenue     ----

total_acre  # total_num_plots | total_acre
plotNew # new_plot_num | new_plot_acre

### t_test plots_num_acre 

library(rstatix)
library(rempsyc)
t11 <- plots_num_acre %>% t_test(total_acre  ~ farmers_hh , detailed = T) %>% add_significance()
t12 <- plots_num_acre %>% t_test(total_num_plots   ~ farmers_hh , detailed = T) %>% add_significance()

# BASLINE
land_bl <- baseline_RMTL%>% select(hh_id,D2,D2_acer,D2_guntas,D3) %>% 
  rename( total_acres=D2 , total_plots=D3) %>% right_join(a_sample[,1:2])
d1 <- land_bl %>% t_test(total_acres  ~ farmers_hh, detailed = T) %>% add_significance()
d2 <- land_bl %>% t_test(total_plots  ~ farmers_hh, detailed = T) %>% add_significance()

# MID 2022
t21 <- plotNew %>% t_test(new_plot_acre  ~ in1_out0 , detailed = T) %>% add_significance()
t22 <- plotNew %>% t_test(new_plot_num   ~ in1_out0 , detailed = T) %>% add_significance()

table100=bind_rows(t11,t12,d1,d2) %>% 
  rename(Inside_Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Inside_Ramthal,Outside_Ramthal,
         estimate,conf.low,conf.high,t,df,p)
nice_table(table100)
rm(t11,t12,t21,t22)

table100[c(1,3),] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "acres per HH" ,x = "", y = "Acre") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))

table100[c(4),] %>%
  ggplot(aes(x = .y., y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange(size = 0.3,color = "royalblue") + 
  labs(title = "total plots per HH",x = "", y = "number of plots") + theme_minimal()+  theme(text = element_text(size = 12, family = "serif"))

##################### Economic outcome ##################### ----
# 🟩   REVENUE ----
### t_test   revenue_per_acre_SEASON | distance_km ----
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

#   ggplot ----
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

#   ggplot ----
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

### t_test    revenue_per_acre_SEASON | irri method ----
# L48_irri_rain a_irri_method 

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


### t_test    income asset

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
  left_join(a_sample [,1:2])

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

