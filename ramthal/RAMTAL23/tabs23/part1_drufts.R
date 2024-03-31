
#|🟡rmtl_baseline2016 |🟠rmtl_midline2018 |🟣rmtl_srvy22

#|🟩Cultivation      methods[L48a] ; source[L7]
#|🟦Water(Ramthal)   Infrastructure[mm4] ; WATER USAGE[mm5]
#|🟨Income,Assat  
#|🟪Social

library(dplyr)
library(haven)
library(tidyr)
library("stringr") #"str_replace"

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )
library(tidyverse)
----------------------------------------------- # essentials               -----
#|🟥🟧🟨🟩🟦🟪⬛⬜🟫 
#|🟦🟩

select(where(~!all(is.na(.x)))) 

############################ t test ###

t_L48a <- irrigation_season.HH %>% group_by(season) %>%
  t_test(hh_irri ~ farmers_hh , detailed = T) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(season,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(t_L48a,
           title = c("Table 48a | Fraction of hh who use irrigation, by season"),
           note = c("[L48a] What is the method of irrigation?","🟩" ))

############################ full_seasons
full_seasons <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id)%>% mutate(kha=1,rab=1,KHA22=1) %>% 
  pivot_longer(!c(farmers_hh,hh_id) , names_to = "season", values_to = "count") %>% 
  select(farmers_hh,hh_id,season)

############################ df
data.frame(
  df = c("rmtl_baseline2016", "rmtl_midline2018", "rmtl_srvy22"),
  o = c("in1_out0","hh_2022",""),
  farmers_hh = c("farmers_hh", "inside_ramthal","outside_ramthal")
) %>% kable() %>% kable_material()

library(kableExtra)
df <- data.frame(df = c("a_irri_rain_method", "", "")
                 ,hh = c(1578, 1609, 1612),
                 X3_season = c(4734, 4827, 4836))
kable(df, format = "html") %>%kable_styling()

irri_prt_02 %>% arrange(hh_id)
irri_prt_02 %>% filter(hh_id %in% c(100019,1000011)) %>% arrange(hh_id)

add_significance(cutpoints = c(0,1e-04,0.001,0.01,0.05,1),symbols = c("****","***","**","*","ns"))
add_significance(cutpoints = c(0,0.12,1),symbols = c("*","ns"))


scale_fill_manual(values=c(  "#E69F00","skyblue")) +
  theme_minimal()+  theme(text = element_text(family = "serif"))
theme(text = element_text(family = "serif", size = 16),axis.title = element_text(size = 18),axis.text = element_text(size = 14))
theme_sjplot() #library(sjPlot)
theme_nice() #library(jtools) 

# colors

"Aquaculture";     "dodgerblue" 
"Cultivated Land"; "#a1d99b"

"Saptari";                "darkolivegreen4"
"Rautahat\nBara Sarlahi"; "lightsalmon4" 

"Total Area Cultivated" :"steelblue2"
"Area Irrigated" :       "steelblue"

"Winter" : "dodgerblue4"
"Monsoon" :"dimgrey"
"Summer" : "darkolivegreen4" 


#Mapping plots
# Mapping plots that have more than one irrigation method per plot. Total | 15 plots [14 farmers] | 9 inside 6 outside | All irrigated|
ts=md %>% select(hh_id , season ,plotID,cropIrri,plot_3methods  ) %>% pivot_wider(names_from = cropIrri, values_from = plot_3methods)
ts$ex=ifelse(ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3 & ts$cropIrri_3==ts$cropIrri_4|
               ts$cropIrri_1==ts$cropIrri_2 & ts$cropIrri_2==ts$cropIrri_3|ts$cropIrri_1==ts$cropIrri_2 ,1,NA)
ts$ex=ifelse(is.na(ts$cropIrri_2),1,ts$ex)

ts %>% left_join(a_sample) %>% filter(is.na(ex)) %>% count(farmers_hh )
#  | 15 plots [14 farmers] | 9 inside, 6 outside | All irrigated|
#  I leave them for analysis marked as irrigated/drip

#🟩   IRRIGATION:  HH freq | methods [HH wise]                             ----

## Percentage of households practicing irrigation-----

#🟣 
#|🟩 [L48a] #What is the method of irrigation? 

  # Kharif_2021 / rabi_2021_22 / Kharif_2022

irrigation_season.HH <- # former hh_irr_season <- 
  a_irri_rain_method %>% # A tibble: 6,740 × 11
  group_by(hh_id,season )  %>%
  mutate(hh_6methods  =  # make it  ONE irri method for ONE hh/season 
           ifelse("drip" %in% irri_method , "drip",
                  ifelse(any(irri_method  == "furrows"), "furrows",
                         ifelse(any(irri_method  == "flood"), "flood",
                                ifelse(any(irri_method  == "sprinkler"), "sprinkler",
                                       ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% 
  ungroup() %>%  select(hh_id,season,hh_6methods ) %>% distinct() %>% 
  mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
         hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% left_join(hh_2022)

##### STAT
irrigation_season.HH %>% group_by(season,farmers_hh) %>% summarise(mean(hh_irri)) %>%  count(hh_irri) %>% group_by(season,farmers_hh) %>% mutate( n/sum(n)) %>% mutate_at(5,round,2)
t_L48a <- irrigation_season.HH %>% group_by(season) %>%t_test(hh_irri ~ farmers_hh , detailed = T) %>% rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% select(season,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(t_L48a,title = c("Table 48a | Fraction of hh who use irrigation, by season"),note = c("[L48a] What is the method of irrigation?","🟩" ))

--------------------------------------------------------------------------------

# 2021 No seasons 
  
irrigation_HH <- 
  a_irri_rain_method %>% filter(season != "KHA22") %>%  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods = ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() %>% 
  mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
         hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% left_join(hh_2022)

##### STAT
irrigation_HH %>% group_by(farmers_hh) %>% summarise(mean(hh_irri )  )
mt01 <-  irrigation_HH %>% t_test(hh_irri ~ farmers_hh , detailed = T)


#🟡  BASELINE
#|🟩 [D12] # Has this plot been irrigated at least once during the last 5 years?
bl_hh_irrigate = rmtl_baseline2016 %>%  select(in1_out0,hh_id,starts_with("D12") ,-c(D12_11:D12_17 )) %>% 
  mutate(total_irriPlot = rowSums (.[names(.)[3:12]], na.rm = T)) %>%
  rename( hh_irrigate_BL= D12_) %>% select(in1_out0,hh_id,hh_irrigate_BL )

#####STAT
bl_hh_irrigate %>% count(in1_out0,hh_irrigate_BL) %>% group_by(in1_out0) %>%  mutate(n/sum(n)) 
mt_bl01 <-bl_hh_irrigate %>% rename(bl_last_5years=hh_irrigate_BL ) %>% t_test(bl_last_5years ~ in1_out0 , detailed = T) 

#🟠  MIDELINE 2018
# "ml18_irri_methods" IN "mid2018.R"
#🟩  ###### (D13_A	Has this plot been irrigated at least once in the past year)?
#|  [D15_A]  What methods?(Mark all) 1Flood 2Furrows 3Drip 4Sprinkler 5Manual 6Hose -888Other(specify)

##### STAT
ml18_irri_methods %>%mutate(hh_irrigation = ifelse(irri==0,0,1 )) %>% count(farmers_hh , hh_irrigation) %>% group_by(farmers_hh) %>% mutate(grp=n/sum(n))
ml18_15a <- ml18_irri_methods %>% mutate(hh_irrigate_past_year = ifelse(irri==0,0,1 )) %>% 
  t_test(hh_irrigate_past_year ~ farmers_hh , detailed = T)

##### t-test table_2H
table_2H=bind_rows(mt01, ml18_15a, mt_bl01) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y.,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
table_2H$.y.[table_2H$.y.== "bl_last_5years"] <- "2012-2016"
table_2H$.y.[table_2H$.y.== "hh_irrigate_past_year"] <- "2017-2018"
table_2H$.y.[table_2H$.y.== "hh_irri"] <- "2021-2022"
names(table_2H)[names(table_2H) == ".y."] <- "Crop year"

nice_table(table_2H, # W700 H400
           title = c("Table 2H", "Share of households who use irrigation"),
           note = c("", 
                    "Baseline2016: Has this plot been irrigated at least once during the last 5 years? [d12]",
                    "Midline 2018: Has this plot been irrigated at least once in the past year? [d13a,d15a]",
                    "Midline 2022: What is the method of irrigation? [L48a]" ))


#hh_6methods -----


#🟣 MID22 hh_6methods
hh_irrigation %>% count(farmers_hh,hh_6methods ) %>% group_by(farmers_hh) %>%  mutate(n/sum(n)) 
.
#🟡  BASELINE hh_6methods
# D21 start in "base16.R"
bl16_irri_methods %>% count(farmers_hh,irri_method) %>% 
  group_by(farmers_hh) %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(4,round,3)%>% filter(!is.na(farmers_hh))

#🟠 MIDELINE 2018 hh_6methods
ml18_irri_methods %>%filter(!is.na(farmers_hh))  %>% count(farmers_hh, irri_method) %>% 
  group_by(farmers_hh) %>% mutate(grp=n/sum(n)) %>% mutate_at(4,round,3)

#drip OF all  hh -             ----

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




#drip OF irri                -----

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

# 🟩 IRRI SOURCE  [HH wise]     ----
# `source` as sum of rank_1_2_3

# 🟡 BASELINE
# bl_source_irrigate2 in df16.R
#1	Canal #2	Tank #3 Open well #4	River/Pond/Lake # 5	Bore well

bl13_source_irrigate %>% group_by(farmers_hh ,irri_source_5y) %>% 
  count() %>% 
  group_by(farmers_hh) %>% mutate(n/sum(n)) # %>% mutate_at(4,round,3)

bl16_source <-bl13_source_irrigate %>% 
  mutate(sources_Not_rain =ifelse(irri_source_5y =="0",0,1)) %>% 
  mutate(Borewell =ifelse(irri_source_5y =="5",1,0))

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
attr(rmtl_srvy22$l7_rank_1, "labels")
# rank 1
L7_source_irri1 %>% 
  group_by(farmers_hh, l7_rank_1) %>% 
  summarise(n=n()) %>% mutate(n/sum(n)) %>% mutate_at(4,round,2 )

# rank 2
L7_source_irri1 %>% 
  group_by(farmers_hh, l7_rank_2) %>% 
  summarise(n=n()) %>% mutate(n/sum(n)) %>% mutate_at(4,round,2 )

# irrigation from Gov supply
a_irri_rain_method %>% 
  group_by(hh_id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(hh_id,hh_6methods ) %>% distinct() %>% 
  left_join(a_source_irri[,c(1:2,7)] ) %>% 
  
  filter(irri_source_num ==5 ) %>%  
  group_by(farmers_hh) %>% 
  count(hh_6methods) %>% mutate(sum=ifelse(farmers_hh=="inside_ramthal",929,649 ), n/sum) %>% mutate_at(5,round,2)

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

###  River GovSupply    table_2s
table_2s=bind_rows(sr23,sr35) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(y  =c( "[ml18] River","[ml22] GovSupply" )  ) %>%  
  select(y  ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_2s,title = c("Table 2s", ""),
           note = c( "ml18,2017-2018,[D14_A] | ml22,2021-2022,[L7]" ))


###  sources_Not_rain   table_20s 
table_20s=bind_rows(sr10,sr20,sr30) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(sources_Not_rain  =c("2012-2016 [bl]","2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(sources_Not_rain  ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 

nice_table(table_20s,title = c("Table 20s", ""),
           note = c( "bl[d21] | ml18[D14_A] | ml22[L7]" ))

###  Borewell           table_21s      
table_21s=bind_rows(sr11,sr21,sr31) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(Borewell   =c("2012-2016 [bl]","2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(Borewell   ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_21s,title=c("Table 21s",""),note=c("bl[d21] | ml18[D14_A] | ml22[L7]"))

###  Canal              table_22s 
table_22s=bind_rows(sr22,sr32) %>% 
  rename(Ramthal=estimate1,Outside_Ramthal =estimate2,t=statistic ) %>% 
  mutate(Canal    =c("2017-2018 [ml18]","2021-2022 [ml22]" )  ) %>%  
  select(Canal    ,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_22s,title = c("Table 22s", ""), note = c( "ml18[D14_A] | ml22[L7]" ))


#🟩   own/cultivated land acre [HH-plot-season] ----
#🟡 bl_irri_acre_plot
# DF bl_season_plot_irri in "df16.R" 

bl_irri_acre_plot <-  # df is plot wise
  bl_season_plot_irri  %>% # [6,332]   hh_id [1,703]
  mutate(hh_6methods =  
           ifelse(irri_method_5y == 3, "drip",
                  ifelse(irri_method_5y  == 2, "furrows",
                         ifelse(irri_method_5y  == 1, "flood",
                                ifelse(irri_method_5y  == 4, "sprinkler",
                                       ifelse(irri_method_5y %in% c(5,6), "hose",
                                              "rain"))))) ) %>%
  rename(plot_irrigated= irri_plot_3s ) %>% 
  mutate(acre_irrigate=ifelse(plot_irrigated == 1, plot_acre, 0)) %>% 
  mutate(acre_drip=ifelse(hh_6methods =="drip", plot_acre ,0) )

# seasonal cultivated land
bl_irri_acre_plot %>%  
  filter(!is.na(farmers_hh) ) %>% 
  group_by(season, farmers_hh) %>% 
  summarise(plot_acre =mean(plot_acre,na.rm = T ),n())

#### ttest
ir3 <-bl_irri_acre_plot %>% group_by(season) %>% t_test(plot_acre ~ farmers_hh , detailed = T)

#🟠 
ml18_plot <- 
  ml18_irri_acre_plot %>%
  filter(!is.na(farmers_hh) ) %>% 
  group_by(season,hh_id , farmers_hh) %>% 
  summarise(plot_acre =sum(acres ,na.rm = T )) 


#### ttest
ir3 <-ml18_plot %>% group_by(season) %>% t_test(plot_acre  ~ farmers_hh , detailed = T)

#🟣 irri_acre_plotID

irri_acre_plotID <- #  [HH-season-plot] # md # irrigation_drip_plot 
  a_irri_rain_method %>% # [1,578 hh_id] # [6,740 rows]
  select( hh_id ,season ,plotID ,irri_method) %>% distinct() %>%  # [6,242]
  group_by(hh_id , season, plotID)  %>%  # make it  ONE irri method for ONE plot 
  mutate(hh_6methods =  ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>%
  ungroup() %>% select(hh_id, season, plotID ,hh_6methods) %>% distinct() %>% 
  left_join(a_plots_size[,c(1:2,7)])  %>% 
  mutate(plot_irrigated=ifelse(hh_6methods == "rain",0,1)) %>%
  mutate(acre_irrigate=ifelse(plot_irrigated ==1,acres,0 )) %>% 
  mutate(acre_drip=ifelse(hh_6methods =="drip", acres,0) ) %>% 
  left_join(hh_2022 )
  

irri_acre_plotID %>%
  group_by(hh_id ,season, farmers_hh) %>% summarise(acres=sum(acres)) %>%
  group_by(season, farmers_hh) %>% summarise(acres=mean(acres))

#### ttest
cr44 <-irri_acre_plotID %>% group_by(season) %>% t_test(acres ~ farmers_hh , detailed = T)
cr44$season <- c("Kharif_2022","kharif_2021","rabi_2021_22" )
nice_table(cr44)



#🟩  irrigated land acre [HH-season]              ----

#⬛ DF cultivated seasons                             

#  🟣 irri_acre_season
irri_acre_season <- #  irrigation = acres | rain = 0
  irri_acre_plotID %>% 
  group_by(farmers_hh,hh_id,season) %>% summarise(acre_irrigate =sum(acre_irrigate ))

irri_acre_season %>% group_by(season,farmers_hh) %>% summarise(mean(acre_irrigate))


#  🟡  bl_irri_acre
bl_irri_acre <- 
  bl_irri_acre_plot %>% 
  group_by(farmers_hh,hh_id, season ) %>% 
  summarise(acre_irrigate=sum(plot_acre,na.rm = T))

bl_irri_acre %>%  group_by(season, farmers_hh) %>% summarise(acre_irrigate=mean(acre_irrigate,na.rm = T))

#### ttest
ir3 <-bl_irri_acre %>% group_by(season) %>% t_test(acre_irrigate ~ farmers_hh , detailed = T)

table_ir=ir3 %>% 
  rename(Ramthal=estimate1,Outside_Ramthal=estimate2,t=statistic) %>% 
  select(.y. ,season,Ramthal,Outside_Ramthal,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_ir)



#  🟠     NOT DONE                            ----

# DF [ml18_irri_acre_plot] in "df18.R" 

ml18_IRRI_acre_plotId <- #  [HH-season-plot] # md # irrigation_drip_plot 
  ml18_irri_acre_plot %>% # [ hh_id] # [ # A tibble: 4,519 × 7]
#  select(farmers_hh, hh_id,season, irri_method) %>% distinct()
  group_by(farmers_hh, hh_id, season,irri_method)  %>%
    summarise(acres =sum(acres,na.rm = T )) %>% 
  mutate(acre_irrigated=ifelse(irri_method  == "rain", 0 , acres)) %>% 
  mutate(acre_drip=ifelse(irri_method =="drip", acres  ,0) ) %>% 
  filter(acres!=0 )
  
  
ml18_IRRI_acre_plotId %>%  group_by(season, farmers_hh) %>% summarise(acre_irrigated=mean(acre_irrigated))

#### ttest
ir3 <-bl_irri_acre %>% group_by(season) %>% t_test(acre_irrigated ~ farmers_hh , detailed = T)



#⬛ DF drip irrigation                             

#🟣 
drip_acre_season <-  # drip = acres | irrigation = 0 | rain = 0
  irri_acre_plotID %>% 
  group_by(farmers_hh, hh_id,season) %>% summarise(acre_drip=sum(acre_drip))
  
drip_acre_season %>% group_by(season,farmers_hh) %>% summarise(mean(acre_drip))

# 🟡 

bl_irri_acre %>%  group_by(season, farmers_hh) %>% summarise(acre_drip=mean(acre_drip))

bl_irri_acre %>%filter(acre_drip != 0) %>%   group_by(season, farmers_hh) %>% summarise(acre_drip=mean(acre_drip),n())
bl_irri_acre %>%filter(acre_drip == 0) %>%   group_by(season, farmers_hh) %>% summarise(n())


ir4 <-bl_irri_acre %>% group_by(season) %>% t_test(acre_drip ~ farmers_hh , detailed = T)


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




# prt of irrigated land      
----

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


### t_test   plots | revenue

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


# 🟩   REVENUE ----
### t_test   revenue_per_acre_SEASON | distance_km
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

#   ggplot 
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


### t_test    revenue_per_acre_SEASON | water source 
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

#   ggplot 
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

### t_test    revenue_per_acre_SEASON | irri method
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

A=rmtl_srvy22 %>%
  select(plot_size_acre_1,plot_size_acre_2,plot_size_acre_3,
         plot_acre_sum,plot_acre_guntas_for_round,plot_acre_guntas_div,
         plot_acre_guntas,
         plot_acre_1,plot_acre_2,plot_acre_3,hh_id)



# total plots option 1               ## total_plots1        ----
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


# total acre + total plots option 2  ## plots_num_acre        ----

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



# total new plots                     ## plotNew             ----
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
  
