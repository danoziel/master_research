library(dplyr)
library(tidyverse)
library(haven)
library("stringr") #"str_replace"

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )

#🟦  mm2 gov project but Ramthal      ----
#Is your land coming under such a government project?	if yes - which?	 
#1 Ramrhal | 2	Krishi Honda ( Farm pond) | 3	Pradhan Mantri Krishi Sinchai Yojana(PMKSY) | 4Ganga kalyana | 5none

rmtl_srvy22 %>% select(farmers_hh,hh_id,mm2) %>% count(farmers_hh,mm2)

#| 2Krishi Honda(Farm pond) 8in 9out
#| 4Ganga kalyana 2in 4out

#🟦  mm4 Infrastructure               ----

#🟣 MID22 
mm4 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id,mm4) %>% 
  left_join(hh_2022) %>%   
  rename(infrastructure_installed=mm4)

# stat
mm4 %>% count(farmers_hh,infrastructure_installed) %>% group_by(farmers_hh) %>% mutate(n/sum(n))

# t test
wi1 <-mm4 %>%t_test(infrastructure_installed ~ farmers_hh , detailed = T) %>% add_significance() 

table_wi= wi1 %>% 
  select(estimate1,,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Project farms`=estimate1,`No Project farms`=estimate2,t=statistic) 
nice_table(table_wi, title = c("Table mm4", "Fraction of HH with infrastructure installed"))

#🟠 MIDELINE 2018

Y2018=rmtl_midline2018 %>%  
  select(hh_id,in1_out0)

# 1Yes / 2No / -999 Don't know

#| i16 # Is your land taking part in the Ramthal project?"

#[17,15 are plot wise]
#| i17 # Has any drip irrigation pipes been laid on top of the field
#       (Small black pipes laid over the ground and used for drip irrigation)
#| i15.a	# Were you offered irrigation water -Y/N
#| I15.b	# If yes, Did you use water offered
#| I15.c	# If yes, When did you start using the water

i16.17.15ab <- Irrigation_Midline_Clean_with_Callbacks %>% 
  select(id, i16, 
         matches("^i17_\\d+$"), 
         matches("^i15_A"), 
         matches("^i15_B")) %>%
  mutate(
    across(-id, ~ if_else(. == "2", "", .)),
    i16 = as.integer(ifelse(i16==1, 1,0)),
    i17 = as.integer(if_any(starts_with("i17_"), ~ . == "1")),
    i15a = as.integer(if_any(starts_with("i15_A"), ~ . == "1")),
    i15b = as.integer(if_any(starts_with("i15_B"), ~ . == "1"))
  ) %>%
  select( id,i16,i17,i15a,i15b) %>% 
  rename(i_infr=i17, i_offered_water=i15a,i_use_water=i15b )

i15c <- Irrigation_Midline_Clean_with_Callbacks %>%
  select(id, matches("^i15_C_yyyy")) %>%
  rowwise() %>%
  mutate(i15c=max(c_across(starts_with("i15_C_yyyy")), na.rm = TRUE))%>%
  ungroup() %>% 
  mutate(start_use_water=ifelse(i15c %in% c("2017","2018"),1,0 )) %>%
  select( id,start_use_water )

wI2018 <- inner_join(i16.17.15ab,i15c ) %>% rename(hh_id =id)

wi2018 <- Y2018 %>% 
  filter(!is.na(in1_out0)) %>% 
  left_join(wI2018)


wi2018 %>% count(in1_out0,i_infr) %>% mutate(pct = n / sum(n), .by = in1_out0)
wi2018 %>% count(in1_out0,i_offered_water) %>% mutate(pct = n / sum(n), .by = in1_out0)
wi2018 %>% count(in1_out0,i_use_water) %>% mutate(pct = n / sum(n), .by = in1_out0)
wi2018 %>% count(in1_out0,start_use_water) %>% mutate(pct = n / sum(n), .by = in1_out0)


#🟩  L7  ir source                    -----

a_source_irri %>%  # Freq: source_type
  left_join(rmtl_InOut) %>% 
  group_by(farmers_hh,source_type) %>%  summarise(n=n()) %>% mutate(pct=n/sum(n)) %>% 
  mutate_at(4,round,2) %>% select( -n) %>% 
  pivot_wider(names_from = farmers_hh, values_from = pct)

a_source_irri %>% # Freq: source
  left_join(rmtl_InOut) %>% 
  group_by(farmers_hh,source) %>%  summarise(n=n()) %>% mutate(pct=n/sum(n)) %>% 
  mutate_at(4,round,2) %>% select( -n) %>% 
  pivot_wider(names_from = farmers_hh, values_from = pct)

######################## t test L7a  rmtl_InOut_groups
ir_source <- 
  a_source_irri %>% 
  left_join(rmtl_InOut_groups) %>% 
  select(hh_id,source_type,farmers_hh ) %>% mutate(frq=1) %>%   
  pivot_wider (names_from = source_type, values_from = frq) %>% 
  pivot_longer(!c(hh_id,farmers_hh),names_to = "source",values_to = "frq") %>% 
  mutate(frq=ifelse(is.na(frq),0,frq))

table_L7a <- 
  ir_source %>% group_by(source) %>%
  t_test(frq ~ farmers_hh , detailed = T) %>% 
  select(source,estimate1,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Project farms`=estimate1,`No Project farms`=estimate2,t=statistic) 
nice_table(table_L7a, title = c("Table L7a", "Fraction of HH with irrigation water source"))



######################## t test L7b | rmtl_In drip
source_In_drip <- 
  a_source_irri %>% 
  right_join(rmtl_In) %>% 
  select(hh_id,source_type,drip_use ) %>% mutate(frq=1) %>%   
  pivot_wider (names_from = source_type, values_from = frq) %>% 
  pivot_longer(!c(hh_id,drip_use),names_to = "source",values_to = "frq") %>% 
  mutate(frq=ifelse(is.na(frq),0,frq),
         drip_use=ifelse(drip_use==1,"Drip","No_drip"))

table_L7b <- 
  source_In_drip %>% group_by(source) %>%
  t_test(frq ~ drip_use , detailed = T) %>% 
  select(source,estimate1,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Drip`=estimate1,`No drip`=estimate2,t=statistic) 
nice_table(table_L7b, title = c("Table L7b", "Fraction of farms with/without drip-irrigation using water source"))


######################## t test L7c | rmtl_In irrigation
source_In_ir <- 
  a_source_irri %>% 
  right_join(rmtl_In) %>% 
  select(hh_id,source_type,ir_use ) %>% mutate(frq=1) %>%   
  pivot_wider (names_from = source_type, values_from = frq) %>% 
  pivot_longer(!c(hh_id,ir_use),names_to = "source",values_to = "frq") %>% 
  mutate(frq=ifelse(is.na(frq),0,frq),
         ir_use=ifelse(ir_use==1,"ir","No_ir"))

table_L7c <- 
  source_In_ir %>% group_by(source) %>%
  t_test(frq ~ ir_use , detailed = T) %>% 
  select(source,estimate1,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Irrigation`=estimate1,`Non-irrigation`=estimate2,t=statistic) 
nice_table(table_L7c, title = c("Table L7c", "Fraction of farms with/without irrigation using water source"))






  
  

# 3 | Irrigation ----
#______3.1 Users' fraction ----

# Table 21H B | ------------------------------------------------------------------
# Fraction of household who uses drip-irrigation in 2017-2022

### 2021-22
md22 <-  #🟣 MID22  
  rmtl_InOut %>% t_test(hh_drip_2021_22 ~ farmers_hh, detailed = T)

### 2020-21 #🟣 MID22
### 2019-20 #🟣 MID22
### 2018-19 #🟣 MID22
### 2017-18
#🟠 MIDELINE 2018

### Baseline 2012-16
#🟡  BASELINE 

### 2017-21
rmtl_InOut %>% 
  t_test(drip_use  ~ farmers_hh, detailed = T) %>% 
  select(estimate1,,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Project farms`=estimate1,`Not Project farms`=estimate2,t=statistic) 


# Table 21H B | ------------------------------------------------------------------
# Total household who uses irrigation in 2017-2022

### 2021-22
#🟣 MID22
mt01 <-  irrigation_HH %>% t_test(hh_irri ~ farmers_hh , detailed = T)

### 2020-21
#🟣 MID22

### 2019-20
#🟣 MID22

### 2018-19
#🟣 MID22

### 2017-18
#🟠 MIDELINE 2018
#"ml18_irri_methods" IN "df18.R" 
ml18_15a <- 
  ml18_irri_methods %>% 
  mutate(hh_irrigate_past_year = ifelse(irri==0,0,1 )) %>% 
  t_test(hh_irrigate_past_year ~ farmers_hh , detailed = T)

### 2012-16
#🟡  BASELINE 
# [D12] # Has this plot been irrigated at least once during the last 5 years
bl_hh_irrigate = rmtl_baseline2016 %>%  select(in1_out0,hh_id,starts_with("D12") ,-c(D12_11:D12_17 )) %>% 
  mutate(total_irriPlot = rowSums (.[names(.)[3:12]], na.rm = T)) %>%
  rename( hh_irrigate_BL= D12_) %>% select(in1_out0,hh_id,hh_irrigate_BL )
mt_bl01 <-
  bl_hh_irrigate %>% rename(bl_last_5years=hh_irrigate_BL ) %>% t_test(bl_last_5years ~ in1_out0 , detailed = T) 

### 2017-21
rmtl_InOut %>% 
  t_test(ir_use  ~ farmers_hh, detailed = T) %>% 
  select(estimate1,,estimate2,statistic,df,p,conf.low,conf.high) %>% 
  rename(`Project farms`=estimate1,`Not Project farms`=estimate2,t=statistic) 


# Table 48a | ----------------------------------------------------------------
# A fraction of households irrigated in 2021-22, 6 years after the project was launched
# [L48a] What is the method of irrigation?

t_L48a_1 <- irrigation_season.HH %>% group_by(season) %>%t_test(hh_drip ~ farmers_hh , detailed = T) 
t_L48a_1=t_L48a_1[c(2,3,1),]
t_L48a_2 <- irrigation_season.HH %>% group_by(season) %>%t_test(hh_irri ~ farmers_hh , detailed = T) 
t_L48a_2=t_L48a_2[c(2,3,1),]

t_L48a <- 
  rbind(t_L48a_1,t_L48a_2) %>% 
  mutate_at(c(3:4,10:14) ,round,2) %>%
  rename(Project=estimate1,`Non-project`=estimate2,t=statistic) %>% 
  mutate(CI = paste0("[", conf.low, ", ", conf.high, "]")) %>% 
  select(season,Project,`Non-project`,t,p,df,CI)

kbl(t_L48a, caption = "Table 48a | Fraction of hh who use Drip irrigation/irrigation, by season") %>%
  kable_paper() %>%
  pack_rows("Drip irrigation", 1, 3) %>%
  pack_rows("Irrigation", 4, 6)







#______3.2.Irrigated Land ----
# Table LD | 2021-22 -----------------------------------------------------------  

# Land holding
land_holding_In=
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(total_num_plots=n(),total_acre=sum(acres,na.rm = T)) %>% 
  filter(total_acre<39) %>%  # 99% removed. see LAND HOLDING in "inmpact.R"
  right_join(rmtl_In)
  
land_holding_In %>% t_test(total_acre  ~ drip_use  , detailed = T) %>% select(2:3,9:13)


# Cultivated land

cultivated_land_In =
  land_holding %>% 
  left_join(a_irri_rain_method) %>% # filter(hh_id==101046)
  left_join(a_plots_size[,c(1:2,7)]) %>% 
  select(hh_id, season, plotID, irri_method,acres) %>% distinct() %>% 
  group_by(season ,hh_id) %>% 
  summarise(total_acre=sum(acres,na.rm = T)) %>%
  right_join (full_seasons) %>% 
  mutate(total_acre=ifelse(is.na(total_acre),0,total_acre)) %>%
  ungroup() %>% 
  right_join(rmtl_In  %>% select(hh_id,drip_use) ) %>% 
  mutate(drip_use=ifelse(drip_use==1,"Drip","NoDrip")) %>% 
  filter(!is.na(season) ) 

quantile(cultivated_land_In$total_acre, 0.99)

cultivated_land_In %>%
  filter(total_acre<29.8315)%>% 
  group_by(season) %>%  
  t_test(total_acre  ~ drip_use , detailed = T
         )%>% 
  mutate_at(c(3:4,10:14) ,round,2) %>%
  mutate(CI = paste0("[", conf.low, ", ", conf.high, "]")) %>% 
  select(season ,estimate1, estimate2, statistic,p,df,CI) %>%  
  kbl() %>% kable_paper()










# mw 2		Have you used it in [ _____ ]?
attr(rmtl_srvy22$mw1a, "labels")
rmtl_srvy22 %>% select(hh_id, starts_with("mw1") )
# How many years in total did you ever make use of the water for irrigation 
#                                              during Kharif[mw5]/rabi[mw6] ?
mw5_mw6 <- 
  rmtl_srvy22 %>% 
  # filter(farmers_hh=="inside_ramthal",mm4==1) %>%  
  select(hh_id,mw5,mw6, mw2) %>%
  mutate(year_of_use=pmax(mw5,mw6)) %>% 
  filter(year_of_use>=1) %>% 
  mutate(years_use_2022=ifelse(year_of_use>=3,"3-6",year_of_use))

mw5_mw6 %>% count(years_use_2022) %>% mutate(N=sum(n),pct=n/N)





# WATER moudul ----
############ m3   advantages of drip irrigation  ----
#Are you aware of any advantages of drip irrigation over other irrigation methods?	
# 0	No knowledge # 1	Increased yields # 2	Water saving
# 3	Fertilizer saving # 4	Less weeds # 5	Less labor requirements # 6	Other

rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("m3_") )

rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m3_0:m3_5 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(farmers_hh,ans) %>% summarise(n=sum(yn)) %>%  
  #group_by(farmers_hh) %>% mutate(N=sum(n),prt_mm4= n/N ) % of ans
  mutate(prt2=ifelse(farmers_hh=="inside_ramthal",n/946,n/666)) %>% # % of respondents
  filter(prt2<0.05|prt2>0.49) %>% 
  mutate(prt2=prt2*100) %>% mutate_at(4,round) %>% 
  kbl() %>% kable_styling()

rmtl_srvy22 %>% select(hh_id,m3_0:m3_5 ) %>%
  pivot_longer(-hh_id, names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>% summarise(n=sum(yn))%>%  mutate(prt=n/1612)

############ m4   ALLOWED TO TAKE PART ----
#"Who is deciding which land and farmers are ALLOWED TO TAKE PART in this drip irrigation project?
# "	1	Government # 2	Company # 4	Anyone who wants to can join # 5	Panchayat # -999	Don't know #-888	Other (specify)

rmtl_srvy22 %>% select(farmers_hh,hh_id,starts_with("m4_") )

rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m4_1:m4_5,m4__999 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(farmers_hh,ans) %>% summarise(n=sum(yn))%>%  
  mutate(prt2=ifelse(farmers_hh=="inside_ramthal",n/946,n/666)) 

rmtl_srvy22 %>% select(hh_id,m4_1:m4_5,m4__999 ) %>%pivot_longer(-hh_id, names_to = "ans",values_to = "yn") %>% group_by(ans) %>% summarise(n=sum(yn))%>%  mutate(prt= n/1612 ) 

rmtl_srvy22 %>% count (m4 ) %>%  mutate(prt= n/1612 ) 

# ttest to the right answer and to "dont know"
rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m4_1,m4__999 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>%t_test(yn ~ farmers_hh , detailed = T) %>% 
  select(ans,estimate1, estimate2,statistic,p,df,conf.low, conf.high) %>% 
  mutate_at(2:8,round,2) %>% kbl() %>% kable_styling()

############ m5		deciding what/who is included  -----
#"On what basis are they deciding what/who is included or not in the project?
# 1	Elevation # 2	Political factors/connections # 3	Wealth # 4	Farming skill # 5	Location # -999	Don't know # -888	Other

rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m5_1:m5_5,m5__999 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(farmers_hh,ans) %>% summarise(n=sum(yn))%>%  
  mutate(prt2=ifelse(farmers_hh=="inside_ramthal",n/946,n/666)) 

rmtl_srvy22 %>% select(hh_id,hh_id,m5_1:m5_5,m5__999) %>%pivot_longer(-hh_id, names_to = "ans",values_to = "yn") %>% group_by(ans) %>% summarise(n=sum(yn))%>%  mutate(prt= n/1612 ) 

rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m5_1:m5_5,m5__999 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>%t_test(yn ~ farmers_hh , detailed = T) %>% 
  select(ans,estimate1, estimate2,statistic,p,df,conf.low, conf.high) %>% mutate_at(2:8,round,2) %>% kbl() %>% kable_styling()

############ mw12 13 14 when water is provided Month and days  ----
# mw12 Typically, in your experience, when water is provided in a particular year, in which month does it start?	
#                                      1	[ Indicate Month ] #2	Dont know
# 
# mw13 Typically, in your experience, when water is provided in a particular year, in which month does it end for the year?
#                                      1	[ Indicate Month ] # 2	Dont know
# 
# mw14 Typically, in your experience, during the period water is provided, how often is it provided?	
#                                      1	Every  _____ Days # 2	Very unpredictable and irreular #	3	Dont know

mw12=rmtl_srvy22 %>% select(farmers_hh, hh_id,mm5,mw12)%>%filter(!is.na(mw12),farmers_hh=="inside_ramthal") %>% count(mw12) %>% mutate(freq=n/sum(n),prt=freq*100) %>% mutate_at(4,round)
barplot(mw12$prt, main="start", horiz=TRUE,names.arg = mw12$mw12)

mw13=rmtl_srvy22 %>% select(farmers_hh, hh_id,mm5,mw13)%>%filter(!is.na(mw13),farmers_hh=="inside_ramthal") %>% count(mw13) %>% mutate(freq=n/sum(n),prt=freq*100) %>% mutate_at(4,round)
barplot(mw13$prt, main="end", horiz=TRUE,names.arg = mw13$mw13)

c("Don't know","January", "February","March", "April","May","June","July","August","September","October","November","December")


mw14= rmtl_srvy22 %>% select(farmers_hh, hh_id,mw14,mw14_int)%>%filter(!is.na(mw14),farmers_hh=="inside_ramthal") %>% 
  count(mw14) %>% mutate(freq=n/sum(n))

mw14_int= rmtl_srvy22 %>% select(farmers_hh, hh_id,mw14,mw14_int)%>% filter(farmers_hh=="inside_ramthal")  %>% 
  summarise(mean(mw14_int,na.rm = T))

############ m35 a b c  Damaged in the irrigation system  ----
m35s <- 
  rmtl_srvy22 %>%select(hh_id,contains("m35")) %>%
  right_join(rmtl_In_groups)

############# m35		
# m35 What is the status of the main pipe coming into your land ?
m35s %>% 
  group_by(waterIR_17_21) %>% 
  freq(m35, report.nas = FALSE, headings = T, cumul= FALSE)
attr(rmtl_srvy22$m35, "labels")



# M35c : What is the status of the laterals?
rmtl_srvy22 %>%select(farmers_hh,hh_id,contains("m35")) %>% filter(farmers_hh=="inside_ramthal") %>% 
  freq(m35c, report.nas = FALSE, headings = T, cumul= FALSE)
attr(rmtl_srvy22$m35c, "labels")

# m35a: What caused the damage?	#1	Animals that graze in the field #2	Rodents #3	Machinery like tractors or threshers #4	Thieves #5	Other Farmers #6	Damaged during operation #7	Company 
rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m35a_1:m35a_7 ) %>% # filter(!is.na(m35a_1)) N=386
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>%
  filter(!is.na(yn) ,farmers_hh=="inside_ramthal") %>% 
  group_by(ans) %>% summarise(n=sum(yn))%>%  mutate(n/386)
  
########### m35b
# How long has the main pipes been damaged?		[months/years]
rmtl_srvy22 %>% select(contains("m35")) %>% select(m35b_month, m35b_year) %>% filter(!is.na(m35b_month)) %>% count()

rmtl_srvy22 %>% select(farmers_hh,hh_id ,m35b_month, m35b_year)%>%
  mutate(month_yr=m35b_year*12+m35b_month ) %>% 
  filter(farmers_hh=="inside_ramthal",month_yr>0 ) %>% 
  summarise(mean(month_yr)/12 )

############ m35c		
# What is the status of the laterals?	# 1	Works, laid in the field # 2	OK, but in storage # 3	Damaged
rmtl_srvy22 %>% select(farmers_hh,hh_id,contains("m35")) %>% filter(!is.na(m35c),farmers_hh=="inside_ramthal") %>%  
  count(m35c) %>%  mutate(N=sum(n),n/N)

############ m36 # did you contact someone to fix ? -----
#1	Contact government office	#2	Contact the company	#3	Contact Water User Association	#4	Dont know who to contact #5	I did not

rmtl_srvy22 %>% select(farmers_hh,hh_id, contains("m36")) %>% 
  filter(!is.na(m36)) %>% count(m36)%>%  mutate(N=sum(n),n/N)
  
############ m37 # did they help you and fix it?	-----
#1	Yes	#2	didnt get no response	#3	They said they would fix it but never came	

rmtl_srvy22 %>% select(farmers_hh,hh_id, contains("m37")) %>% 
  filter(!is.na(m37)) %>% count(m37)%>%  mutate(N=sum(n),n/N)

# INFORMATION - what farmers knows on drip ------

# [m3] Are you aware of any advantages of drip irrigation over other irrigation me...

m3_mm4=rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m3_0:m3_5 ) %>%
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>% 
  left_join(rmtl_srvy22 %>% select(hh_id,mm4,mm5)) %>% 
  group_by(farmers_hh,mm4,ans) %>% summarise(n=sum(yn)) %>% ungroup()

m3_mm4[c(7:18),] %>% group_by(farmers_hh) %>% mutate(N=sum(n),prt_mm4= n/N )



# IR SOURCE "irriRain" ( from L 'cultivation') ----
#   L7 rank irrigation source
# What irrigation source are you dependent on? (Rank according to the degree of importance)

L7_source_irri # df in DF_22.R
a_source_irri  # df in DF_22.R
  

# preq tbl source rank_1
a_source_irri %>%  # df in DF_22.R
  left_join(hh_2022) %>% count(farmers_hh,N_in.out, source_rank1) %>% 
  mutate(pct=(n/N_in.out)*100 
  ) %>% select(farmers_hh, source_rank1, n, pct )


# preq tbl source:  MORE than 1 source to HH
tb_source <- 
  L7_source_irri %>% 
  select(-l7_other ) %>% 
  pivot_longer(cols = -hh_id,names_to = "rank",  values_to = "source") %>% 
  filter(!is.na(source), source != 6)  %>%  left_join(hh_2022)

# TABLE
tb_source %>% count(farmers_hh,N_in.out) %>% 
  mutate(pct_water_source= n/N_in.out, rain= 1-pct_water_source)

# TABLE
tb_source %>% 
  count(farmers_hh,N_in.out,source)%>% 
  mutate(pct=(n/N_in.out)*100 
  ) %>% 
  mutate(Source = case_when(
    source == 2 ~ "pond",
    source == 3 ~ "openwell",
    source == 4 ~ "borwell",
    source == 5 ~ "ramthal",
    source == 7 ~ "canal",
    TRUE ~ NA_character_)) %>% 
  select(farmers_hh, Source, n, pct )

# TABLE
L7_source_irri  %>%  left_join(hh_2022) %>% 
  count(farmers_hh,N_in.out,l7_rank_1 ) %>% 
  mutate(pct=(n/N_in.out)*100 
  )


####### HH%_IR 2016-2021 ----

#| 2016       basline

#| 2017-2018  midline

#| 2018-2019  midline22
#| 2019-2020  midline22
#| 2020-2021  midline22

#| 2021-2022  midline22

# irrigation_2022      ----

a_irri_rain_method %>% count(season)

kharif_21 <- a_irri_rain_method %>% 
  filter(season == "kharif_2021") %>% 
  # filter(season == "rabi_2021_22") %>% 
  # filter(season == "kharif_2022") %>% 
  left_join(hh_2022) %>%  select(farmers_hh, hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods = ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() %>% 
  rename(kharif_2021=hh_6methods)
# rename(rabi_2021_22=hh_6methods)
# rename(kharif_2022=hh_6methods)


krk2=left_join(kharif_21,rabi_21_22) %>% left_join(kharif_22) %>% 
  mutate(kharif_2021=ifelse(kharif_2021=="drip",1,0)) %>% 
  mutate(rabi_2021_22=ifelse(rabi_2021_22=="drip",1,0)) %>% 
  mutate(kharif_2022=ifelse(kharif_2022=="drip",1,0)) %>%
  mutate(Y2022=NA,Y2021=NA)


krk2$Y2022[krk2$rabi_2021_22==0 | krk2$kharif_2022==0 ] <- 0
krk2$Y2022[krk2$rabi_2021_22==1 | krk2$kharif_2022==1 ] <- 1
krk2$Y2022[is.na(krk2$rabi_2021_22) & is.na(krk2$kharif_2022) ] <- NA
krk2$Y2021[krk2$rabi_2021_22==0 | krk2$kharif_2021==0 ] <- 0
krk2$Y2021[krk2$rabi_2021_22==1 | krk2$kharif_2021==1 ] <- 1
krk2$Y2021[is.na(krk2$rabi_2021_22) & is.na(krk2$kharif_2021) ] <- NA


krk2 %>% left_join(hh_2022) %>% 
  group_by(farmers_hh ) %>% 
  summarise(Y2021 = mean(Y2021,na.rm=T)*100,
            Y2022 = mean(Y2022,na.rm=T)*100)



krk_ir=left_join(kharif_21,rabi_21_22) %>% left_join(kharif_22) %>% 
  mutate(kharif_2021=ifelse(kharif_2021=="rain",0,1)) %>% 
  mutate(rabi_2021_22=ifelse(rabi_2021_22=="rain",0,1)) %>% 
  mutate(kharif_2022=ifelse(kharif_2022=="rain",0,1)) %>%
  mutate(Y2022=NA,Y2021=NA)



###### 2021 ir data [survey of 2022] # ir22_2021
ir22_2021 <-  
  a_irri_rain_method %>%  select( hh_id ,irri_method) %>% distinct() %>% 
  group_by(hh_id)  %>%
  mutate(hh_6methods = ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>%
  ungroup() %>% select(hh_id,hh_6methods) %>% distinct() #%>% 
  # mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
  #        hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% left_join(hh_2022)%>% mutate(year_ir=2021) %>% 
  # select(year_ir,farmers_hh,hh_id,hh_6methods,hh_irri,hh_drip )

irrigation_2022 <- 
  ir22_2021 %>% 
  rename(ir_method_2022=hh_6methods  ) %>%
  mutate(ir_use_2022  =ifelse(ir_method_2022=="rain",0,1),
         drip_use_2022=ifelse(ir_method_2022=="drip",1,0))

# irrigation_2018_2020 ----

ir_18_20 <- 
  rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with("l48a_y"),-contains("other")) %>% 
  pivot_longer(cols = -c(farmers_hh,hh_id),names_to = "observation",  values_to = "irri_method") %>% 
  separate(observation, into = c("L48a","y" ,"year_ir","crop15"), sep = "_") %>% 
  mutate(year_ir =as.numeric(year_ir),year_ir=year_ir+2000) %>% 
  mutate(irri_method=ifelse(is.na(irri_method),0,irri_method)) %>% 
  group_by(hh_id,year_ir) %>% 
  mutate(
    ir_method = ifelse(3 %in% irri_method , "drip", 
                  ifelse(any(irri_method  == 2), "furrows",
                  ifelse(any(irri_method  == 1), "flood",
                  ifelse(any(irri_method  == 4), "sprinkler",
                  ifelse(any(irri_method  == 6), "hose","rain"))))) 
    ) %>%  #  filter(hh_id %in% c(103257,105832))
  select(farmers_hh,hh_id,ir_method,year_ir) %>% distinct() %>% ungroup() %>% 
  mutate(ir_use=ifelse(ir_method=="rain",0,1),
         drip_use=ifelse(ir_method=="drip",1,0))

irrigation_2018_2020 <- 
  ir_18_20 %>% select(-farmers_hh) %>% 
  pivot_wider(
    names_from = year_ir, 
    values_from = c(ir_method,drip_use, ir_use),
    names_glue = "{.value}_{year_ir}" )


# irrigation_2017      ----

# ###### [survey of 2018]
# ir18_2017 <- 
#   ml18_irri_methods %>% rename(hh_6methods=irri_method,hh_irri=irri  ) %>%
#   mutate(hh_irri=ifelse(hh_6methods=="rain",0,1),
#          hh_drip=ifelse(hh_6methods=="drip",1,0)) %>% 
#   mutate(year_ir=2017) %>% 
#   select(year_ir,farmers_hh,hh_id,hh_6methods,hh_irri,hh_drip )

irrigation_2017 <- 
  ml18_irri_methods %>% 
  rename(ir_method_2017=irri_method  ) %>%
  mutate(ir_use_2017  =ifelse(ir_method_2017=="rain",0,1),
         drip_use_2017=ifelse(ir_method_2017=="drip",1,0)) %>%  
  select(-irri,-farmers_hh)

# irrigation_BL        ----

irrigation_BL <- 
  bl_ir_method %>% # IN  df16.R
  rename(
    ir_method_BL=ir_method_name,
    drip_use_BL=drip_use,
    ir_use_BL=ir_use) %>% 
  select(-farmers_hh,-irri_method_2016)

# irrigation_BL_to_22  ----

irrigation_BL_to_22 <- 
  full_join(irrigation_2022,
            irrigation_2018_2020) %>% 
  full_join(irrigation_2017) %>% 
  full_join(irrigation_BL)
  








####### HH%_IR 2021-22 SEASONs ----

ir_season_2021_21 <-
  a_irri_rain_method %>% select(season, hh_id ,irri_method) %>% distinct() %>% 
  group_by(season,hh_id)  %>%
  mutate(hh_6methods = ifelse("dri %>% %>% )


# SEASON USAGE ----
rmtl_srvy22 %>% select(farmers_hh,hh_id, contains("mw2"))%>% 
  filter(!is.na(mw2)) %>% count(farmers_hh,mw2) %>% 
  group_by(farmers_hh) %>% mutate (N=sum(n),n/N)


m20= rmtl_srvy22 %>% select(farmers_hh,hh_id,mw2, contains("m20_")) 
m20 %>% filter(!is.na(mw2)) %>% group_by(farmers_hh) %>% summarise(sum(m20_kharif_2018),sum(m20_rabi_2018))
m20%>% filter(!is.na(mw2)) %>% select(farmers_hh,hh_id,ends_with("2018")) %>% 
  mutate(kr18=m20_kharif_2018+ m20_rabi_2018) %>% count(farmers_hh,kr18)

### WATER TAB ### ====

#* [mw2] Have you used it in Kharif/Rabi/Both?
#* [mw1a] If Yes, in which year did you first make use of the water?
#* [mw1b] In what season did you use it in that year?
# [mw1c] If No, Why? | IN part2_why.R 

#* [mw4] Are you still making use of the water from the project to irrigate your land?
#* [mw4a] If Yes, in what season?
#* [mw4b] If No or Sometimes- What was the last year you use of the water?

# [m20_] 2022-2017 did you ever make use kharif_2022

water_tab <-  
  a_rmtl_srvy22 %>% 
  select(hh_id,
         mw2,mw1a,mw1b,mw1c,
         mw4,mw4a,mw4b)

# i34	m52		Have you attended any of the trainings ?	1	Yes	→ M54				m52		rephrased										
m52 <- Ramthal_Karnataka_Cleaned %>%select(id,m52 ) %>% left_join(ramtal_groups)
m52 %>% group_by(in1_out0,m52) %>% summarise(n=n())









rmtl_srvy22 %>% select(mm5,mm9,mm10) %>% 
  filter(mm9>-1,!is.na(mm5)) %>% 
  count(mm5,mm9) %>% 
  group_by(mm5) %>%mutate(pct=n/sum(n)) %>% 
  group_by(mm9) %>% 
  mutate(sum_pct=sum(pct)) %>% mutate(percent=pct/sum_pct) %>% 
  
  
  
  
  
  
  
  
  
  
  
  ## ADDING BASELINE DATA  Caste & income ploting----
library(haven)
baseline_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

# caste_incom_water ----
# A18 What is your caste?
# A19 Which caste category does this fall under?
# A13 According to what you indicated, your total HH income is Rs. [     ].
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)

c_i=
  rmtl_baseline2016 %>% select(hh_id,A22, #caste
                               A23, #caste category
                               F13,# total HH income
                               B9)# In the last 5 years, has the household received any assistance

B9=
  c_i %>%
  rename(id_srvy=Id, income_2016= F13) %>% 
  mutate(id_srvy=as.character(id_srvy))

caste_incom=
  c_i %>%
  filter(A23 !="4" | F13 < 500000) %>%
  mutate(caste_cat = ifelse(A23=="1","GC",
                            ifelse(A23=="2","Other BC",
                                   ifelse(A23== c("3","4"),"SC/ST","")))
  ) %>% 
  rename(id_srvy=Id, income_2016= F13) %>% 
  mutate(id_srvy=as.character(id_srvy))

income2022 = a_rmtl_srvy22 %>%
  mutate(id_srvy=as.character(id)) %>% 
  select(id_srvy, f13,f6,f6_amt ) %>% filter(f13>0) %>% 
  rename(income_2022=f13,salaried_job=f6)

caste_incom_water <- 
  a_water_usage_key_var %>% rename(id_srvy=id) %>% 
  mutate(id_srvy=as.character(id_srvy)) %>% 
  left_join(caste_incom) %>% 
  left_join(income2022) 

# income_2016

income_test= caste_incom_water %>% mutate(mm5=as.character(mm5),mm4=as.character(mm4))

income_test$mm4 [income_test$mm4 == 1] <- "1.IS" #  Irrigation System installed
income_test$mm4 [income_test$mm4 == 0] <- "2.NIS" # No Irrigation System

income_test$mm5 [income_test$mm5 == 1] <- "1a.UW" # use water
income_test$mm5 [income_test$mm5 == 0] <- "1b.NUW" # no use water
income_test$mm5 [is.na(income_test$mm5)] <- "2. NIS" # No Irrigation System

income_test$mm5 [income_test$mm5 == 1] <- "UW" # use water
income_test$mm5 [income_test$mm5 == 0] <- "NUW" # no use water
income_test$mm5 [is.na(income_test$mm5)] <- "NIS" # No Irrigation System

mm4=
  income_test  %>%
  group_by(mm4) %>%
  get_summary_stats(income_2016, type = "mean_sd")
nice_table(mm4)

# T_TEST
library(rstatix)
tmm4 <- 
  income_test %>% 
  t_test(income_2016 ~ mm4, detailed = F) %>%
  add_significance()
nice_table(tmm4[c(1,6:8)])

mm5=
  income_test  %>%
  group_by(mm5) %>%
  get_summary_stats(income_2016, type = "mean_sd") %>% 
  mutate_at(4:5,round) # (column ,round ,digits)
nice_table(mm5)


library(jtools)
library(huxtable)
lmm5 <- lm(income_2016 ~ mm5, data = income_test)
summary(lmm5)
summ(lmm5)

plot_summs(lmm5, inner_ci_level = .9)
plot_summs(lmm5, plot.distributions = TRUE, inner_ci_level = .9)

library(sjPlot)
tab_model(lmm5, show.se = TRUE)
tab_model(lmm5, collapse.ci = TRUE, show.se = TRUE)

#salaried_job
caste_incom_water %>%
  count(mm5, salaried_job) %>% mutate(nu=n*.0108991825613079)

# m62a #Are you also benefiting and increasing your income ?
a_rmtl_srvy22 %>% count (mm5,m62a) %>% mutate(nu=n*.0108991825613079)

# F9	Government pension or scheme
a_rmtl_srvy22 %>% count (mm5,f9) %>% mutate(nu=n*.0108991825613079)


names(caste_incom_water)
# bar plot Water usage by caste ----
caste_water <- 
  caste_incom_water %>%
  filter(mm5>=0) %>% 
  filter(caste_cat>0) %>% 
  count(caste_cat, water_usage) %>% 
  group_by(caste_cat) %>% 
  mutate(pct=n/sum(n)) %>%
  mutate_at(4,round,2) %>% 
  mutate(nu=n*.0108991825613079)



caste_incom_water <- 
  a_water_usage_key_var %>% 
  left_join(caste_incom) %>% 
  left_join(income2022) %>% 
  filter(mm5>=0)

caste_water %>% 
  ggplot(aes(y = pct, fill = water_usage, x = caste_cat)) +
  geom_bar(stat="identity")+
  
  scale_fill_manual(values=c(  "#E69F00","skyblue")) +
  labs(title = "Water usage by caste",
       caption = "n=633 \nGC=General Category \nBC=Backward Caste \nSC/ST=Scheduled Caste/Tribe",
       subtitle = "Use/not use water",
       x ="", y = "% of farmers")+
  theme_light()+
  theme(strip.text.x = element_text(size = 8),legend.position="right" ,legend.title = element_blank())

# plot Water usage by income ----

#income 2016 750
caste_incom_water %>% 
  mutate(Income2016=income_2016/1000) %>% 
  mutate_at(4,round) %>% 

  ggplot( aes(x=income_2016 , y=water_usage,color=water_usage)) + 
  geom_point()+
  scale_color_manual(values=c( "#E69F00","skyblue"))+
  labs(title = "Use/not use water by income",caption = "n=734",x ="", y = "Annual Income 2016 (in Rs.)")+
  theme_light()+
  theme(strip.text.x = element_text(size = 8),legend.position = "none",legend.title = element_blank())+
  coord_cartesian(xlim = c(NA, 250000))
  scale_x_continuous(
    breaks = c(5000,100000,150000, 200000) ,
    labels = c("5K" , "100K", "150K", "200K")
  )

caste_incom_water %>% 
  group_by(water_usage) %>% 
  summarise(n=n(),mean_income_2016=mean(income_2016,na.rm = T)) 

#income 2022
caste_incom_water %>% 
  mutate(Income2022=income_2022/1000) %>% 
  mutate_at(4,round) %>% 
  
  ggplot( aes(x=income_2022 , y=water_usage,color=water_usage)) + 
  geom_point()+
  scale_color_manual(values=c( "#E69F00","skyblue"))+
  labs(title = "Use/not use water by income",caption = "n=734",
       x ="", y = "Annual Income 2022 (in Rs.)")+
  theme_light()+
  theme(strip.text.x = element_text(size = 8),legend.position = "none",
        legend.title = element_blank())+
  coord_cartesian(xlim = c(NA, 500000))+
scale_x_continuous(
  breaks = c(5000,100000,250000,500000) ,
  labels = c("5K" , "100K", "250K","500K"))

caste_incom_water %>% 
  group_by(water_usage) %>% 
  summarise(n=n(),mean_income_2016=mean(income_2016,na.rm = T))


# wua_mm m59a Water User Associations (WUA) ----
a_water_usage_key_var %>%  
  mutate(wua_mm= ifelse( m59a == 2,"mm","no mm" )) %>%  
  left_join(caste_incom_water) %>% 
  count(water_usage,wua_mm) %>% 
  group_by(water_usage) %>% 
  mutate(pct=n/sum(n))#  %>% mutate(nu=n*.0108991825613079)



#----
#----
  # usefull code ----
# remove columns that contain only NAs
x %>% 
  select(-which(sapply(., function(x) all(is.na(x)))))
# remove columns that contain only empty cells
x %>%
  mutate_all(as.character) %>%  # Convert all columns to character type
  select_if(~ !all(is.null(.)) & !all(. == ""))

# adding character "plot_" to digit
df$PLOT <- ifelse(grepl("^plot_\\d$", df$PLOT), sub("^plot_(\\d)$", "plot_0\\1", df$PLOT), df$PLOT)

# Using sub to replace the digits before the last underscore
# with the digits after the last underscore
names(L48_prev) <- sub("(\\d+)_(\\d)$", "\\2_\\1", names(L48_prev))


# %%%%%
count(in1_out0,irri.rain) %>%
  group_by(in1_out0) %>%  
  mutate(prc=n/sum(n)) %>%
  arrange(desc(in1_out0)) %>% 
  kable() %>%  kable_minimal()



scale_fill_manual(values=c("#69b3a2", "grey")) +
  
# crosstab ----
crosstab= 
  Ramthal_Karnataka_Cleaned [,c("id","in1_out0","mm2_1")] %>%
  mutate(in_out=ifelse(in1_out0=="Inside","IN","OUT"),
         self_raport_2022=ifelse(mm2_1=="Im_inside","IN","OUT")) %>% 
  mutate(in_out=case_when(is.na(in_out) ~ self_raport_2022,TRUE ~ in_out)) # NAs

library(sjPlot)
sjPlot::tab_xtab(var.row = crosstab$in_out , 
                 var.col = crosstab$self_raport_2022, 
                 title = "Farmers' houshold. In & Out the project Baseline list | Survey2022 self-report",
                 var.labels = c("Baseline list", "_______Self-report_______"),
                 show.row.prc = TRUE,
                 digits = F,
                 #                 show.exp = TRUE,
                 tdcol.n = "black",
                 #                tdcol.expected = "gray",
                 tdcol.row = "#339933",
                 emph.total = T,
                 emph.color = "#f8f8f8",
                 show.legend = T,
                 show.summary = T)
rm(crosstab)


#

  ggplot( aes(x=mm9 , y=water_usage ,color=water_usage)) + 
  geom_point()+
  scale_x_continuous(breaks = c(3,4,5,6,8,9 ,10,18,20))


