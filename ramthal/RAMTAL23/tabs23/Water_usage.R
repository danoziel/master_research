library(dplyr)
library(tidyverse)
library(haven)
library(hrbrthemes)
theme_ipsum()+



# Irrigation source "irriRain" ( from L 'cultivation') ----
#   L7 rank irrigation source
# What irrigation source are you dependent on? (Rank according to the degree of importance)
# 
# value    label
# 2        Tank/ farm pond
# 3        Open well
# 4        Borewell
# 5        Government water supply source (other than canal)
# 6        Rainfed
# 7        canal
# -888	   Other (specify)

L7 <- 
  a_rmtl_srvy22 %>%select(hh_id,starts_with("l7_"))%>%  
  mutate(
    l7_rank_1 = as.numeric (l7_rank_1),
    l7_rank_2 = as.numeric(l7_rank_2),
    l7_rank_3 = as.numeric(l7_rank_3)
  ) %>%
  mutate(l7_rank_1=ifelse(l7_rank_1=="-888", 7 ,l7_rank_1),
         l7_rank_2=ifelse(l7_rank_2=="-888",7,l7_rank_2),
         l7_rank_3=ifelse(l7_rank_3=="-888",7,l7_rank_3)
  ) %>% 
  select(-l7_other)


L7_irriRain <- L7 %>%
  mutate(irri.rain=ifelse(
    l7_rank_1 != 6|l7_rank_2 !=6|l7_rank_3 != 6, "irrigation" ,"rainfed")) %>% 
  mutate(irri.rain=
           ifelse(l7_rank_1 == 6 & is.na(l7_rank_2) & is.na(l7_rank_3),"rainfed", irri.rain )
         ) %>% 
  mutate(drip=ifelse(l7_rank_1 == 5|l7_rank_2 ==5|l7_rank_3 == 5, 1,NA)) 
rm (L7)  

irriRain=
  L7_irriRain %>% left_join(list_groups_rmtl) %>% 
  select(id,in1_out0,sample_mm2_1,irri.rain, drip )
rm (L7_irriRain)  


### WATER USAGE ### ====
# a_water_usage_key_var---- 
  
# KEY VARS variables for 'water usage' database

key_var = a_rmtl_srvy22 %>%
  select(id,
         mm4,mm5,mw2,mm9,mm10, 
         mw1a,mw1b,mw4,mw4b,
         mw12,mw13,mw14,
         m59a,#Are you aware of the existence of WUA?
         m62b,#Are you also benefiting and increasing your income ?
         m20_kharif_2022,m20_rabi_2022,m52)

a_water_usage_key_var= 
  key_var %>% 
  mutate(id_srvy=as.character( id)) %>% 
  select(-id) %>% select(id_srvy,everything()) %>% 
  rename(id_srvy=id,layer_elevation=layer) %>%
  full_join(irriRain) %>% 
  left_join(list_groups_rmtl) %>% 
  mutate(water_usage=ifelse(mm5==1,"use water","didnt use water"))

rm(irriRain,key_var)

# water_mid22 # mm4 mm5 mw2 mw1a mw1b mw4 mw4b mw12 mw13 mw14 m20_ m52 mm9 mm10 ----
water_mid22 = Ramthal_Karnataka_Cleaned %>% select (id,in1_out0,starts_with("m"))

# mm4 # infrastructure installed 
water_mid22 %>% group_by(in1_out0,mm4) %>% summarise(n=n()) %>% mutate(sum(n))

# mm4 mm5
water_mid22 %>% group_by(in1_out0,mm4,mm5) %>% summarise(n=n()) %>% mutate(sum(n))
water_mid22 %>% group_by(in1_out0,mm5) %>% summarise(n=n()) %>% mutate(sum(n))
#    after visit
water_mid22 %>%full_join(ramtal_groups) %>% 
  group_by(sample_mm2_1,mm4,mm5) %>% summarise(n=n()) %>% mutate(sum(n))

    
#,mm4,mm5,mw2 Have you used it in [ _____ ]? Kharif Rabi Both
a_water_usage_key_var %>%group_by(in1_out0,mm4,mm5,mw2) %>% summarise(n=n()) 

 #mm4,mm5,mw1a If Yes, in which year did you first make use of the water?
water_mid22 %>% group_by(in1_out0,mm4,mm5,mw1a) %>% summarise(n=n())

#mm4,mm5,mw1b,#In what season did you use it in that year?
water_mid22 %>% group_by(in_out,mm4,mm5,mw1b) %>% summarise(n=n())

#mm4,mm5,mw4, # Are you still making use of the water from the project to irrigate your land?
a_water_usage_key_var %>% group_by(mm5,mw4) %>% summarise(n=n())

#AFTER VISIT
water_mid22 %>% 
  select(id,mm4,mm5,mw4) %>% 
  full_join(rmtl_groups) %>% 
  group_by(in1_out0,mm4,mm5,mw4) %>% 
  summarise(n=n())


#mm4,mm5 mw4b,#If No or Sometimes- What was the last year you use of the water?
a_water_usage_key_var %>% group_by(in_out,mm4,mm5,mw4b) %>% summarise(n=n())
a_water_usage_key_var %>% group_by(mm4,mm5,mw4b) %>% summarise(n=n()) %>% 
  filter(!is.na(mw4b)) %>% mutate(n/sum(n))

#mm4,mm5  mw12,# Typically, in your experience, when water is provided in a particular year, in which month does it start?
water_mid22 %>% group_by(in_out,mm4,mm5,mw12) %>% summarise(n=n())

#mm4,mm5mw13,# Typically, in your experience, when water is provided in a particular year, in which month does it end for the year?
water_mid22 %>% group_by(in_out,mm4,mm5,mw13) %>% summarise(n=n())

#mm4,mm5 mw14) %>% # Typically, in your experience, during the period water is provided, how often is it provided?
water_mid22 %>% group_by(in_out,mm4,mm5,mw14) %>% summarise(n=n())

#mm4,mm5 m20_ 2022-2017 did you ever make use kharif_2022
water_mid22 %>% group_by(in1_out0,mm4,mm5,m20_kharif_2022) %>% summarise(n=n())
water_mid22 %>% group_by(in_out,mm4,mm5,m20_rabi_2022) %>% summarise(n=n())

# i34	m52		Have you attended any of the trainings ?	1	Yes	→ M54				m52		rephrased										
m52 <- Ramthal_Karnataka_Cleaned %>%select(id,m52 ) %>% left_join(ramtal_groups)
m52 %>% group_by(in1_out0,m52) %>% summarise(n=n())

#mm9, How many farmers are there between you and the valve/pipeline?
#mm10 Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?
water_mid22 %>% full_join(rmtl_groups) %>%  filter(mm9>0) %>% group_by(in1_out0,mm4,mm5) %>% summarise(mean(mm9),n=n())
water_mid22 %>% full_join(rmtl_groups) %>%  filter(mm9>0) %>% group_by(sample_mm2_2,mm4,mm5) %>% summarise(mean(mm9),n=n())
 
#mm10 Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?
water_mid22 %>% full_join(rmtl_groups) %>% group_by(in1_out0,mm4,mm5) %>% count (mm10)
water_mid22 %>% full_join(rmtl_groups) %>% group_by(sample_mm2_2,mm4,mm5) %>% count (mm10)

# Locarion on pipe    frmrs_b4u -----

water_mid22 = Ramthal_Karnataka_Cleaned %>% select (id,in1_out0,starts_with("m"))

frmrs_b4u=
  a_water_usage_key_var %>%
  select(id,in1_out0,sample_mm2_1,mm4,mm5,mm9,mm10) %>% 
  mutate(mm10=as.numeric(mm10)) %>% 
  filter(mm4==1)  # n mm4 = n mm5

# mm9 rm -999 + mutate mm5
frmrs_b4u_mm9 <- 
  frmrs_b4u %>% 
  filter(mm9>=0) %>% 
  mutate(water_usage=ifelse(mm5==1,"use water","didnt use water"))


## distribution: X=mm9 No. of frmrs b4u ====
# water_usage Y/N  ----
library(ggplot2)
library(dplyr)
library(hrbrthemes)

mm9_in_50= frmrs_b4u_mm9 %>% filter(in1_out0==1)
mm9_in_20= frmrs_b4u_mm9 %>% filter(in1_out0==1,mm9<=20)
mm9_ii_50= frmrs_b4u_mm9
mm9_ii_20= frmrs_b4u_mm9 %>% filter(mm9<=20) 

mm9_in_50 %>% count(water_usage,mm9) %>% group_by(water_usage) %>%mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=mm9, y=pct, fill=water_usage)) +
  geom_bar(stat="identity", position=position_dodge())+

mm9_in_20 %>% count(water_usage,mm9) %>% 
  group_by(water_usage) %>%mutate(pct=n/sum(n)) %>% 
  group_by(mm9) %>% 
  mutate(sum_pct=sum(pct)) %>% mutate(percent=pct/sum_pct) %>% 
  
  ggplot(aes(x=mm9, y=percent, fill=water_usage)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c( "#E69F00","skyblue")) +
  labs(title = "Usage water / Location on the pipeline",x ="No. farmers before you", y = "% of farmers")+
# labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "Sample:Ramthal farmers 2016",  caption = "n=538")  #Ramthal

# scatter plot: location/pct====
mm9_ii_50 %>%
  count(water_usage,mm9) %>% group_by(water_usage) %>% mutate(pct=n/sum(n)) %>%
  ggplot( aes(x=mm9 , y=pct,color=water_usage)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c( "#E69F00","skyblue"))+
  labs(title = "Usage water / Location on the pipeline",x ="No. farmers before you", y = "% of farmers")+
  labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "Sample:Ramthal farmers 2016",  caption = "n=538")+  #Ramthal
  theme_ipsum()+
  theme(strip.text.x = element_text(size = 8),legend.position = c(0.85,0.9),legend.title = element_blank())

# mean table 
frmrs_b4u_mm9 %>% 
  group_by(mm5) %>% 
  summarise(n=n(),farmers_before_you=mean(mm9)) %>% 
  kable() %>% kable_minimal()

tapply(frmrs_b4u_mm9$mm9,frmrs_b4u_mm9$mm5, var)


# mm10  1-5  ----

library(viridis)
library(ggridges)
# only water_usage==1 on this sample
frmrs_b4u_mm10=
  frmrs_b4u[,c(1,2,5,7,6)] %>% 
  filter(mm9>=0) %>% # rm -999
  drop_na() %>%   # rm NAs mm10
  mutate(water_usage=ifelse(mm5==1,"use water","didnt use water")) 

frmrs_b4u_mm10$mm10[frmrs_b4u_mm10$mm10  == 1] <- "1 never happened"
frmrs_b4u_mm10$mm10[frmrs_b4u_mm10$mm10  == 2] <- "2 once a season"
frmrs_b4u_mm10$mm10[frmrs_b4u_mm10$mm10  == 3] <- "3 several times a season"
# Replacing 4 with 5 from the survey, to create a logical value scale
frmrs_b4u_mm10$mm10[frmrs_b4u_mm10$mm10  == 4] <- "5 constant basis"
frmrs_b4u_mm10$mm10[frmrs_b4u_mm10$mm10  == 5] <- "4 Some seasons"

# mm10 plot for each answer ----
frmrs_b4u_mm10 %>% 
  filter(in1_out0==1)%>%  # Ramthal grp N=230
#  count(mm10) %>% 
  count(mm10,mm9) %>%group_by(mm10) %>%    mutate(pct=n/sum(n)) %>% 

  ggplot(aes(x=mm9, y=pct)) +
  geom_bar(stat="identity", position=position_dodge(), fill="skyblue",)+

  facet_wrap(~ mm10, scales = "free") +
  labs(title = "Usage water / Location on the pipeline",x ="No. farmers before you", y = "% of farmers")+
#  labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "Sample:Ramthal farmers 2016",  caption = "n=538")+  #Ramthal
  theme_ipsum()+   theme(strip.text.x = element_text(size = 8),legend.position = "none")+
#  coord_cartesian(ylim = c(NA, 0.31),xlim = c(NA, 50))   # x & y-axis limit height
  coord_cartesian(ylim = c(NA, 0.31),xlim = c(NA, 20))   # x & y-axis limit height

frmrs_b4u_mm10 %>% 
  group_by(mm10) %>% 
  summarise(n(),farmers_before_you=mean(mm9),sd(mm9)) %>% 
  kable() %>% kable_minimal()

tapply(frmrs_b4u_mm10$mm9,frmrs_b4u_mm10$mm10, var)

# mm10 ONE plot for ALL answers ----

frmrs_b4u_mm10 %>% 
  filter(in1_out0==1)%>%  # Ramthal grp N=230
#  filter(mm9<=20) %>% 
  count(mm10,mm9) %>%group_by(mm9) %>%    mutate(pct=n/sum(n)) %>% 
  ggplot() + geom_bar(aes(y = pct, x = mm9, fill = mm10),stat="identity")+
  scale_fill_manual(
    values=c("deepskyblue4","deepskyblue3","skyblue2","skyblue3","skyblue1"))+
  theme_ipsum()+
  labs(title = "Usage water / mmm10",x ="No. farmers before you", y = "% of farmers")+
  #  labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "Sample:Ramthal farmers 2016",  caption = "n=538")+  #Ramthal
  theme(strip.text.x = element_text(size = 8),legend.title = element_blank())
  geom_text(aes(label = paste0(round(pct * 100), "%"), y = pct, x = mm9),
            size = 2,position = position_stack(vjust = 0.5))

frmrs_b4u_mm10 %>% 
  filter(in1_out0==1)%>%  # Ramthal grp N=230
  filter(mm9<=20, !mm10 %in% c("4 Some seasons")) %>% 
  mutate(mm10=ifelse(mm10=="5 constant basis","3 several times a season",mm10)) %>% 
  count(mm10,mm9) %>%group_by(mm9) %>%    mutate(pct=n/sum(n)) %>% 
  ggplot() + geom_bar(aes(y = pct, x = mm9, fill = mm10),stat="identity")+
  scale_fill_manual(
    values=c("deepskyblue4","deepskyblue3","skyblue2"))+
  theme_ipsum()+
  labs(title = "Usage water / mmm10",x ="No. farmers before you", y = "% of farmers")+
  #  labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "Sample:Ramthal farmers 2016",  caption = "n=538")+  #Ramthal
  theme(strip.text.x = element_text(size = 8),legend.title = element_blank())
  geom_text(aes(label = paste0(round(pct * 100), "%"), y = pct, x = mm9),
            size = 2,position = position_stack(vjust = 0.5))

# frmrs_b4u + irriRain = b4u_irriRain ----

b4u_irriRain=
  water_mid22 %>% full_join(rmtl_groups) %>%
  select(id,in1_out0,sample_mm2_2,mm4,mm5,mm9,mm10) %>%

  full_join(irriRain) %>% 
  select(id,in1_out0,mm5,mm9,mm10,irri.rain,drip,a5)

b4u_irriRain %>% group_by(in1_out0,mm5,irri.rain) %>% count()

# Location on the pipeline of irrigation/rainfed farmers ----

b4u_irriRain  %>% 
  filter(mm9>=0) %>% # rm -999
  filter(in1_out0==1) %>% # n= 538 
  filter(mm9<=20)%>%  # Outliers removed
  count(irri.rain,mm9) %>% group_by(irri.rain) %>%mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=mm9, y=pct, fill=irri.rain)) +
  geom_bar(stat="identity", position=position_dodge())+
  
  scale_fill_manual(values=c( "skyblue3","skyblue")) +
  labs(title = "Location on the pipeline Vs.irrigation/rainfed",
       #subtitle = "Sample:Infrastructure installed",
       caption = "n=538", #Ramthal
       #caption = "n=667", #Infrastructure
       subtitle = "Sample:Ramthal farmers 2016",
       x ="No. farmers before you", y = "% of farmers")+
  theme_ipsum()+
  theme(strip.text.x = element_text(size = 8),legend.position = c(0.85,0.9),legend.title = element_blank())

# mm5 use water Vs. irrigation/rainfed farmers 
library(sjPlot)
sjPlot::tab_xtab(var.row = b4u_irriRain$mm5  , var.col =b4u_irriRain$irri.rain  , 
                 title = "use water Vs. irrigation/rainfed",var.labels = c("Use water", " "),
                 show.row.prc = TRUE,digits = F,tdcol.n = "black",tdcol.row = "#339933",
                 emph.total = T,emph.color = "#f8f8f8",show.legend = T,show.summary = T)
# ramthal only
b4u_irriRain_in <- subset(b4u_irriRain,in1_out0 == 1) 
sjPlot::tab_xtab(var.row = b4u_irriRain_in$mm5  , var.col =b4u_irriRain_in$irri.rain  , 
                 title = "in out Vs. irrigation/rainfed",var.labels = c("Use water", " "),
                 show.row.prc = TRUE,digits = F,tdcol.n = "black",tdcol.row = "#339933",
                 emph.total = T,emph.color = "#f8f8f8",show.legend = T,show.summary = T)



# July 23 VISIT farmers_interviews_july23----

library(readxl)
farmers_july23 <- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/field visit july 2023/List of farmers for interviews.xlsx", 
             sheet = "interviws")

names(farmers_july23)

farmers_july23 <- 
  farmers_july23 %>% 
  filter(!UID == "out of list") %>% 
  mutate(id = as.numeric (UID)) %>% 
  left_join(frmrs_b4u) 

farmers_july23$mm10[farmers_july23$mm10  == 1] <- "1 never happened"
farmers_july23$mm10[farmers_july23$mm10  == 2] <- "2 once a season"
farmers_july23$mm10[farmers_july23$mm10  == 3] <- "3 several times a season"
# Replacing 4 with 5 from the survey, to create a logical value scale
farmers_july23$mm10[farmers_july23$mm10  == 4] <- "5 constant basis"
farmers_july23$mm10[farmers_july23$mm10  == 5] <- "4 Some seasons"
farmers_july23_01 <- farmers_july23%>% select(id, infrastructureYN,mm10,mm9,useYN,mm5)
  

add0 <- data.frame( water_usage= c("use","never_use","use","never_use","never_use"),
            mm9= c(3,4,9,10,18),
            n= c(0,0,0,0,0))

farmers_july23_01 %>%
  filter(mm9>0) %>% 
  mutate(water_usage= ifelse(mm5==1, "use","never_use")) %>% 
  count(water_usage ,mm9) %>% 
  bind_rows(add0) %>% 
  
  # mutate(pct=n/sum(n)) %>% 
   ggplot(aes(x=mm9, y=n, fill=water_usage)) +
   geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c( "#E69F00","skyblue")) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9,10,18,20))+
  theme_linedraw()+
  labs(title = "Usage water / Location on the pipeline",x ="No. farmers before you", y = "Total No. of farmers")+
  # labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "July 2023 visit sample:",  caption = "n=18")  #Ramthal

  
  
  
  

## ADDING BASELINE DATA  Caste & income ploting----
library(haven)
baseline_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

# caste_incom_water ----
# A18 What is your caste?
# A19 Which caste category does this fall under?
# A13 According to what you indicated, your total HH income is Rs. [     ].
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)

c_i=
  baseline_2016 %>% select(Id,A22, #caste
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
  mutate(caste_cat=ifelse(A23=="1","GC",
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


