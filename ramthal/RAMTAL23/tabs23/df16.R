#| THIS R SCRIPT [df16.R] is data-frames/sets/bases of "2016 baseline survey"
#| 🟡BASELINE 2016  | rmtl_baseline2016 #= baseline_RMTL


#| df18.R # scrip of data-frames/sets/bases of "2018 midline survey"
#🟠MIDELINE 2018| rmtl_midline2018 #= mid2018_RMTL

#| DF_22.R # scrip of data-frames/sets/bases of "2022 midline survey"
#🟣MIDELINE 2022| rmtl_srvy22 #= a_rmtl_srvy22 


# vars_02 caste income 2016----
# A18 What is your caste?
# A19 Which caste category does this fall under?
# A13 According to what you indicated, your total HH income is Rs. [     ].
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)


# C5	What is their relationship to the head of household?
# C6	Are they literate?
# C7	What is their educational level?
C5=baseline_RMTL[,c(1,grep("^C5_[1-9]",names(baseline_RMTL)))]  
C5[C5>1] <- NA
C5 %>% select(where(~!all(is.na(.x))))

edu1=baseline_RMTL[,c(1,grep("^C5_[1-9]|^C7_[1-9]",names(baseline_RMTL)))] %>% select(-ends_with("bin"))%>% mutate(edu_head_hh=NA ) 

edu1$edu_head_hh <- ifelse(edu1$C5_1 == 1, edu1$C7_1, edu1$edu_head_hh)
edu1$edu_head_hh <- ifelse(edu1$C5_2 == 1, edu1$C7_2, edu1$edu_head_hh)
edu1$edu_head_hh <- ifelse(edu1$C5_3 == 1, edu1$C7_3, edu1$edu_head_hh)
edu1a <-
  edu1 %>% 
  mutate(edu_head_hh=
           ifelse(C5_1==1,C7_1,ifelse(C5_3 == 1, C7_3,ifelse(C5_4 == 1, C7_4,ifelse(C5_5 == 1,C7_5,ifelse(C5_6 == 1, C7_6,ifelse(C5_7 == 1, C7_7,ifelse(C5_8 == 1, C7_8, ifelse(C5_9 == 1, C7_9,ifelse(C5_10 == 1,C7_10,ifelse(C5_11== 1, C7_11,ifelse(C5_13 == 1, C7_13, ifelse(C5_16 == 1,C7_16,NA)))))))))))))


vars_02=
  baseline_2016 %>% rename(hh_id=Id)%>% 
  select(hh_id,A22, #caste
         A23, #caste category
         F13,# total HH income
         B9) %>% # In the last 5 years, has the household received any assistance
  right_join(a_sample) %>%
  rename(income_2016= F13) 

# 1	General Category |2 Other Backward Caste |3 Scheduled Caste |4 Scheduled Tribe
vars_02 %>%# filter(A23 !="4" | F13 < 500000) %>%
  mutate(caste_cat=ifelse(A23=="1","GC",ifelse(A23 %in% c("2","3","4"),"OBC/SC/ST",""))
  ) %>% group_by(in1_out0,caste_cat) %>% count() %>% 
  group_by(in1_out0) %>% mutate(prc=n/sum(n))


# income_2016
library(rstatix)
ic02=
  vars_02 %>% group_by(HH_project) %>%
  get_summary_stats(income_2016, type = "mean_sd") %>% 
  mutate_at(4:5,round) # (column ,round ,digits)

# T_TEST
test02 <- vars_02 %>% t_test(income_2016 ~ HH_project, detailed = F) %>% add_significance()

library(rempsyc)
nice_table(test02[c(6:9)])



# OLS linear regression ----
library(jtools)
library(huxtable)
lmm5 <- lm(income_2016 ~ HH_project, data = vars_02)
summary(lmm5)
summ(lmm5)

# plot for OLS linear regression
plot_summs(lmm5, inner_ci_level = .9)
plot_summs(lmm5, plot.distributions = TRUE, inner_ci_level = .9)

# table for OLS linear regression
library(sjPlot)
tab_model(lmm5, show.se = TRUE)
tab_model(lmm5, collapse.ci = TRUE, show.se = TRUE)




library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
baseline_2016 <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

#### baseline_RMTL 2016  baseline_RMTL####
baseline_RMTL=
  baseline_2016 %>% 
  rename(hh_id=Id)


#### LAND d D CULTIVATION ####

# irrigation method [bl16_irri_methods]  ----

# D21	What is the method of irrigation?
# 1	Flood
# 2	Furrows
# 3	Drip
# 4	Sprinkler
# 5	Manual
# 6	Hose
# -888	Other, specify

D21 <- rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id, starts_with("D21"), -c("D21_12","D21_0","D21_os_0" ))%>% mutate(irri_method_num=D21_1 )
D21$irri_method_num[D21$D21_os_1== "BOREWEL"] <- 2

D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_2,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_3,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_4,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_5,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_6,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_7,irri_method_num ))
D21=D21 %>% mutate(irri_method_num=ifelse( is.na(irri_method_num),D21_10,irri_method_num ))
 # all "drip" record in "irri_method_num" 

D21_1 <- D21 %>% mutate( irri_method_num =ifelse( irri_method_num %in% c(1:5 ),irri_method_num,0) ) %>% 
  mutate(irri_method =irri_method_num)

D21_1$irri_method[D21_1$irri_method==1] <- "flood"
D21_1$irri_method[D21_1$irri_method==2] <- "furrows"
D21_1$irri_method[D21_1$irri_method==3] <- "drip"
D21_1$irri_method[D21_1$irri_method==4] <- "sprinkler"
D21_1$irri_method[D21_1$irri_method %in% c(5,6) ] <- "hose"
D21_1$irri_method[D21_1$irri_method==0] <- "rain"

bl16_irri_methods <- D21_1 %>% select( "farmers_hh","hh_id", "irri_method_num", "irri_method" )


# total_acres  total_plots -  ---
# D4	Survey/hissa number
D4_land <- baseline_RMTL %>% select(hh_id, starts_with("D4_"))

# D5	Village in which survey plot is located
D5_land <- baseline_RMTL %>% select(hh_id, starts_with("D5_"))

# D6	Area of Plot (acres/gunta)
D6_land <- baseline_RMTL %>% select(hh_id, starts_with("D6_"))

names(D4_land)

land_bl <- baseline_RMTL%>% 
  select(hh_id,D2,D2_acer,D2_guntas,D3) %>% 
  rename( total_acres=D2 , total_plots=D3) %>% 
  right_join(a_sample[,1:2])

# D2 total_acres COR D3 total_plots ----
ggplot(d, aes(x=total_acres, y=total_plots)) +
  geom_point(shape=18, color="green4")+
  geom_smooth(method=lm, se=FALSE, color="brown4")+ theme_minimal()

d2d3 <- lm(total_acres ~ total_plots, d)
summary(d2d3)

library(sjPlot)
tab_model(d2d3, show.se = TRUE)

# D2	Total Area  (acres) ----


# total_acres | total_plots summary_stats t.test ====
library(rstatix)
stats_acres= land_bl %>% group_by(farmers_hh) %>% get_summary_stats(total_acres, type = "mean_sd")

d1 <- land_bl %>% t_test(total_acres  ~ farmers_hh, detailed = T) %>% add_significance()
d2 <- land_bl %>% t_test(total_plots  ~ farmers_hh, detailed = T) %>% add_significance()

table_dd=bind_rows(d1,d2) %>% 
  rename(`Inside \nRamthal`=estimate1,`Outside \nRamthal`=estimate2,t=statistic) %>% 
  select(.y. ,`Inside \nRamthal`,`Outside \nRamthal`,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_dd)





# bl_plot	Area per Plot (acres)  [D12 + D6] ----

bl_d12 = baseline_RMTL [,c(1,grep("^D12",names(baseline_RMTL) ))] %>% 
  select(hh_id,D12_1,D12_2,D12_3,D12_4,D12_5,D12_6,D12_7,D12_8,D12_9,D12_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_plot")

bl_d6 <- baseline_RMTL %>% select(hh_id,D6_1:D6_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "plot_acre")


bl_d6$plot_num <- str_replace(bl_d6$plot_num, "D6_(\\d)$", "plot_0\\1")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "^D6_", "plot_")
bl_d6 <- filter(bl_d6,!is.na(plot_acre))

bl_d12$plot_num <- str_replace(bl_d12$plot_num, "D12_(\\d)$", "plot_0\\1")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "^D12_", "plot_")
bl_d12 <- filter(bl_d12,!is.na(irri_plot))

bl_plot <-full_join(bl_d12,bl_d6) 

# bl_irri_land	Area per HH (acres) [D12 + D6] ----
bl_plot %>% count(irri_plot)

b_t=bl_plot %>% filter(!is.na(plot_acre)) %>% group_by(hh_id) %>% summarise(acre_total=sum(plot_acre)) 

b_i=bl_plot %>% filter(!is.na(plot_acre),irri_plot != 0) %>% group_by(hh_id) %>% summarise(acre_irri=sum(plot_acre)) 

bl_irri_land= b_t %>% left_join(b_i) %>% 
  right_join(a_sample [,1:2])
bl_irri_land$acre_irri[is.na(bl_irri_land$acre_irri)] <- 0
bl_irri_land=bl_irri_land %>% filter(!is.na(acre_total ))

# D12	irrigation before 2016 ----
# D12	Has this plot been irrigated at least once during the last 5 years?

d12_irrigate = baseline_RMTL [,c(1,47,grep("^D12",names(baseline_RMTL) ))] %>% # what is D12_0 ? ? ?
  mutate(total_irriPlot = rowSums (.[names(.)[3:19]], na.rm = T)) %>%
  rename( hh_used_irri= D12_) %>% 
  select(hh_id,hh_used_irri ) %>% 
  right_join(a_sample[,1:2]) %>% count(farmers_hh,hh_used_irri)

# HH's who irrigate from all HH (in %)
d12_prc <- d12_irrigate%>% 
  mutate(irriPlot = rowSums (.[names(.)[3:19]], na.rm = T)) %>% mutate(pec_irriPlot=irriPlot/D3 ) 

# HH's who irrigate from all HH (in %)
d12_prc %>% count(HH_project,D12_) %>%
  pivot_wider(names_from = D12_, values_from = n) %>% mutate(prc=`1`/`0`) %>% mutate(graphPrc=prc/sum(prc))

# 🟪🟥 D21	What is the method of irrigation?	-----
# 1	Flood
# 2	Furrows
# 3	Drip
# 4	Sprinkler
# 5	Manual
# 6	Hose
# -888	Other, specify
bl_d21 = baseline_RMTL [,c(1,grep("^D21",names(baseline_RMTL) ))] 


# study 2 ----

d12 = baseline_RMTL[,c(1,grep("^D12",names(baseline_RMTL) ))] %>% 
  right_join(a_sample_irri)

# base_irri_plot [list]
d12_list= d12 %>% 
  select(HH_id,D12_1:D12_10) %>% 
  pivot_longer(-HH_id, names_to = "plot_num", values_to = "irri_yesno")
library(stringr)
d12_list$plot_num <- str_replace(d12_list$plot_num, "D12_(\\d)$", "plot_0\\1")
d12_list$plot_num <- str_replace(d12_list$plot_num, "^D12_", "plot_")
base_irri_plot <- filter(d12_list,!is.na(irri_yesno))


# HH's who irrigate from all HH (in %)
d12_prc <- d12%>% 
  right_join(a_sample_irri) %>% 
  mutate(irriPlot = rowSums (.[names(.)[3:19]], na.rm = T)) %>% 
  mutate(pec_irriPlot=irriPlot/D3 )

d12_prc %>% count(irrigation_water3,D12_) %>%
  pivot_wider(names_from = D12_, values_from = n) %>% 
  mutate(prc=`1`/`0`) %>% mutate(graphPrc=prc/sum(prc))

# Irrigated plots from all HH's plots (in %)
# mean includes 0s
d12_prc %>% group_by(irrigation_water3) %>% 
  summarise(prc=mean(pec_irriPlot,na.rm = T)) %>% 
  mutate(graphPrc=prc/sum(prc))


#D13	source of irrigation bl_source_irrigate----
#| D13	"What was the principal source of irrigation for this plot over the last 5 years?"
#1 Canal  #2	Tank  #3	Open well  #4	River/Pond/Lake  #5	Bore well  # -888	Other, specify

bl_source = rmtl_baseline2016 %>% select(hh_id,starts_with("D13"))  
bl_source[is.na(bl_source)] <- 0
bl_source[bl_source==-666] <- 10
bl_source=bl_source %>% filter(D13_0 != 10 ,D13_2!= 10,D13_4!= 10)
bl_source[bl_source==-888] <- 5

bl_source_irrigate1 =bl_source %>% 
  mutate(irri_source_bl= ifelse(D13_0>0,D13_0,D13_1)) %>% 
  mutate(irri_source_bl= ifelse(irri_source_bl>0,irri_source_bl,D13_2)) %>% 
  mutate(irri_source_bl= ifelse(irri_source_bl>0,irri_source_bl,D13_3)) %>% 
  mutate(irri_source_bl= ifelse(irri_source_bl>0,irri_source_bl,D13_4)) %>% 
  mutate(irri_source_bl= ifelse(irri_source_bl>0,irri_source_bl,D13_5)) %>% 
  mutate(irri_source_bl= ifelse(irri_source_bl>0,irri_source_bl,D13_6))

hh_bl=rmtl_baseline2016 %>% select(hh_id, farmers_hh)
bl_source_irrigate =
  bl_source_irrigate1 %>% select(hh_id ,irri_source_bl) %>%
  left_join(hh_bl) 

bl_source_irrigate %>% group_by(farmers_hh ,irri_source_bl) %>% 
  count() %>% 
  group_by(farmers_hh) %>% mutate(n/sum(n))




# -----

  
base_plot_INFO=left_join(base_plot_acre ,base_irri_plot)
base_plot_INFO[is.na(base_plot_INFO)] <- 0

a1=
  base_plot_INFO %>% 
  left_join(a_sample) %>% 
  group_by(HH_id) %>% 
  mutate(sum_hh=sum(plot_acre) ) %>% 
  select(HH_id,sum_hh ,irrigation_water3) %>% distinct() %>% 
  group_by(irrigation_water3) %>% 
  summarise(mean_acre=mean(sum_hh) )

a2=
  base_plot_INFO %>% 
  left_join(a_sample_irri) %>% 
  filter(irri_yesno==1) %>% 
  group_by(HH_id) %>% 
  mutate(sum_hh=sum(plot_acre) ) %>% 
  select(HH_id,sum_hh ,irrigation_water3) %>% distinct() %>% 
  group_by(irrigation_water3) %>% 
  summarise(irri_acre=mean(sum_hh) )

a12=full_join(a1,a2)


bs = baseline_RMTL %>%  
  select(south_north_inner, sampledafter200517 ,Srno, SI, survey, si_no, surveyround, south1_north0 ,hh_id, inner_plots, in1_out0, in_out_intersect)


