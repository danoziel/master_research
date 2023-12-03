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

# Study 2
#a_sample_irri
a_sample_irri= a_sample[,c("HH_id","mm4","mm5","in1_out0","survey")]
a_sample_irri$irrigation_water3 <- 
  ifelse(is.na(a_sample_irri$mm5),"no_irrigation_system", 
         ifelse(a_sample_irri$mm5==1,"yes_irrigation_water","no_irrigation_water"))
a_sample_irri$irrigation_water2 <- ifelse(a_sample_irri$mm5==1,"yes_irrigation_water","no_irrigation_water")
a_sample_irri$irrigation_system <- ifelse(a_sample_irri$mm4==1,"yes_irrigation_system","no_irrigation_system")

#### d D CULTIVATION ####

# D2 total_acres COR D3 total_plots ----
d <- baseline_RMTL%>% 
  select(hh_id,D2,D2_acer,D2_guntas,D3) %>% 
  rename( total_acres=D2 , total_plots=D3) %>% 
  right_join(a_sample)

ggplot(d, aes(x=total_acres, y=total_plots)) +
  geom_point(shape=18, color="green4")+
  geom_smooth(method=lm, se=FALSE, color="brown4")+ theme_minimal()

d2d3 <- lm(total_acres ~ total_plots, d)
summary(d2d3)

library(sjPlot)
tab_model(d2d3, show.se = TRUE)

# D2	Total Area  (acres) ----
# Study 1----
d_1=d %>% 
  mutate(HH_project= ifelse(in1_out0==1,"In","Out")) %>% 
  mutate(HH_south_north= ifelse(south1_north0==1,"south","north"))


# total_acres | total_plots summary_stats t.test ====
library(rstatix)
stats_acres= d_1 %>% group_by(HH_project) %>% get_summary_stats(total_acres, type = "mean_sd")

test_acres <- d_1 %>% t_test(total_acres ~ HH_project, detailed = F) %>% add_significance()

library(rempsyc)
nice_table( stats_acres)
nice_table(test_acres[c(6:9)])


stats_num=d_1 %>% group_by(HH_project) %>% get_summary_stats(total_plots, type = "mean_sd")
test_acres=d_1 %>% t_test(total_plots ~ HH_project, detailed = F) %>% add_significance()

nice_table( stats_num)
nice_table(stats_num[c(6:9)])


# Study 2 ----
d %>% group_by(irrigation_water3) %>% 
  summarise(total_acres=mean(total_acres,na.rm = T),total_plots=mean(total_plots,na.rm = T))

# t = 1.7642, df = 617.85, p-value = 0.0782 # Signif. 0.1
cult11= 
  d%>% select(HH_id,irrigation_water2,total_acres ) %>% 
  pivot_wider(names_from = irrigation_water2, values_from = total_acres)
t.test(cult11$yes_irrigation_water, cult11$no_irrigation_water) 


# t = 2.3844, df = 1441, p-value = 0.01724 #Signif. 0.05
cult12=
  d %>% select(HH_id,irrigation_system, total_acres ) %>% 
  pivot_wider(names_from = irrigation_system, values_from = total_acres)
t.test(cult12$yes_irrigation_system, cult12$no_irrigation_system)

# D6	Area per Plot (acres) ----

# study 1 ----
d6= baseline_RMTL[,c(1,grep("^D6_",names(baseline_RMTL) ))] %>% 
  right_join(a_sample)






# study 2 ----
d6= baseline_RMTL[,c(1,grep("^D6_",names(baseline_RMTL) ))] %>% 
  right_join(a_sample_irri)

base_plot <- d6 %>% select(HH_id,D6_1:D6_10) %>% 
  pivot_longer(-HH_id, names_to = "plot_num", values_to = "plot_acre")
base_plot$plot_num <- str_replace(base_plot$plot_num, "D6_(\\d)$", "plot_0\\1")
base_plot$plot_num <- str_replace(base_plot$plot_num, "^D6_", "plot_")
base_plot_acre <- filter(base_plot,!is.na(plot_acre))



# D12	irrigation before 2016 ----
# D12	Has this plot been irrigated at least once during the last 5 years?
# study 1 ----
d12 = baseline_RMTL [,c(1,47,grep("^D12",names(baseline_RMTL) ))] %>% 
  right_join(a_sample)%>% 
  mutate(HH_project= ifelse(in1_out0==1,"In","Out")) %>% 
  mutate(HH_south_north= ifelse(south1_north0==1,"south","north"))

# HH's who irrigate from all HH (in %)
d12_prc <- d12%>% 
  mutate(irriPlot = rowSums (.[names(.)[3:19]], na.rm = T)) %>% 
  mutate(pec_irriPlot=irriPlot/D3 ) 

# HH's who irrigate from all HH (in %)
d12_prc %>% count(HH_project,D12_) %>%
  pivot_wider(names_from = D12_, values_from = n) %>% 
  mutate(prc=`1`/`0`) %>% mutate(graphPrc=prc/sum(prc))


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





