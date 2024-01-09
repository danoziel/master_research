#| THIS R SCRIPT [df16.R] is data-frames/sets/bases of "2016 baseline survey"
#| 🟡BASELINE 2016  | rmtl_baseline2016 #= baseline_RMTL


#| df18.R # scrip of data-frames/sets/bases of "2018 midline survey"
#🟠MIDELINE 2018| rmtl_midline2018 #= mid2018_RMTL

#| DF_22.R # scrip of data-frames/sets/bases of "2022 midline survey"
#🟣MIDELINE 2022| rmtl_srvy22 #= a_rmtl_srvy22 

library(dplyr)
library(haven)
library(tidyr)
library("stringr") #"str_replace"

####### HH CHARACTERISTICS #######             ----
# vars_02 caste income 2016
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



# OLS linear regression
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



####### CULTIVATION  ############             ####

# plot info            

bl_plot_dt <-
  bl_plot_SrvyHis %>%                 # [D4] Survey/hissa number  : 3,459 × 4
  left_join( bl6_plotAcre ) %>%       # [D6] Area acres/gunta     : 3,455 × 4
  left_join( bl21_irri_methods ) %>%  # [D21] plot irri method 5y : 191 × 4
  left_join( bl13_source_irrigate)    # [D13] source              : 194 × 3

bl_plotSeason_dt <-
  bl_plot_SrvyHis[,1:2] %>% # [,hh_id:plot_num]  3,459
  left_join(bl28_irri_plot_season) %>%  # [D28]	SEASON crop irrigated # A tibble: 6,332 × 5
  

  
  
#       D2 Total Area (acres) ----

# D2	How many acres (guntas) of land does your household currently own?
land_bl <- baseline_RMTL%>% 
  select(hh_id,D2,D2_acer,D2_guntas,D3) %>% 
  rename( total_acres=D2 , total_plots=D3) %>% 
  right_join(a_sample[,1:2])

# D2 total_acres COR D3 total_plots 
ggplot(d, aes(x=total_acres, y=total_plots)) +
  geom_point(shape=18, color="green4")+
  geom_smooth(method=lm, se=FALSE, color="brown4")+ theme_minimal()

d2d3 <- lm(total_acres ~ total_plots, d)
summary(d2d3)

library(sjPlot)
tab_model(d2d3, show.se = TRUE)

# D2	Total Area  (acres) 


# total_acres | total_plots summary_stats t.test
library(rstatix)
stats_acres= land_bl %>% group_by(farmers_hh) %>% get_summary_stats(total_acres, type = "mean_sd")

d1 <- land_bl %>% t_test(total_acres  ~ farmers_hh, detailed = T) %>% add_significance()
d2 <- land_bl %>% t_test(total_plots  ~ farmers_hh, detailed = T) %>% add_significance()

table_dd=bind_rows(d1,d2) %>% 
  rename(`Inside \nRamthal`=estimate1,`Outside \nRamthal`=estimate2,t=statistic) %>% 
  select(.y. ,`Inside \nRamthal`,`Outside \nRamthal`,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_dd)

# [D4]	Survey/hissa number  ||  bl_plot_SrvyHis         ----
D4_h <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("D4_h")) %>% 
  select(hh_id:D4_hissa_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "hissa_bl") %>% 
  filter(hissa_bl != "" )
D4_h$plot_num <- str_replace(D4_h$plot_num, "D4_hissa_(\\d)$", "plot_0\\1")
D4_h$plot_num[D4_h$plot_num=="D4_hissa_10"] <- "plot_10"

D4_ <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("D4_"), -starts_with("D4_h"))%>% 
  select(hh_id:D4_10  )%>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "survey_bl")%>% 
  filter(survey_bl != "" ) 
D4_$plot_num <- str_replace(D4_$plot_num, "D4_(\\d)$", "plot_0\\1")
D4_$plot_num[D4_$plot_num=="D4_10"] <- "plot_10"

bl_plot_SrvyHis <- full_join(D4_,D4_h)
bl_plot_SrvyHis$hissa_bl[is.na(bl_plot_SrvyHis$hissa_bl)] <- ""

#       D5 Village in which survey plot is located     ----
D5_land <- baseline_RMTL %>% select(hh_id, starts_with("D5_"))






#       D63 Leased land                                ----
#The questions in the CULT module were not asked about "Leased land"
d6_own_land <- rmtl_baseline2016 %>% select(hh_id, starts_with("D6_"))
d63_Leased_land_2016 <- rmtl_baseline2016 %>% select(hh_id,matches ("D63_"),matches ("D11"),-ends_with("_0")) # %>% filter(!is.na(D63_acer_1  ))

# [D6]  Area of Plot (acres/gunta)  ||  bl6_plotAcre      ----
bl_d6 <- rmtl_baseline2016 %>% select(farmers_hh, hh_id,D6_1:D6_10) %>% 
  pivot_longer(-c(farmers_hh, hh_id), names_to = "plot_num", values_to = "plot_acre")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "D6_(\\d)$", "plot_0\\1")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "^D6_", "plot_")
bl6_plotAcre <- filter(bl_d6,!is.na(plot_acre))



#       D12 plot irri last 5 years                      ----
# Has this plot been irrigated at least once during the last 5 years? 

bl_d12 = rmtl_baseline2016  [,c(1,grep("^D12",names(rmtl_baseline2016 ) ))] %>% 
  select(hh_id,D12_1,D12_2,D12_3,D12_4,D12_5,D12_6,D12_7,D12_8,D12_9,D12_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_plot_5y")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "D12_(\\d)$", "plot_0\\1")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "^D12_", "plot_")
bl_d12 <- filter(bl_d12,!is.na(irri_plot_5y))

# [D21] plot irri method last 5 years  ||  bl21_irri_methods                    ----

# D21	What is the method of irrigation?
#     #1 Flood #2 Furrows #3	Drip #4	Sprinkler #5 Manual  #6	Hose # -888	Other, specify

D21_method <- rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id, starts_with("D21"), -c("D21_12","D21_0","D21_os_0" ))
D21_method$D21_1[D21_method$D21_os_1== "BOREWEL"] <- 2
D21_method$D21_2[D21_method$D21_os_2== "BOREWELL"] <- 2

D21_method <- D21_method %>% select(-c(D21_os_1 ,D21_os_2 ) ) %>% 
  pivot_longer(-c(farmers_hh,hh_id), 
               names_to = "plot_num", values_to = "irri_method") %>%
  filter(irri_method>0 )
D21_method$plot_num <- str_replace(D21_method$plot_num, "D21_(\\d)$", "plot_0\\1")

bl21_irri_methods = D21_method

# [D28]	Was the crop irrigated? | Crop-Plot-SEASON  ||  bl28_irri_plot_season   ----

D28_1 <- # 2015-16 RABI
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_1") ) %>% 
  select(farmers_hh: D28_1_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_1$plot_num <- str_replace(D28_1$plot_num, "D28_1_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_1$plot_num[D28_1$plot_num=="D28_1_irrigated_1_10"] <- "plot_10"
D28_1=D28_1 %>% 
  group_by(farmers_hh,hh_id,plot_num) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="rabi_2015_16")
  
D28_2 <- # 2015 KHARIF
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_2") ) %>% 
  select(farmers_hh: D28_2_irrigated_1_7)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_2$plot_num <- str_replace(D28_2$plot_num, "D28_2_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_2=D28_2 %>% 
  group_by(farmers_hh,hh_id,plot_num) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="kharif_2015")

D28_3 <- # 2014-15 RABI
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_3") ) %>% 
  select(farmers_hh: D28_3_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_3$plot_num <- str_replace(D28_3$plot_num, "D28_3_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_3=D28_3 %>% 
  group_by(farmers_hh,hh_id,plot_num) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="rabi_2014_15")

bl28_irri_plot_season <- rbind(D28_1,D28_2,D28_3)





# [D13] source  ||  bl_source_irrigate                                          ----
# "What was the principal source of irrigation for this plot over the last 5 years?"
#1 Canal  #2	Tank  #3	Open well  #4	River/Pond/Lake  #5	Bore well  # -888	Other, specify

D13 <-  
  rmtl_baseline2016 %>% select(hh_id,starts_with("D13"),-c( D13_12,D13_0) )
D13$D13_1[D13$D13_os_1 == "BOREWELL"] <- 5
D13$D13_2[D13$D13_os_2 == "BOREWELL"] <- 5
D13 <-D13 %>% select(-c(D13_os_1,D13_os_2 ) )%>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_source_5y")
D13$plot_num <- str_replace(D13$plot_num, "D13_(\\d)$", "plot_0\\1")
D13$plot_num[D13$plot_num == "D13_10"] <- "plot_10" 

bl13_source_irrigate <- D13 %>% filter(irri_source_5y>0)

# [D24] crops planted
# What crops are planted on this plot? Mark all that apply (Perennial crops will be listed in 2 seasons)

D24 <- 
  rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id,                         
         starts_with("D4_"),                 # srvy+hissa 
         starts_with("D24_"),                # crop       
         starts_with("D27_"))   %>%          # plot size
  select(1:8,39,41,43,45,47,55 )

x=rmtl_baseline2016 %>% 
  select(hh_id,
         D27_1_Crop_1_1 ,D27_1_Crop_1_2, D27_1_Crop_1_3 ,D27_1_Crop_1_4, D27_1_Crop_1_5,
         D27_1_Crop_1_6, D27_1_Crop_1_7, D27_1_Crop_1_8 ,D27_1_Crop_1_9, D27_1_Crop_1_10 ,
         
         D27_1_Crop_2_1 ,D27_1_Crop_2_2, D27_1_Crop_2_3 ,D27_1_Crop_2_4, D27_1_Crop_2_5,
         D27_1_Crop_2_6, D27_1_Crop_2_7, D27_1_Crop_2_8 ,D27_1_Crop_2_9, D27_1_Crop_2_10 ,

         D27_2_Crop_1_1 ,D27_2_Crop_1_2, D27_2_Crop_1_3 ,D27_2_Crop_1_4, D27_2_Crop_1_5,
         D27_2_Crop_1_6, D27_2_Crop_1_7, D27_2_Crop_2_1 ,D27_2_Crop_2_2, D27_2_Crop_2_3 ,

         D27_3_Crop_1_1 ,D27_3_Crop_1_2, D27_3_Crop_1_3 ,D27_3_Crop_1_4, D27_3_Crop_1_5,
         D27_3_Crop_1_6, D27_3_Crop_1_7, D27_3_Crop_1_8 ,D27_3_Crop_1_9, D27_3_Crop_1_10 ,
         )

x[x==-444] <- 0
x[x==-666] <- 0
x[x==-999] <- 0
x[x==-888] <- 888       
x[x==-777] <- 0       

x1=x %>% mutate(af = rowSums(.[names(.)[2:41]], na.rm = T))
x2=x1[,42]





# sample <----  
  
sample <-  
  rmtl_baseline2016   %>%  
  select(south_north_inner, sampledafter200517 ,Srno, SI, survey, 
         si_no, surveyround, south1_north0 ,hh_id, 
         inner_plots, in1_out0, in_out_intersect)


