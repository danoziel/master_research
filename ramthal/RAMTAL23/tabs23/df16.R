#| THIS R SCRIPT [df16.R] is data-frames/sets/bases of "2016 baseline survey"
#| 🟡BASELINE 2016  | rmtl_baseline2016 #= baseline_RMTL

rmtl_baseline2016 <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv")

#🟠MIDELINE 2018| rmtl_midline2018 is in `df18.R` script
#🟣MIDELINE 2022| rmtl_srvy22 is in `DF_22.R` script

library(dplyr)
library(haven)
library(tidyr)
library("stringr") #"str_replace"
library(summarytools)

####### HH CHARACTERISTICS #######             ----
# vars_02 caste income 2016
# A18 What is your caste?
# A19 Which caste category does this fall under?
# A13 According to what you indicated, your total HH income is Rs. [     ].
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)


# C5	What is their relationship to the head of household?
# C6	Are they literate?
# C7	What is their educational level?
C5=rmtl_baseline2016[,c(1,grep("^C5_[1-9]",names(rmtl_baseline2016)))]  
C5[C5>1] <- NA
C5 %>% select(where(~!all(is.na(.x))))

edu1=rmtl_baseline2016[,c(1,grep("^C5_[1-9]|^C7_[1-9]",names(rmtl_baseline2016)))] %>% select(-ends_with("bin"))%>% mutate(edu_head_hh=NA ) 

edu1$edu_head_hh <- ifelse(edu1$C5_1 == 1, edu1$C7_1, edu1$edu_head_hh)
edu1$edu_head_hh <- ifelse(edu1$C5_2 == 1, edu1$C7_2, edu1$edu_head_hh)
edu1$edu_head_hh <- ifelse(edu1$C5_3 == 1, edu1$C7_3, edu1$edu_head_hh)
edu1a <-
  edu1 %>% 
  mutate(edu_head_hh=
           ifelse(C5_1==1,C7_1,ifelse(C5_3 == 1, C7_3,ifelse(C5_4 == 1, C7_4,ifelse(C5_5 == 1,C7_5,ifelse(C5_6 == 1, C7_6,ifelse(C5_7 == 1, C7_7,ifelse(C5_8 == 1, C7_8, ifelse(C5_9 == 1, C7_9,ifelse(C5_10 == 1,C7_10,ifelse(C5_11== 1, C7_11,ifelse(C5_13 == 1, C7_13, ifelse(C5_16 == 1,C7_16,NA)))))))))))))


vars_02=
  rmtl_baseline2016 %>% 
  select(hh_id,A22, #caste
         A23, #caste category
         F13,# total HH income
         B9) %>% # In the last 5 years, has the household received any assistance
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

####### INCOME tab #######             ----



# NET income earned by household members in the past 12 months.       				
incom16= rmtl_baseline2016 %>% select(hh_id ,contains(c("F")))

# F1	Income sent by seasonal migrating household members	
# F2	Remittances (from permanent migrants)

#   Do you earn any income from this source? (Y/N)	
rmtl_baseline2016 %>% select(contains(c("F1_s","F2_s"))) %>% freq()

#   How much earned in past month?	
#   How much earned in 2015?
f1_f2=rmtl_baseline2016 %>% select(contains(c("F1_","F2_")))
summary(f1_f2)


####### F3-F12 not include income from seasonal or permanent migrants.

f3_f13=
  rmtl_baseline2016 %>% 
  select(starts_with (c("F"))) %>% select(contains(c("_s"))) %>% freq()

# F3	"Farming own or rented land (Net profit)
# F4	"Own livestock (Net profit)
# F5	"Own non-agricultural business (Net profit)
# F6	Salaried job
# F7	Casual work or daily labor by current household members
# F9	Government pension or scheme
# F10	Rent/lease of property or land (land, house, vehicle, electronic appliances, tractor, etc.)
# F11	Other jobs or activities not already mentioned
# F12	Total (Rs.)
# F13	According to what you indicated, your total HH income is Rs. [     ].
# F14	What is you income expectation in 2 years from now? (Rs.)		


  ####### ASSETS tab #######             ----

# E1	In the past 3 years, did your household sell any agricultural land?
# E2	How many plots has your household sold in the past 3 years?
# E3	When was the plot sold? (MMYYYY)
# E4	How much was this plot sold for? (Rs.)
# E4	What was the area of the plot? (Acres)

# E6-E22
# [A] How many of this item does the household currently own? (0 if none)	
# [B] How many of this item did the household own 3 years ago?

rmtl_baseline2016 %>% select(starts_with (c("E"))) 

e2016= rmtl_baseline2016 %>% 
  select(hh_id, E6_1:E21_1 ) %>% select(hh_id,contains(c("_1")))%>% select(-c(E14_1,E19_1)) # e17
e2016[is.na(e2016)] <- 0

e9=e2016 %>% filter(!is.na(E9_1))
freq(e9$E9_1)
compute_summary_1_99(e9$E9_1)
e2016$E9_1 [e2016$E9_1>20] <- 20

e2016A=
  e2016 %>% mutate(total_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
         total_livestock=E6_1+E7_1+E8_1+E9_1,
         total_farm_equipments=E10_1+E11_1+E12_1+E13_1) 

e2016B= e2016 %>% select(-c(E20_1,E21_1))
e2016B[e2016B>0 & e2016B<1000] <- 1
e2016B=e2016B %>% 
  mutate(yn_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
         yn_livestock=E6_1+E7_1+E8_1+E9_1,
         yn_farm_equipments=E10_1+E11_1+E12_1+E13_1) 




# LIVESTOCK
# E6	Cows
# E7	Bullock
# E8	Buffaloes
# E9	Goats and sheep
# FARM EQUIPMENT
# E10	Tractor
# E11	Plough
# E12	Thresher
# E13	Seed drill
# E14	JCB (front/back loader, for digging/leveling)
# VEHICLES
# E15	Cycles
# E16	Motorcycles
# E17	Cars

# HOUSEHOLD ITEMS
# E18	Fridge
# E19	Television
# E20	Gold (in grams)
# E21	Silver (in grams)























####### CULTIVATION  ############             ####
# plot info  bl_plot_dt  ----

bl_plot_dt <-
  bl_plot_SrvyHis %>%                 # [D4] Survey/hissa number  : 3,459 × 4
  left_join( bl6_plotAcre ) %>%       # [D6] Area acres/gunta     : 3,455 × 4
  left_join( bl21_irri_methods ) %>%  # [D21] plot irri method 5y : 191 × 4
  left_join( bl13_source_irrigate)    # [D13] source              : 194 × 3

bl_plot_dt$irri_method[is.na(bl_plot_dt$irri_method )] <- 0
bl_plot_dt$irri_source_5y[is.na(bl_plot_dt$irri_source_5y)] <- 0

# season-plot info       -----
bl_season_plot_irri <-
  bl28_irri_plot_season %>%            # [D28]	SEASON crop irrigated : 6,332 × 5
  left_join( bl21_irri_methods ) %>%   # [D21] plot irri method 5y : 191 × 4
  left_join( bl13_source_irrigate) %>% # [D13] source    5y       : 194 × 3
  left_join( bl6_plotAcre )            # [D6] Area acres/gunta       : 3,455 × 4
bl_season_plot_irri$irri_method_5y[is.na(bl_season_plot_irri$irri_method_5y )] <- 0
bl_season_plot_irri$irri_source_5y[is.na(bl_season_plot_irri$irri_source_5y)] <- 0


    bl_crop_plot_3s   # A tibble: 7,268 × 5

--------------------------------------------------------------------------------
  
#       D2 Total Area (acres)                            ----

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

#       D5 Village in which survey plot is located       ----
D5_land <- baseline_RMTL %>% select(hh_id, starts_with("D5_"))






#       D63 Leased land                                   ----
#The questions in the CULT module were not asked about "Leased land"
d6_own_land <- rmtl_baseline2016 %>% select(hh_id, starts_with("D6_"))
d63_Leased_land_2016 <- rmtl_baseline2016 %>% select(hh_id,matches ("D63_"),matches ("D11"),-ends_with("_0")) # %>% filter(!is.na(D63_acer_1  ))

# [D6]  Area of Plot (acres/gunta)  ||  bl6_plotAcre      ----
bl_d6 <- rmtl_baseline2016 %>% select(farmers_hh, hh_id,D6_1:D6_10) %>% 
  pivot_longer(-c(farmers_hh, hh_id), names_to = "plot_num", values_to = "plot_acre")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "D6_(\\d)$", "plot_0\\1")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "^D6_", "plot_")
bl6_plotAcre <- filter(bl_d6,!is.na(plot_acre))
rm(bl_d6)


#       D12 plot irri last 5 years                        ----
# Has this plot been irrigated at least once during the last 5 years? 

bl_d12 = rmtl_baseline2016  [,c(1,grep("^D12",names(rmtl_baseline2016 ) ))] %>% 
  select(hh_id,D12_1,D12_2,D12_3,D12_4,D12_5,D12_6,D12_7,D12_8,D12_9,D12_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_plot_5y") %>% 
  filter(!is.na(irri_plot_5y))
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "D12_(\\d)$", "plot_0\\1")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "^D12_", "plot_")


# [D21] plot irri method last 5 years  ||  bl16_irri_methods                    ----

# D21	What is the method of irrigation?
#     #1 Flood #2 Furrows #3	Drip #4	Sprinkler #5 Manual  #6	Hose # -888	Other, specify

D21_method <- rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id, starts_with("D21"), -c("D21_12","D21_0","D21_os_0" ))
D21_method$D21_1[D21_method$D21_os_1== "BOREWEL"] <- 2
D21_method$D21_2[D21_method$D21_os_2== "BOREWELL"] <- 2

D21_method <- D21_method %>% select(-c(D21_os_1 ,D21_os_2 ) ) %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_method_5y") %>%filter(irri_method_5y>0 )
D21_method$plot_num <- str_replace(D21_method$plot_num, "D21_(\\d)$", "plot_0\\1")

bl16_irri_methods = D21_method
rm(D21_method )

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
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="kharif_2015") %>% 
  ungroup()

D28_3 <- # 2014-15 RABI
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_3") ) %>% 
  select(farmers_hh: D28_3_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_3$plot_num <- str_replace(D28_3$plot_num, "D28_3_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_3$plot_num[D28_3$plot_num=="D28_3_irrigated_1_10"] <- "plot_10"
D28_3=D28_3 %>% 
  group_by(farmers_hh,hh_id,plot_num) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="rabi_2014_15")

bl28_irri_plot_season <- rbind(D28_1,D28_2,D28_3) %>% rename(irri_plot_3s =irri_plot )

rm(D28_1,D28_2,D28_3)



# [D13] source  ||  bl13_source_irrigate                                          ----
# "What was the principal source of irrigation for this plot over the last 5 years?"
#1 Canal  #2	Tank  #3	Open well  #4	River/Pond/Lake  #5	Bore well  # -888	Other, specify

D13 <-  
  rmtl_baseline2016 %>% select(farmers_hh, hh_id,starts_with("D13"),-c( D13_12,D13_0) )
D13$D13_1[D13$D13_os_1 == "BOREWELL"] <- 5
D13$D13_2[D13$D13_os_2 == "BOREWELL"] <- 5
D13 <-D13 %>% select(-c(D13_os_1,D13_os_2 ) )%>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_source_5y")
D13$plot_num <- str_replace(D13$plot_num, "D13_(\\d)$", "plot_0\\1")
D13$plot_num[D13$plot_num == "D13_10"] <- "plot_10" 

bl13_source_irrigate <- D13 %>% filter(irri_source_5y>0)
rm(D13 )

# [D24] crops planted  || bl_crop_plot_3s                                        ----
# What crops are planted on this plot? Mark all that apply (Perennial crops will be listed in 2 seasons)

d24 <- 
  rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id,                         
         starts_with("D4_"),                 # srvy+hissa 
         starts_with("D24_"),                # crop       
         starts_with("D27_"),     # plot size
         -contains("os"),
         -ends_with( c("11","12","13","14","15","16","17","_0" )))

D24 <- 
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "D24_") )%>% 
  select(-contains("os"),
         -ends_with( c("11","12","13","14","15","16","17","_0" ))) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "crop_bl") %>% 
  filter(!is.na(crop_bl))

D24$season <- sub("^(D24_1)_.*", "rabi_2015_16", D24$plot_num)
D24$season <- sub("^(D24_2)_.*", "kharif_2015", D24$season)
D24$season <- sub("^(D24_3)_.*", "rabi_2014_15", D24$season)

D24$crop_num <- sub("^.*_(Crop_\\d+)_.*", "\\1", D24$plot_num)

D24$plot_num <- str_replace(D24$plot_num, "^.*_(\\d)$", "plot_0\\1")
D24$plot_num <- str_replace(D24$plot_num, "^.*_10$", "plot_10")

D24$plot_num[D24$plot_num == "D24_10"] <- "plot_10" 

D24[,1] %>% distinct() # A tibble: 1,729 × 1

D24$crop_bl[D24$crop_bl == -444] <- 0
D24$crop_bl[D24$crop_bl == -666] <- 0
D24$crop_bl[D24$crop_bl == -777] <- 0
D24$crop_bl[D24$crop_bl == -999] <- 0
D24=D24 %>% filter(crop_bl != 0)

D24[,1] %>% distinct() # A tibble: 1,723 × 1 

bl_crop_plot_3s= 
  D24 %>%rename(crop_code =crop_bl) %>%  
  left_join(list_crop)
  




# sample <----  
  
sample <-  
  rmtl_baseline2016   %>%  
  select(south_north_inner, sampledafter200517 ,Srno, SI, survey, 
         si_no, surveyround, south1_north0 ,hh_id, 
         inner_plots, in1_out0, in_out_intersect)
######################    essantials    ----


attach(rmtl_baseline2016)
detach(rmtl_baseline2016)

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
    p = quantile(x, 0.01),
    P = quantile(x, 0.99),
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

# LM
#m_edu <- lm(hh_irrigated ~ edu_hh_head + edu_hh + hh_literacyPrt , social_bl16)
m_edu <- lm(hh_irrigated ~ hh_literacyPrt+edu_hh_head , social_bl16)
summary(m_edu)
sjPlot::tab_model(m_edu,show.se = TRUE,
                  pred.labels = c("(Intercept)",  "% literacy in HH","HH head education level"),
                  dv.labels = "`",
                  string.se = "__Std. Err__",
                  string.pred = "Predictors",
                  string.est = "Coef.",
                  string.ci = "CI (95%)",
                  string.p = "p",
                  p.style = "numeric",
                  col.order = c("est","se","ci","p")
)


# regression with fixed effects
model <- lm(hh_irrigated ~ factor(District) + X, data = social_bl16)

broom::tidy(m_edu) %>%mutate(p.value = round(p.value, 3)) %>% 
  kbl() %>% kable_minimal()

#nice_lm
model1 <- lm(mpg ~ cyl + wt * hp, mtcars)
model2 <- lm(qsec ~ disp + drat * carb, mtcars)
mods <- nice_lm(list(model1, model2), standardize = TRUE)
mods
nice_table(mods, highlight = TRUE)


# write.csv
write.csv(rmtl_In_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_In_groups.csv", row.names=FALSE)
write.csv(rmtl_InOut_groups, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut_groups.csv", row.names=FALSE)
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv", row.names=FALSE)



