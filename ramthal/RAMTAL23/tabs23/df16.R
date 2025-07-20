#| THIS R SCRIPT [df16.R] is data-frames/sets/bases of "2016 baseline survey"
#| 🟡BASELINE 2016  | rmtl_baseline2016 #= baseline_RMTL


CMF_RAMTHAL_IRRIGATION_18_Aug_2016_cleaned <- read_dta("~/master_research/DATAs/ramthal_data/baseline_survey_2016/CMF_RAMTHAL_IRRIGATION_18 Aug 2016 - cleaned.dta")

library(readr)
rmtl_baseline2016 <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv")

library(dplyr)
library(haven)
library(tidyr)
library("stringr") #"str_replace"
library(summarytools)


full_seasons_2016 <- 
  rmtl_baseline2016  %>% select(hh_id)%>% 
  mutate(kharif_2015=1,rabi_2014_15=1,rabi_2015_16=1) %>% 
  pivot_longer(!c(hh_id),names_to = "season", values_to = "count") %>% 
  select(-"count")

####### HH CHARACTERISTICS #######             ----
# vars_02 caste income 2016
# A18 What is your caste?
# A19 Which caste category does this fall under?
# A13 According to what you indicated, your total HH income is Rs. [     ].
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)


# C5	What is their relationship to the head of household?
# C6	Are they literate?
# C7	What is their educational level?

#
#_____DF_Socioeconomic ___________________ [economic16]       ----

# bpl_card AND official_assistance
# B8	Does this household have a BPL ration card?
# B9	In the last 5 years, has the household received any assistance from the municipality/government/gram panchayat? (NOT including ration card)
rmtl_baseline2016 %>% select(contains(c( "B8","B9")))
# B1	Is this housing constructed with pucca, semi-pucca, or kutcha materials?1Pucca House/ 2Semi Pucca House/ 3Kutcha House
# D2	How many acres (guntas) of land does your household currently own?
# D3	How many plots of land does your household currently own?
rmtl_baseline2016 %>% select(contains(c( "B1","D2" ,"D3")))
# F12	Total (Rs.)
# F13	According to what you indicated, your total HH income is Rs. [     ].
# F14	What is you income expectation in 2 years from now? (Rs.)		

f13=
  rmtl_baseline2016 %>% select(hh_id ,contains(c("F12_year","F13"))) %>% 
  mutate(income_2015= ifelse(is.na(F12_year), F13,ifelse(is.na(F13),F12_year, pmax(F12_year,F13, na.rm = TRUE)))) %>% 
  select(hh_id,income_2015)
summary_1_99(f12_f13$bl_yr_income)
f13$income_2015[f13$income_2015<7000] <- NA
f13$income_2015[f13$income_2015>1000000] <- NA
f13$incomeK_2015=f13$income_2015/1000  


economic16=
  rmtl_baseline2016 %>% 
  select(hh_id, D2 ,D3, B1,B8,B9) %>% 
  rename(bpl_card=B8,official_assistance=B9,
         total_plots=D3, total_acre=D2) %>%   #[total_acre | total_plots] 
  mutate(total_acre_bin = ntile(total_acre, 5)) %>% 
  mutate(housing_str01=ifelse(B1==1,1,0)) %>% 
  mutate(housing_str321=ifelse(B1==1,3,ifelse(B1==3,1,2))) %>% 
  select(-B1) %>% 
  left_join(f13) %>% 
  mutate(income2015_bin = ntile(incomeK_2015, 5))


# Summary stat
summary_1_99(economic16$total_acre)
summary_1_99(economic16$total_plots)

# remove outlyres
economic16$total_acre16[economic16$total_acre16>50] <- NA
economic16$total_plots16[economic16$total_plots16>8] <- NA

write.csv(economic16, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/economic16.csv", row.names=FALSE)


#DF education_age_gndr_2016 _______________________ []      ----
#   
# C5	What is their relationship to the head of household?
# C7	What is their educational level? 1-6
C3_gndr=rmtl_baseline2016 %>% select(hh_id, starts_with("C3_")) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to= "gendar") %>% 
  mutate(id_member = gsub("C3_", "C_", id_member))

c4= rmtl_baseline2016 %>% select(hh_id,starts_with("C4_" ) ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "age")
c4$id_member <- sub("^C4_(\\d{1,2})","C_\\1",c4$id_member )

c5= rmtl_baseline2016 %>% select(hh_id,starts_with("C5" ),-contains("_os_") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "hh_member")
c5$id_member <- sub("^C5_(\\d{1,2})","C_\\1",c5$id_member )

c6= rmtl_baseline2016 %>% select(hh_id,starts_with("C6" ),-contains("_os_") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "literate")
c6$id_member <- sub("^C6_(\\d{1,2})","C_\\1",c6$id_member )

c7= rmtl_baseline2016 %>% select(hh_id, starts_with("C7"), -ends_with("_bin") ) %>% 
  pivot_longer(!hh_id, names_to = "id_member", values_to = "edu_level")
c7$id_member <- sub("^C7_(\\d{1,2})","C_\\1",c7$id_member )

ed = 
  full_join(c5,c4) %>% full_join(c6) %>% full_join(c7) %>% full_join(C3_gndr) %>% 
  filter(!is.na(age))

ed$hh_member[ed$hh_member<0 ] <- 800
ed$hh_member[is.na(ed$hh_member)] <- 1000

ed$age[ed$age==0] <- 45
ed$edu_level[ed$edu_level<0] <- NA

# add col "hh_head"
#   HH with more then one head- the older = will be the HH head
#   HH no head- the low member + the older = will be the HH head
edu <- 
  ed %>%
  group_by(hh_id) %>%
  mutate( count_hh_member_1 = sum(hh_member == 1, na.rm = TRUE)) %>% 
  mutate(hh_haed=ifelse(hh_member ==1 & count_hh_member_1 ==1,1,
                        ifelse(count_hh_member_1 %in% c(0,2,3) & id_member =="C_1" ,1,NA )
  ) ) %>%ungroup() 

# DF  hh_haed_2016
hh_haed_2016= edu %>%
  filter(hh_haed ==1) %>% 
  select(hh_id, id_member, age, edu_level, gendar ) %>% 
  rename(hh_haed_age=age,hh_haed_edu_level=edu_level,hh_haed_gendar=gendar)
hh_haed_2016$hh_haed_edu_level[is.na(hh_haed_2016$hh_haed_edu_level)] <- 0
write.csv(hh_haed_2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/hh_haed_2016.csv", row.names=FALSE)


educ = edu %>% 
  mutate(
    educated_6th=ifelse(edu_level >=2 & literate ==1,1,0),
    educated_PUC=ifelse(edu_level >=3 & literate ==1,1,0), # Grade 11th and above
    educated_UG=ifelse(edu_level >=3 & literate ==1,1,0))

# DF education_age_gndr_2016  ----
education_age_gndr_2016 =
  educ %>% 
  group_by(hh_id) %>% 
  summarise(
    edu_level_hh= mean(edu_level, na.rm = TRUE),
    literate_hh_pct=mean(literate, na.rm = TRUE),
    educated_6th_pct_hh= mean(educated_6th, na.rm = TRUE),
    educated_PUC_pct_hh=mean(educated_PUC, na.rm = TRUE),
    educated_UG_pct_hh=mean(educated_UG, na.rm = TRUE)
  ) %>% 
  full_join(hh_haed_2016) 


write.csv(education_age_gndr_2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/education_age_gndr_2016.csv", row.names=FALSE)
rm(edu, educ, ed,c4,c5,c6,c7)

economic16 education_age_gndr_2016


#_____DF_Social __________________________ [caste]            ----

# a21	What is your religion?	#	rmtl_baseline2016 %>% count(A21)
# a22 What is your caste? # rmtl_baseline2016 %>% count(A22) / rmtl_baseline2016$A22_os
# a23 Which caste category does this fall under?

caste1= rmtl_baseline2016 %>% select(hh_id,A21, A22,A22_os,A23)
caste1 %>% count(A23)

# 1 General Category # 2 Other Backward Caste # 3 Scheduled Caste # 4 Scheduled Tribe
caste1$A23[caste1$A23 %in% c("","-777","-999")] <- NA
caste2=caste1%>%
  mutate(caste_01=ifelse(A23=="1",1,                # General Category
                         ifelse(A23 %in% c("2","3","4"),0 ,# OBC/SC/ST
                                NA))) %>% 
  rename(caste_4321=A23) %>% 
  mutate(caste_cat=ifelse(caste_01=="1","GC",
                   ifelse(caste_01 %in% c("2","3","4"),
                   "OBC/SC/ST","")))
         

caste = caste2 %>% select(hh_id, caste_01, caste_4321)
rm(caste1,caste2)
#

# DF demographic_vars_2016 ------

demographic_vars_2016 <- 
  left_join(economic16,education_age_gndr_2016) %>% 
  left_join(caste) %>% 
  left_join(BL_total_assets) %>% 
  left_join(BL_incomS)
  
write.csv(demographic_vars_2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/demographic_vars_2016.csv", row.names=FALSE)


# IRRIGARION 2016 -------------------------

#🟡  BASELINE
#[D12] Has this plot been irrigated at least once during the last 5 years?
bl_hh_irrigation = rmtl_baseline2016 %>%  select(in1_out0,hh_id,starts_with("D12") ,-c(D12_11:D12_17 )) %>% 
  mutate(total_irriPlot = rowSums (.[names(.)[3:12]], na.rm = T)) %>%
  rename( hh_irrigate_BL= D12_) %>% select(in1_out0,hh_id,hh_irrigate_BL )



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


# BL_incomSBL_incomS ----
BL_incomS_A=
  rmtl_baseline2016 %>% 
  select(hh_id,F5_source,F6_source, F7_source,F9_source,F10_source,F11_source)
BL_incomS_A[BL_incomS_A==2] <- 0

BL_incomS=BL_incomS_A %>% 
  mutate(
    job_income_sourceS=F5_source+F6_source+ F7_source+F11_source,
    govPnsin_scheme=F9_source,
    rent_property= F10_source) %>% 
  select(hh_id,job_income_sourceS,govPnsin_scheme, rent_property)






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
  e2016 %>% mutate(
    total_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
    total_livestock=E6_1+E7_1+E8_1+E9_1,
    total_farm_equipments=E10_1+E11_1+E12_1+E13_1) 

e2016B= e2016 %>% select(-c(E20_1,E21_1))
e2016B[e2016B>0 & e2016B<1000] <- 1
e2016B=e2016B %>% 
  mutate(yn_assets=E6_1+E7_1+E8_1+E9_1+E10_1+E11_1+E12_1+E13_1+E15_1+E16_1+E18_1,
         yn_livestock=E6_1+E7_1+E8_1+E9_1,
         yn_farm_equipments=E10_1+E11_1+E12_1+E13_1) 

# BL_total_assets ----
BL_total_assets = e2016 %>% 
  mutate(
    total_livestock=E6_1+E7_1+E8_1+E9_1,
    total_farm_equipments=E10_1+E11_1+E12_1+E13_1) %>% 
  select(hh_id,total_livestock,total_farm_equipments )

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

# [D4]	Survey/hissa number  ||  bl_plot_SrvyHis         ----
D4_h <- 
  rmtl_baseline2016 %>% select(hh_id, starts_with("D4_h")) %>% 
  select(hh_id:D4_hissa_10) %>% mutate(D4_hissa_9=as.character(D4_hissa_9),D4_hissa_10=as.character(D4_hissa_10)) %>%  
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

rm(D4_,D4_h)


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
  
      
      quantile(a_plots_size$acres, 0.99, na.rm = TRUE)
    
      
      
#       D2 Total Area (acres)                            ----

# D2	How many acres (guntas) of land does your household currently own?
land_bl <- rmtl_baseline2016 %>% 
  select(hh_id,D2,D2_acer,D2_guntas,D3,in1_out0) %>% 
  rename( total_acres=D2 , total_plots=D3) %>% 
      filter(total_acres<40.74) # elimination of the 99%

quantile(land_bl$total_acres, 0.99, na.rm = TRUE)
    

# D2 total_acres COR D3 total_plots 
ggplot(land_bl, aes(x=total_acres, y=total_plots)) + geom_point(shape=18, color="green4")+ geom_smooth(method=lm, se=FALSE, color="brown4")+ theme_minimal()
d2d3 <- lm(total_acres ~ total_plots, land_bl)
summary(d2d3)


# D2	Total Area  (acres) 


# total_acres | total_plots summary_stats t.test
library(rstatix)
land_bl %>% get_summary_stats(total_acres, type = "mean_sd")
land_bl %>% group_by(in1_out0) %>% get_summary_stats(total_acres, type = "mean_sd")

d1 <- land_bl %>% t_test(total_acres  ~ in1_out0, detailed = T) %>% add_significance()
d2 <- land_bl %>% t_test(total_plots  ~ in1_out0, detailed = T) %>% add_significance()

table_dd=bind_rows(d1,d2) %>% 
  rename(`Inside \nRamthal`=estimate1,`Outside \nRamthal`=estimate2,t=statistic) %>% 
  select(.y. ,`Inside \nRamthal`,`Outside \nRamthal`,n1,n2,estimate,conf.low,conf.high,t,df,p) 
nice_table(table_dd)






#       D63 Leased land                                   ----
#The questions in the CULT module were not asked about "Leased land"
d6_own_land <- rmtl_baseline2016 %>% select(hh_id, starts_with("D6_"))
d63_Leased_land_2016 <- rmtl_baseline2016 %>% select(hh_id,matches ("D63_"),matches ("D11"),-ends_with("_0")) # %>% filter(!is.na(D63_acer_1  ))

# [D6]  Area of Plot (acres/gunta)     ||  bl6_plotAcre            ----
bl_d6 <- rmtl_baseline2016 %>% select(hh_id,D6_1:D6_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "plot_acre")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "D6_(\\d)$", "plot_0\\1")
bl_d6$plot_num <- str_replace(bl_d6$plot_num, "^D6_", "plot_")
bl6_plotAcre <- filter(bl_d6,!is.na(plot_acre))
rm(bl_d6)


# [D12] plot irri last 5 years         ||  bl_d12                  ----
# Has this plot been irrigated at least once during the last 5 years? 

bl_d12 = rmtl_baseline2016  [,c(1,grep("^D12",names(rmtl_baseline2016 ) ))] %>% 
  select(hh_id,D12_1,D12_2,D12_3,D12_4,D12_5,D12_6,D12_7,D12_8,D12_9,D12_10) %>% 
  pivot_longer(-hh_id, names_to = "plot_num", values_to = "irri_plot_5y") %>% 
  filter(!is.na(irri_plot_5y))
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "D12_(\\d)$", "plot_0\\1")
bl_d12$plot_num <- str_replace(bl_d12$plot_num, "^D12_", "plot_")

d12_16=bl_d12 %>% right_join (bl6_plotAcre)

# [D13] source                         ||  bl13_source_irrigate    ----
# "What was the principal source of irrigation for this plot over the last 5 years?"
#1 Canal  #2	Tank  #3	Open well  #4	River/Pond/Lake  #5	Bore well  # -888	Other, specify

D13 <-  
  rmtl_baseline2016 %>% select(farmers_hh,hh_id,starts_with("D13"),-c( D13_12,D13_0) )
D13$D13_1[D13$D13_os_1 == "BOREWELL"] <- 5
D13$D13_2[D13$D13_os_2 == "BOREWELL"] <- 5
D13 <-D13 %>% select(-c(D13_os_1,D13_os_2 ) )%>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plot_num", values_to = "irri_source_5y")
D13$plot_num <- str_replace(D13$plot_num, "D13_(\\d)$", "plot_0\\1")
D13$plot_num[D13$plot_num == "D13_10"] <- "plot_10" 

bl13 <- D13[,2:4] %>% filter(irri_source_5y>0)
bl13$irri_source_5y[bl13$irri_source_5y==1] <- "Canal"
bl13$irri_source_5y[bl13$irri_source_5y==2] <- "Tank"
bl13$irri_source_5y[bl13$irri_source_5y==3] <- "Openwell"
bl13$irri_source_5y[bl13$irri_source_5y==4] <- "Borewell"
bl13$irri_source_5y[bl13$irri_source_5y==5] <- "River/Pond/Lake"

bl13_source_irrigate=bl13

rm(D13 )


d12_16_13=bl_d12 %>% right_join (bl6_plotAcre) %>% left_join(bl13)



# [D21] plot irri method last 5 years  ||  bl16_irri_methods       ----

# D21	What is the method of irrigation?
##1 Flood #2 Furrows #3	Drip #4	Sprinkler #5 Manual  #6	Hose # -888	Other, specify

D21_method <- rmtl_baseline2016 %>% 
  select(farmers_hh,hh_id, starts_with("D21") )
D21_method$D21_1[D21_method$D21_os_1== "BOREWEL"] <- 1
D21_method$D21_2[D21_method$D21_os_2== "BOREWELL"] <- 1

D21 <- D21_method %>%
  select(-c(D21_os_1, D21_os_2,"D21_12","D21_0","D21_os_0" )) %>%
  mutate(D21_7 = as.numeric(D21_7 )) %>% 
  pivot_longer(-c(farmers_hh, hh_id), 
               names_to = "plot_num", 
               values_to = "irri_method")

D21$plot_num <- str_replace(D21$plot_num, "D21_(\\d)$", "plot_0\\1")
D21$plot_num[D21$plot_num == "D21_10"] <- "plot_10" 

D21$irri_method[D21$irri_method %in% c(1,2)  ] <- "Flood" 
D21$irri_method[D21$irri_method == 3   ] <- "Drip" 
D21$irri_method[D21$irri_method == 4  ] <- "Sprinkler" 
D21$irri_method[D21$irri_method %in% c(5,6)  ] <- "Manual" 


D6_12_13_21=
  bl6_plotAcre %>% 
  left_join(bl_d12) %>% 
  left_join(bl13)%>% 
  left_join(D21)

D6_12_13_21$irri_source_5y[D6_12_13_21$hh_id=="106974" & is.na(D6_12_13_21$irri_source_5y) ] <- "Canal"
D6_12_13_21$irri_method[D6_12_13_21$irri_method<0 & !is.na(D6_12_13_21$irri_source_5y) ] <- "Flood"
D6_12_13_21$irri_method[is.na(D6_12_13_21$irri_method) ] <- "Rain"
D6_12_13_21$irri_method[D6_12_13_21$irri_method<0 ] <- "Rain"


# BL5y_plot_IRsource_IRmethod | BL5Y_IRsource_IRmethod..................... ----
BL5y_plot_IRsource_IRmethod = D6_12_13_21
BL5Y_IRsource_IRmethod = D6_12_13_21 %>% 
  group_by(hh_id) %>%
  mutate(
    ir_hh_method = case_when(
      any(irri_method == "Drip") ~ "Drip",
      any(irri_method == "Flood") ~ "Flood",
      any(irri_method == "Sprinkler") ~ "Sprinkler",
      any(irri_method == "Manual") ~ "Manual",
      TRUE ~ "Rain")
  ) %>%
  select(farmers_hh, hh_id, ir_hh_method) %>% 
  distinct() %>% 
  mutate(drip_use=ifelse(ir_hh_method=="Drip",1,0),
         ir_use = ifelse(ir_hh_method =="Rain",0,1))
  
write.csv(BL5y_plot_IRsource_IRmethod, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL5y_plot_IRsource_IRmethod.csv", row.names=FALSE)
write.csv(BL5Y_IRsource_IRmethod, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL5Y_IRsource_IRmethod.csv", row.names=FALSE)



# [D24] crops planted                  || bl_crop_plot_3s         ----
# What crops are planted on this plot? Mark all that apply (Perennial crops will be listed in 2 seasons)

d24 <- 
  rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "d24_"), -ends_with("_0") ) %>% select(-contains("os_")) %>% 
  mutate_at(vars(-hh_id), as.character) %>% 
  pivot_longer(-hh_id, names_to = "scp", values_to = "crop_code") %>% 
  mutate(crop_code=as.numeric(crop_code)) %>% 
  filter(crop_code>0) %>% 
  separate(scp, into = c("prefix1", "number1", "prefix2", "number2", "number3"), sep = "_") %>%
  select(-prefix2,-prefix1) %>% 
  unite("season_cropNUM_plot", number1:number3, sep = "_", remove = TRUE)


# Checking where there is an "other"
D24_os <- rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "D24_") )%>% 
  select(hh_id,contains("os"),
         -ends_with( c("11","12","13","14","15","16","17","_0" ))) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  mutate(hh_id=as.character(hh_id)) %>% 
  select(where(~ !(is.numeric(.) )))

# Adding "Other" to the appropriate column
D24_os1 <- rmtl_baseline2016 %>% 
  select(hh_id,starts_with( "D24_"),
         -ends_with( c("11","12","13","14","15","16","17","_0" ))) %>%  
  mutate(
    D24_1_Crop_1_1 = ifelse(D24_1_Crop_1_1 %in% c(-888, -666), D24_1_Crop_1_os_1, D24_1_Crop_1_1),
    D24_1_Crop_2_1 = ifelse(D24_1_Crop_2_1 %in% c(-888, -666), D24_1_Crop_2_os_1, D24_1_Crop_2_1),
    D24_1_Crop_4_1 = ifelse(D24_1_Crop_4_1 %in% c(-888, -666), D24_1_Crop_4_os_1, D24_1_Crop_4_1),
    D24_1_Crop_1_2 = ifelse(D24_1_Crop_1_2 %in% c(-888, -666), D24_1_Crop_1_os_2, D24_1_Crop_1_2),
    D24_1_Crop_1_3 = ifelse(D24_1_Crop_1_3 %in% c(-888, -666), D24_1_Crop_1_os_3, D24_1_Crop_1_3),
    D24_1_Crop_1_4 = ifelse(D24_1_Crop_1_4 %in% c(-888, -666), D24_1_Crop_1_os_4, D24_1_Crop_1_4),
    D24_1_Crop_1_5 = ifelse(D24_1_Crop_1_5 %in% c(-888, -666), D24_1_Crop_1_os_5, D24_1_Crop_1_5),
    D24_2_Crop_1_1 = ifelse(D24_2_Crop_1_1 %in% c(-888, -666), D24_2_Crop_1_os_1, D24_2_Crop_1_1),
    D24_2_Crop_2_1 = ifelse(D24_2_Crop_2_1 %in% c(-888, -666), D24_2_Crop_2_os_1, D24_2_Crop_2_1),
    D24_2_Crop_1_3 = ifelse(D24_2_Crop_1_3 %in% c(-888, -666), D24_2_Crop_1_os_3, D24_2_Crop_1_3),
    D24_3_Crop_1_1 = ifelse(D24_3_Crop_1_1 %in% c(-888, -666), D24_3_Crop_1_os_1, D24_3_Crop_1_1),
    D24_3_Crop_2_1 = ifelse(D24_3_Crop_2_1 %in% c(-888, -666), D24_3_Crop_2_os_1, D24_3_Crop_2_1),
    D24_3_Crop_3_1 = ifelse(D24_3_Crop_3_1 %in% c(-888, -666), D24_3_Crop_3_os_1, D24_3_Crop_3_1),
    D24_3_Crop_1_3 = ifelse(D24_3_Crop_1_3 %in% c(-888, -666), D24_3_Crop_1_os_3, D24_3_Crop_1_3),
    D24_3_Crop_1_4 = ifelse(D24_3_Crop_1_4 %in% c(-888, -666), D24_3_Crop_1_os_4, D24_3_Crop_1_4),
    D24_3_Crop_1_5 = ifelse(D24_3_Crop_1_5 %in% c(-888, -666), D24_3_Crop_1_os_5, D24_3_Crop_1_5)
  )


D24_os2 <- D24_os1 %>%  select(-contains("os") )%>% 
  mutate(across(-hh_id, as.character)) %>%  # Convert all columns (except hh_id) to character
  mutate(across(
    where(is.character), ~ ifelse(. %in% c("NA", "0","-666", "-888", "-777","-999", "-444"), NA, .) # Replace specific character values with NA
  )) %>%   pivot_longer(-hh_id, names_to = "plotID", values_to = "crop_bl")


D24_os2 %>% count(crop_bl)

D24_os3 <- D24_os2 %>%
  mutate(crop_bl_2 = case_when(
    crop_bl == "ULLAGADDI"  ~ "Other_Specify", 
    crop_bl == "THENGU" ~ "Coconut",                 # THENGU -> Coconut
    crop_bl == "SUNFLOWER" ~ "Sunflower",           # SUNFLOWER -> Sunflower
    crop_bl == "SUGARCANE" ~ "Sugarcane",           # SUGARCANE -> Sugarcane
    crop_bl == "SPICES" ~ "Spices",                 # SPICES -> Spices
    crop_bl == "SOUTE KAYI" ~ "Bitter Gourd",      # SOUTE KAYI -> Bitter Gourd
    crop_bl == "SHIBEHANU" ~ "Banana",              # SHIBEHANU -> Banana
    crop_bl == "SHIBEHANNU" ~ "Banana",             # SHIBEHANNU -> Banana
    crop_bl == "RAIN PROBLEM EMPTY LAND" ~ NA_character_,  # RAIN PROBLEM EMPTY LAND -> NA
    crop_bl == "PIRALA HANNU" ~ "Guava",            # PIRALA HANNU -> Guava
    crop_bl == "PERU" ~ "Guava",                    # PERU -> Guava
    crop_bl == "ONION" ~ "Onion",                   # ONION -> Onion
    crop_bl == "NP" ~ NA_character_,                # NP -> NA
    crop_bl == "NO CROPS IN THIS LAND" ~ NA_character_,  # NO CROPS IN THIS LAND -> NA
    crop_bl == "NO CROPS" ~ NA_character_,          # NO CROPS -> NA
    crop_bl == "NO CROP" ~ NA_character_,           # NO CROP -> NA
    crop_bl == "NO" ~ NA_character_,                # NO -> NA
    crop_bl == "MAYU" ~ "Jackfruit",                # MAYU -> Jackfruit
    crop_bl == "MANGO" ~ "Mango",                   # MANGO -> Mango
    crop_bl == "KUSUBI" ~ "Sorghum_jowar",         # KUSUBI -> Sorghum_jowar
    crop_bl == "KOTAMBIRI" ~ "Coriander",           # KOTAMBIRI -> Coriander
    crop_bl == "KOTAMBARE" ~ "Coriander",           # KOTAMBARE -> Coriander
    crop_bl == "KABBU" ~ "Pumpkin",                 # KABBU -> Pumpkin
    crop_bl == "GREENCHILLY" ~ "Green Chilly",      # GREENCHILLY -> Green Chilly
    crop_bl == "GOVINA JOLA" ~ "Sorghum_jowar",     # GOVINA JOLA -> Sorghum_jowar
    crop_bl == "EMPTY LAND RAIN PROBLEM" ~ NA_character_, # EMPTY LAND RAIN PROBLEM -> NA
    crop_bl == "COTTON" ~ "Cotton",                 # COTTON -> Cotton
    crop_bl == "COCONUTE" ~ "Coconut",              # COCONUTE -> Coconut
    crop_bl == "COCONUT" ~ "Coconut",               # COCONUT -> Coconut
    crop_bl == "CHIKKU" ~ "Chikoo",                 # CHIKKU -> Chikoo
    TRUE ~ crop_bl                                 # Default: Keep the original value if no match
  ))



D24 <- D24_os3%>% 
  filter(!is.na(crop_bl))

D24$season <- sub("^(D24_1)_.*", "rabi_2015_16", D24$plotID)
D24$season <- sub("^(D24_2)_.*", "kharif_2015", D24$season)
D24$season <- sub("^(D24_3)_.*", "rabi_2014_15", D24$season)

D24$crop_num <- sub("^.*_Crop_(\\d+)_.*", "crop\\1", D24$plotID)

D24$plotID <- str_replace(D24$plotID, "^.*_(\\d)$", "plot_0\\1")
D24$plotID <- str_replace(D24$plotID, "^.*_10$", "plot_10")

D24$plotID[D24$plotID == "D24_10"] <- "plot_10" 

D24[,1] %>% distinct() # A tibble: 1,729 × 1

D24[,1] %>% distinct() # A tibble: 1,723 × 1 

library(readr)
list_crop <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.csv")
View(list_crop)
crops <- list_crop %>% 
  select(crop_code, crop_name, crop_type, crop_common) %>% 
  mutate(crop_code = as.character(crop_code))


bl_crop_plot_3s= 
  D24 %>%rename(crop_code =crop_bl) %>% 
  left_join(crops) %>% mutate(
  crop_common = case_when(
    crop_code %in% c("Banana", "Jackfruit", "Mango", "Guava", "Bitter Gourd",
                     "Onion","Coconut","Pumpkin","Green Chilly","Chikoo") ~ "VegetablesANDFruits",  # Fruits and Vegetables
    crop_code == "Sorghum_jowar" ~ "Sorghum_jowar",         # Sorghum_jowar
    crop_code == "Sunflower" ~ "Sunflower",                 # Sunflower
    crop_code == "Sugarcane" ~ "Sugarcane",                 # Sugarcane
    crop_code == "Cotton" ~ "Cotton",                       # Cotton
    crop_code == "Spices" ~ "Other",                        # Spices -> Other
    crop_code == "Coriander" ~ "Other", 
    crop_code == "Other_Specify" ~ "Other",
    TRUE ~ crop_common                                    # Default: NA for all others
  )
) %>% mutate(
  crop_common = case_when(
    crop_name %in% c("Guava", "Jackfruit", "apaya","Papaya",
                     "Pineapple","Pomegranate","Sapota") ~ "VegetablesANDFruits",  # Fruits and Vegetables
    crop_name %in% c( "Tobacco", "Fig","Cocoa") ~ "Other",
    TRUE ~ crop_common                                
  )
)%>% mutate(crop_name=ifelse(is.na(crop_name),crop_code,crop_name)) %>% 
  mutate(
    crop_common = case_when(
      crop_common %in% c("Chillies","Vegetables") ~ "VegetablesANDFruits",
      TRUE ~ crop_common  )
    )           


library(writexl)
write_xlsx(bl_crop_plot_3s, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl_crop_plot_3s.xlsx")





# [D28]	Was the crop irrigated?        || bl28_irri_plot_season   ----

D28_1 <- # 2015-16 RABI  
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_1") ) %>% 
  select(farmers_hh: D28_1_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_1$plotID <- str_replace(D28_1$plotID, "D28_1_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_1$plotID[D28_1$plotID=="D28_1_irrigated_1_10"] <- "plot_10"
D28_1=D28_1 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% 
  mutate(season="rabi_2015_16") %>% ungroup()

D28_2 <- # 2015 KHARIF
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_2") ) %>% 
  select(farmers_hh: D28_2_irrigated_1_7)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_2$plotID <- str_replace(D28_2$plotID, "D28_2_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_2=D28_2 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% distinct() %>% mutate(season="kharif_2015") %>% 
  ungroup()

D28_3 <- # 2014-15 RABI
  rmtl_baseline2016 %>% select(farmers_hh, hh_id, starts_with("D28_3") ) %>% 
  select(farmers_hh: D28_3_irrigated_1_10)  %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "plotID", values_to = "irri_plot") %>% filter(!is.na(irri_plot))
D28_3$plotID <- str_replace(D28_3$plotID, "D28_3_irrigated_\\d_(\\d)$", "plot_0\\1")
D28_3$plotID[D28_3$plotID=="D28_3_irrigated_1_10"] <- "plot_10"

D28_3=D28_3 %>% 
  group_by(farmers_hh,hh_id,plotID) %>% 
  summarise(irri_plot=sum(irri_plot)) %>% 
  mutate(irri_plot=ifelse( irri_plot>0,1,0 )) %>% 
  distinct() %>% 
  mutate(season="rabi_2014_15") %>% ungroup()

bl28_irri_plot_season <- 
  rbind(D28_1,D28_2,D28_3) %>% 
  rename(irri_plot_3s =irri_plot )

rm(D28_1,D28_2,D28_3)

#### YIELD		         ----					
# [D30]	In what terms is the yield defined?	----

#| IGNORE D30 - IT SEEMS UNRILIBLE

      # 1 Quintals in total (the whole plot)
      # 2	Quintals/acre
      # 3	Quintals/gunta

# d30 <- 
#   rmtl_baseline2016 %>% 
#   select(hh_id, starts_with("D30"), -ends_with("_0") ) %>% 
#   pivot_longer(-hh_id, names_to = "scp", values_to = "yield_defined") %>% 
#   filter(yield_defined>0) %>% 
#   separate(scp, into = c("prefix1", "number1", "prefix2", "number2", "number3"), sep = "_") %>%
#   select(-prefix2,-prefix1) %>% 
#   unite("season_croplot_plot", number1:number3, sep = "_", remove = TRUE)
# 
# inner_join(d24,d29 ) %>% left_join(d30) %>% filter(is.na(yield_defined ))
# d_index <- inner_join(d24,d29 ) %>% left_join(d30)
# d_index$yield_defined <- ifelse(is.na(d_index$yield_defined),1,d_index$yield_defined)
# d_index %>% count(yield_defined)
# 
# d2930 <- 
#   left_join(d29,d30) %>% 
#   mutate(yield_defined=ifelse(is.na(yield_defined),1, yield_defined)) %>% 
#   left_join(bl6_plotAcre %>% select(-farmers_hh) )

# [D29]	What was the total yield? (Quintals)      || bl_crop_yield  ----


d29 <- 
  rmtl_baseline2016 %>% 
  select(hh_id, starts_with("D29"), -ends_with("_0") ) %>% 
  pivot_longer(-hh_id, names_to = "scp", values_to = "yield_bl") %>% 
  filter(yield_bl>0) %>% 
  separate(scp, into = c("prefix1", "number1", "prefix2", "number2", "number3"), sep = "_") %>%
  select(-prefix2,-prefix1) %>% 
  unite("season_croplot_plot", number1:number3, sep = "_", remove = TRUE)




library(tidyr)
library(stringr)

D29_a <- 
  rmtl_baseline2016 %>% 
  select(hh_id, starts_with("D29"), -ends_with("_0") ) %>% 
  pivot_longer(-c(hh_id), names_to="season_crop_plot", values_to = "yield") %>% 
  filter(!is.na(yield)) %>% 
  mutate(
    season = str_extract(season_crop_plot, "(?<=D29_)(\\d)(?=_)"),  # Extracts the season (1st digit)
    crop_num = str_extract(season_crop_plot, "(?<=D29_\\d_yield_)(\\d)(?=_)"),  # Extracts the crop number (2nd digit)
    plotID = str_extract(season_crop_plot, "(?<=D29_\\d_yield_\\d_)(\\d+)")  # Extracts the plotID (3rd digit)
  )
# D29_a %>% count(season)
# BL_2015_16_crop_IRsource_IRmethod %>% count(season)
D29_a$season[D29_a$season==1] <- "rabi_2015_16"
D29_a$season[D29_a$season==2] <- "kharif_2015"
D29_a$season[D29_a$season==3] <- "rabi_2014_15"
# D29_a %>% count(crop_num )
D29_a$crop_num <- paste0("crop", D29_a$crop_num)
# D29_a %>% count(plotID)
D29_a$plotID <- ifelse(nchar(D29_a$plotID) == 1, paste0("plot_0", D29_a$plotID), paste0("plot_", D29_a$plotID))

  
library(readr)
bl6_plotAcre <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl6_plotAcre.csv")

BL_plotAcre <- bl6_plotAcre %>% select(-farmers_hh) %>% rename(plotID =plot_num )  # A tibble: 3,455

BL_plot.Crop.Yield <- bl_crop_plot_3s %>% 
  select(hh_id ,plotID, season,crop_num , crop_name,crop_common) %>% 
  right_join(D29_a %>% select(hh_id ,plotID,crop_num , season,yield))


BL_plotCrop <- bl_crop_plot_3s %>% 
  select(hh_id ,plotID, season,crop_num , crop_name,crop_common) %>% 
  right_join(D29_a %>% select(hh_id ,plotID,crop_num , season,yield))

BL_plotCrop$Season[BL_plotCrop$crop_name=="Sorghum_jowar"] <- "rabi"
BL_plotCrop$Season[BL_plotCrop$crop_name=="Bengal_gram"] <- "rabi"
BL_plotCrop$Season[BL_plotCrop$crop_name=="Greengram"] <- "kharif"
BL_plotCrop$Season[BL_plotCrop$crop_name=="Toor"] <- "kharif"
BL_plotCrop$Season[BL_plotCrop$crop_name=="Maize"] <- "kharif"



yield_per_acre_2015 <- 
  BL_plotCrop %>% select(hh_id, yield ,season,plotID  ) %>% 
  filter (yield >0) %>% 
  left_join(BL_plotAcre) %>%  filter(plot_acre>0) %>% 
   mutate(season = sub("_.*", "", season)) %>% 
  group_by(hh_id, season) %>% 
  summarise(kg_crop=sum(yield,na.rm = T)*100,
            acres=sum(plot_acre,na.rm = T),.groups = "drop" ) %>% 
  mutate(kg_per_acre= kg_crop/acres) 

yield_per_acre_2015_99 <- quantile(yield_per_acre_2015$kg_per_acre, 0.99)

  
yield_per_acre_2015 %>% group_by(season) %>% 
  filter(kg_per_acre<yield_per_acre_2015_99) %>% 
  summarise(mean(kg_per_acre,na.rm=T))


  
  
  
  
  
  




# CROP PLOT IR 2015-2016 | BL_2015_16_crop_IRsource_IRmethod.............. ----

bl28_irri_plot_season # A tibble: 6,332 × 5
bl_crop_plot_3s       # A tibble: 7,268 × 8

bl_crop_plot_3s %>% left_join (bl28_irri_plot_season) # A tibble: 7,268 × 10
df[,1] %>% distinct() #  1,723 HH crop data only
df %>% filter(!is.na(irri_plot_3s)) %>% 
  select(hh_id) %>%  distinct() # 1,681 HH crop and ir data

library(readr)
BL5y_plot_IRsource_IRmethod <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL5y_plot_IRsource_IRmethod.csv")

BL_2015_16_crop_IRsource_IRmethod <- 
  bl_crop_plot_3s %>% 
  left_join(bl28_irri_plot_season) %>% 
  left_join(bl6_plotAcre %>% select(hh_id, plot_num, plot_acre ) ) %>% 
  left_join(BL5y_plot_IRsource_IRmethod %>% 
              select(hh_id, plot_num,irri_method) ) 

write.csv(BL_2015_16_crop_IRsource_IRmethod, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL_2015_16_crop_IRsource_IRmethod.csv", row.names=FALSE)


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







library(readxl)
course_udl_analysis_4_10_23 <- read_excel("C:/Users/Dan/Downloads/course udl analysis 4.10.23.xlsx", 
                                          sheet = "שיפוט מהימנות")
course_udl_analysis=course_udl_analysis_4_10_23 %>%
  select(YAEL,DAN) %>%  
  filter( !is.na(YAEL))


# Chi-Square Test
# Create a contingency table
contingency_table <- table(course_udl_analysis$YAEL, course_udl_analysis$DAN)
# Perform the Chi-Square Test
chi_test <- chisq.test(contingency_table)
print(chi_test)


#  Cohen's Kappa
library(irr)
kappa_result <- kappa2(course_udl_analysis)
# Display the results
print(kappa_result)


