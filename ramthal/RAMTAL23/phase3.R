#----
getwd()
library(haven)
library(tidyverse) 

# ramtal_groups ----
YR_Ramthal_Data_Entry_2_stata13 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata13.dta")

ramtal_groups=YR_Ramthal_Data_Entry_2_stata13[ ,c("id","in1_out0","layer","distance_km","around_boundary","south1_north0")]
rm(YR_Ramthal_Data_Entry_2_stata13) #heavy file- better to remove it

ramtal_groups$in_out = ifelse(ramtal_groups$in1_out0 ==1,"Inside","Outside")
ramtal_groups$south_north = ifelse(ramtal_groups$south1_north0==1,"Southern","Northern")


jan #  ----
jan_18_2023 <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/phase_3/Anonymized_Dataset_raw_18 Jan 2023.dta")
jan_21_2023 <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/phase_3/Anonymized_Dataset_raw_21 Jan 2023.dta")
jan= rbind(jan_18_2023,jan_21_2023)
rm(jan_18_2023,jan_21_2023)

jan=jan%>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)

# crosstab ----
crosstab_rmtl23= jan [,c("id","list2016","survy2022")] %>%
  mutate(list2016=case_when(is.na(list2016) ~ survy2022,TRUE ~ list2016)) # NAs

library(sjPlot)
sjPlot::tab_xtab(var.row = crosstab$list2016 , 
                 var.col = crosstab$survy2022, 
                 title = "HH inside & outside the project\n reality(list 2016) Vs. self-reporting (survey2022)", 
                 show.row.prc = TRUE)
#--------------------------
#----     WATER        ----
#--------------------------
# participation                                         ------
#mm2 = #1 Ramrhal #2 Krishi Honda(Farm pond) #3 PMKSY #4 Ganga kalyana #5 non

participation=
  jan %>%select(id,
                  mm1, #Have you heard of any government irrigation project in your area?
                starts_with("mm2"), #Is your land coming under such a government project? if yes - which?
                  mm4, #Has any infrastructure (e.g. piping) been installed in your land at any time in the past?
                  mm5 , #Have you ever (even once) made any use of this water for irrigating your land?
                  mm9, #How many farmers are there between you and the valve/pipeline?
                  mm10#Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?
                ) %>% 
  left_join(ramtal_groups)
write.csv(participation,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/participation.csv')
rm(participation)


# land_value                                            ----
land_value=
  jan %>% select(
    id,mm5,#Have you ever (even once) made any use of this water for irrigating your land?
         #non ramthal
         m11a	,#	 LEASE your land, how much? (Rs. per Acre per year)
         m11b,#  estimate  LEASE  inside
         m12,# sell  your land, how much? (Rs. per Acre)
         m13,#  estimate  BUY  inside
         
         # ramthal
         m14a,#	 LEASE your land, how much? (Rs. per Acre per year)
         m14b, #  estimate  LEASE  inside
         m17,# sell  your land, how much? (Rs. per Acre)
         m18,#  estimate  BUY  outside
         #  non ramthal +ramthal
         m19) %>%  #spent by the government and the company per acre
  left_join(ramtal_groups)
write.csv(land_value,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/land_value.csv')
rm(land_value)


# use_supply_water                                      ----
#UW Use of Water from the Project
use_supply_water=
  jan %>%select(id,mm5,
                mw2,# Have you used it in [ _____ ]?
                mw1a,# If Yes, in which year did you first make use of the water? 
                mw1b,#In what season did you use it in that year?
                mw1c,#If No, Why?
                starts_with("mw1c"),# 1/0 multiple choice
                mw4, # Are you still making use of the water from the project to irrigate your land?
                mw4a,# If Yes, in what season?
                mw4b,#If No or Sometimes- What was the last year you use of the water?
                mw5,# How many years in total did you ever make use of the water for irrigation during Kharif?
                mw6,#How many years in total did you ever make use of the water for irrigation during Rabi?
                starts_with("m20"),# Can you indicate in which years and seasons you ever make use of the water for irrigation?
                starts_with("mw7"),# 11 choices X 6 Kharif, Kharif when you did NOT irrigate, what are the reasons? 
                starts_with("mw8"),# 10 choices X 6 Kharif. Kharif.seasons that you DID irrigate, did you notice any effect on your crop?
                starts_with("mw9"),# 11 choices X 6  In Rabi, when you did NOT irrigate, what are the reasons? 
                starts_with("mw10"),#10 choices X 6  In Rabi seasons that you DID irrigate, did you notice any effect on your crop?
                m10,# Of all the people who are covered by the project, how many still make use of it? (in percent)
                mw12,# Typically, in your experience, when water is provided in a particular year, in which month does it start?
                mw13,# Typically, in your experience, when water is provided in a particular year, in which month does it end for the year?
                mw14) %>% # Typically, in your experience, during the period water is provided, how often is it provided?
  left_join(ramtal_groups)
write.csv(use_supply_water,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/use_supply_water.csv')
#rm(use_supply_water)

# damage_irri_sys_water                                 ----
#Damaged irrigation system
damage_irri_sys_water=
  jan %>%
  mutate(m35b_yearS=(m35b_month/12)+m35b_year) %>% # How long has the main pipes been damaged?
  select(id,mm5,
         m35,  # What is the status of the main pipe coming into your land ?
         starts_with("m35a"),# What caused the damage?
         m35b_yearS, # How long has the main pipes been damaged?
         m35c,   # What is the status of the laterals?
         m36,    # did you contact someone to fix ?
         m37)%>% # did they help you and fix it?
  left_join(ramtal_groups)
write.csv(damage_irri_sys_water,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/damage_irri_sys_water.csv')
rm(damage_irri_sys_water)


# sec_5_6_7_water = Recommendations + Trainings + WUA   ----
# Not all questions are here
sec_5_6_7_water=
  jan %>%select(id,mm5,
                mw34, #Did you ever receive any advise or recommendations from the irrigation company?
                starts_with("m34"),# If Yes, Did you follow the irrigation company's recommendations? # If no, why?
                starts_with("m50a"), # What prevents you from following the same practices like in the demo plot?
                m52,#	Have you attended any of the trainings ?
                m54,#	How many trainings have you attended?
                m56,#	Did you find them to be informative?
                m62b,#Are you also benefiting and increasing your income?
                m62c #What is preventing you from doing the same?
                ) %>% 
  left_join(ramtal_groups)
write.csv(sec_5_6_7_water,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/sec_5_6_7_water.csv')
rm(sec_5_6_7_water)



# Willingness to pay+Maintenance                        ----
pay_maintenance_water=
  jan %>% 
#ramthal non ramthal
  select(id,mm5,
         m67, # Do you pay for the water you receive?
         m68, # Will you be willing to pay for the water you receive?
#ramthal
         m69,# Do you pay for maintenance of the drip irrigation infrastructure?
         m70,# Will you be willing to pay for maintenance of the drip irrigation infrastructure?
         starts_with("m72"),# What do you do with the collateral laterals between seasons? 
         starts_with("m73"),# How do you plow the soil? (since the drip irrigation infrastructure was installed)
         m74,#Do you allow farm animals (e.g. cows, buffaloes) to graze in the land in which the drip is installed?
         m75_f) %>%  #If yes, which other crops would you plant? (Suppose the infrastructure was working well)
  left_join(ramtal_groups)
write.csv(pay_maintenance_water,'C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/dt4py/pay_maintenance_water.csv')
rm(pay_maintenance_water)




#==================| David's thing |============================================


#==================| CULTIVTION |============================================

# PLOTS        ----
#   What the status of the plot?
L=jan %>% select(id,contains("plot_status") ) 

pivot_longer(
    cols = `l_plot_status_1`:`l_plot_status_10`, 
    names_to = "plot_num",
    values_to = "plot_status") %>%
  filter(!is.na(plot_status) ) 

#   L13		Why does your household no longer own the plot?
L13 = jan %>% select(id,starts_with("l13") ) 

#   L20		Current Operation Status (multiple choice)
L20 = jan %>% select(id,starts_with("l20") ) 


#   L25		When did you start cultivating this plot? 
L25 = jan %>% select(id,list2016, survy2022,starts_with("l25") ) %>% 
  select(where(~!all(is.na(.x))))

#   L28		Why was it left fallow?
L28 = jan %>% select(id,list2016, survy2022,starts_with("l28") ) %>% 
  select(where(~!all(is.na(.x))))

--------


#   L29		Have you gained/received new lands since 2018?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l29") ) %>% 
  select(where(~!all(is.na(.x))))

#   L30		How many NEW plots?
L1 <- Adt3 %>% select(id,list2016, survy2022,starts_with("l30") ) %>% 
  select(where(~!all(is.na(.x))))
#   L31		[ Village ] [ Survey nu ] [ Hissa number ] [ acre ]  
L1 <- Adt3 %>% select(id,starts_with("l31") ) %>% 
  select(where(~!all(is.na(.x))))
#   L32		 How did you come to own this land? select one 
L1 <- Adt3 %>% select(id,starts_with("l32") ) %>% 
  select(where(~!all(is.na(.x))))




