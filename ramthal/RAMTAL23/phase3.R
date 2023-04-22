#----
getwd()
library(haven)
library(tidyverse) 
# remove columns ----
# remove columns that contain only NAs
x %>% 
  select(-which(sapply(., function(x) all(is.na(x)))))
# remove columns that contain only empty cells
x %>%
  mutate_all(as.character) %>%  # Convert all columns to character type
  select_if(~ !all(is.null(.)) & !all(. == ""))


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

# asdsa
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
# IRRIGATION   ----		
#       L7 rank irrigation source ====
# What irrigation source are you dependent on? (Rank according to the degree of importance)
# wide df
L7 <- jan %>%select(id,starts_with("l7_"))

# long df
L7 <- jan %>%select(id,starts_with("l7_"))%>%  
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -id, 
    names_to = "L7_source_rank",
    values_to = "L7_source_type")
# CROP         ----
L39 <- jan  %>% select(id,matches("^l39"))

# L39 What crop are planted on this plot? ====
# Perennial/biseasonal crops will be listed in Kharif




# L39b What crops were planted in [Year]? ====
# [Crop-Year] 2020/2019/2018

# Crops in serial numbers
L39b <-  select(L39 ,c(id, l39b_y_20,l39b_y_19,l39b_y_18))

#Crops by name
L39b=L39 %>%
  select(id,
         l39b_y_20_crop_name_1:l39b_y_20_crop_name_6,
         l39b_y_20_crop_1_other:l39b_y_20_crop_4_other,
         
         l39b_y_19_crop_name_1:l39b_y_19_crop_name_5,
         l39b_y_19_crop_1_other:l39b_y_19_crop_4_other,
         
         l39b_y_18_crop_name_1:l39b_y_18_crop_name_6,
         l39b_y_18_crop_1_other:l39b_y_18_crop_4_other)%>%  
  pivot_longer(
    cols = -id, 
    names_to = "L39b_year",
    values_to = "L39b_crop")

         

         l39_prev_kha_1:l39_prev_kha_4,
         l39_prev_kha_10_1,l39_prev_kha_9_1,l39_prev_kha_11_1,l39_prev_kha_2_1
         



# L39a [Crop-Plot-Season] 
CR3 <- Adt3 %>% filter(id %in% c(1000709 ,39305302 ,39305501)) %>% 
  select(id,list2016, survy2022,starts_with("l39_") ) %>% 
  
  
  CR3 <- Adt3 %>% 
  select(id,list2016, survy2022,mm5,# l39_ _ _1 - l39_ _ _9 l39_new_kha_1
         l39_prev_kha_1,l39_prev_rab_1,l39_prev_sum_1,
         l39_prev_kha_2,l39_prev_rab_2,l39_prev_sum_2,
         l39_prev_kha_3,l39_prev_rab_3,l39_prev_sum_3,
         l39_prev_kha_4,l39_prev_rab_4,l39_prev_sum_4,
         l39_prev_kha_5,l39_prev_rab_5,l39_prev_sum_5,
         l39_prev_kha_6,l39_prev_rab_6,l39_prev_sum_6,
         l39_prev_kha_7,l39_prev_rab_7,l39_prev_sum_7,
         l39_prev_kha_8,l39_prev_rab_8,l39_prev_sum_8,
         l39_prev_kha_9,l39_prev_rab_9,l39_prev_sum_9,
         
         # L39b	What crops were planted in [ Crop-Year ]?
         l39b_y_20,l39b_y_19,l39b_y_18)

V <- CR3 %>%
  filter(mm5==0,l39_prev_kha_1=="",l39_prev_rab_1=="",l39_prev_sum_1=="",
         l39_prev_kha_2=="",l39_prev_rab_2=="",l39_prev_sum_2=="")

#       L48	How often was the crop irrigated manually? ====

# L48a		What is the method of irrigation?
#          L39a Crop-Plot-Season
#          L39b Crop

IM3 <- Adt3 %>%  #l48_prev/new_season*3_plot*9_crop*3
  select(id,Im_in_out,in_out,mm5,
         l48_prev_kha_1_1,l48_prev_kha_1_2,l48_prev_kha_1_3,
         l48_prev_kha_2_1,l48_prev_kha_2_2,l48_prev_kha_2_3,
         # l48_y_year18/19/20_crop*7/6/6
         l48_y_18_1,l48_y_18_2,
         l48_y_19_1,l48_y_19_2,
         l48_y_20_1,l48_y_20_2)

#       L39 What crop # L48	How often # L48a	What method # ====
V <- Adt3 %>%  
  select(id,Im_in_out,in_out,mm5,
         l39_prev_kha_1,# L39 What crop
         l48_prev_kha_1_1,l48_prev_kha_1_2,  # L48	How often
         l48a_prev_kha_1_1,l48a_prev_kha_1_2,# L48a	What method
         
         l39b_y_18,l39b_y_19,l39b_y_20,# L39 What crop
         l48_y_18_1,# L48	How often
         l48_y_19_1,# L48	How often
         l48_y_20_1,# L48	How often
         l48a_y_18_1,# L48a	What method
         l48a_y_19_1,# L48a	What method
         l48a_y_20_1)# L48a	What method



#       L41 Have you used this cultivation method?  [Crop level] ====

L41_3 <- Adt3 %>%
  select(id,Im_in_out,in_out,mm5,
         l41_1_1, # Mulching
         l41_2_1, # Trellising הדליה
         l41_3_1, # Furrow תלם.תעלות
         l41_4_1, # Line sowing
         l41_5_1, # Fertigation
         
         l41_1_8) # crop 8


#       ??? L41a common cultivation method ====
#Which are the most common cultivation method you used 3 years before 2021/2022? (2018/2019)
#    single question

Adt3 %>%
  select(starts_with("l41")) # 
# Mulching
# Trellesing
# Furrow
# Line sowing
# Fertigation  



# PLOTS        ----

#       L_plot_status : What the status of the plot? ====
L_plot_status=jan %>% select(id,contains("plot_status") ) %>%  
  pivot_longer(
    cols = `l_plot_status_1`:`l_plot_status_10`, 
    names_to = "plot",
    values_to = "plot_status")
#  filter(!is.na(plot_status) ) 

L_plot_status$plot <- gsub('l_plot_status_', 'plot_', L_plot_status$plot)


#       L13 :		Why does your household no longer own the plot? ====
#==   NOTE: in case of "5.Partial Sold"
#==   we have the "area_share" vars (l13_area_share_acre_ , l13_area_share_guntha_)

L13 <- select(jan, id, starts_with("l13"))

L13 <- pivot_longer(L13,
                        cols = -id,
                        names_to = c(".value", "plot"),
                        names_pattern = "^(l13|l13_other|l13_area_share_acre|l13_area_share_guntha)_(\\d+)$"
)
L13$plot <- paste("plot_", L13$plot, sep="")

#       L20 Current Operation Status + acre_guntha          ====
#         (multiple choice)

L20 = jan %>% select(id,starts_with("l20") ) 

# Removing the binary "choice" columns
df <-select(L20, -matches("_\\d+_\\d+$"))

L20 <- df %>%
  pivot_longer(-id,
               names_to = c(".value", "plot"),
               names_pattern = "(.*)_(.*)"
  )
L20$plot <- paste0("plot_", L20$plot)











#       L25 When did you start cultivating this plot?       ====

L25 <- select(jan, id, starts_with("l25"))

for (i in 1:10) { # Create new columns for each pair of month and year columns
  month_col <- paste0("l25_month_", i)
  year_col <- paste0("l25_year_", i)
  new_col <- paste(L25[[year_col]], L25[[month_col]], sep = "_")
  L25 <- cbind(L25, new_col)
  colnames(L25)[ncol(L25)] <- paste0("L25_year_month_", i)
} 
L25 <- select(L25,'id', grep('^L25', colnames(L25))) %>% 
  pivot_longer(
    cols = -id, 
    names_to = "plot",
    values_to = "L25_start_cult")

L25$plot <- gsub('L25_year_month_', 'plot_', L25$plot)








#       L28 Why was it left fallow?                         ====
L28 = jan %>% select(id,starts_with("l28") ) %>%
  select(-matches("_\\d+_\\d+$"))

L28 <- pivot_longer(L28,
                    cols = -id,
                    names_to = c(".value", "plot"),
                    names_pattern = "^(l28|l28_other)_(\\d+)$"
)
L28$plot <- paste("plot_", L28$plot, sep="")

colnames(L28)[3] <- "l28_why_fallow"
colnames(L28)[4] <- "l28_why_fallow_other"

#   # NEW plots     ----
#       L29 Have you gained/received new lands since 2018? XX  ====
L29 <- jan %>% select(id,starts_with("l29") ) 

#       L30 How many NEW plots?                                ====
L30 <- jan %>% select(id,starts_with("l30") ) 
colnames(L30)[2] <- "l30_plotNEW_since_2018"


#       L31 [ Village ]                                     ====  
L31 <- jan %>% select(id,starts_with("l31") ) 
L31 <- pivot_longer(L31,
                    cols = -id,
                    names_to = c(".value", "plot"),
                    names_pattern = "^(l31|l31_other)_(\\d+)$"
)
L31$plot <- paste("new_plot_", L31$plot, sep="")

colnames(L31)[3] <- "l31_plotNEW_village"
colnames(L31)[4] <- "l31_plotNEW_village_other"
#       L31 [ Survey nu ] [ Hissa number ] [ acre ]    XX   ====
#       L32 How did you come to own this land? select one   ====
L32 <- jan %>% select(id,matches("^l32"))

L32 <- jan %>% select(id,matches("^l32"))%>%  
  pivot_longer(
    cols = -id, 
    names_to = "plot",
    values_to = "l32_plotNEW_How_own_land")

L32$plot <- gsub('l32_', 'new_plot_', L32$plot)

#   # NEW plots   BIND   ----  

L_NEW_plots <- full_join(L30,L31) %>% full_join(L32)

