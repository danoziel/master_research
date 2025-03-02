# Interviews with Ramtal farmers with Divakar June 2023

library(dplyr)
library(summarytools )

library(haven)
library(readxl)

# DF [rmtl_2024] ----
rmtl_interviews_june24 <- 
  read_excel(
    "C:/Users/Dan/OneDrive - mail.tau.ac.il/
    Ramthal Data/rmtl_interviews_june24.xlsx", 
              sheet = "clean_df")

ro_dt <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/
                    rmtl_interviews_june24.xlsx", 
                    sheet = "ro_dt", col_names = FALSE)
ro_dt=ro_dt %>% rename(UID=`...2`)


rmtl_2024 <- 
  rmtl_interviews_june24 %>% 
  mutate(hh_id=as.numeric(UID),
         drip_1st_yr=as.numeric(drip_1st_yr),
         drip_1use_2useOwmDrip =as.numeric(drip_1use_2useOwmDrip )
         ) %>% 
  select(hh_id,everything()) %>% 
  select(-c(date,UID, comments,stage  ))

rm( rmtl_interviews_june24, ro_dt)

rmtl_2024_villages <- rmtl_2024 %>% count(village)

names(rmtl_2024)


# library(summarytools )----

library(summarytools )
freq(rmtl_2024$drip_trust, plain.ascii = FALSE,cumul = T, style = "rmarkdown")
#cran.r-project.org/web/packages/summarytools/vignettes/introduction.html


#____[4] "drip_1use_2useOwmDrip" ----
#    farmer use the drip at least once since installation ----
# This includes only farmers who are connected to the Ramthel infrastructure

# rmtl_2024
freq(rmtl_2024$drip_1use_2useOwmDrip, cumul = FALSE)

rmtl_2024  %>% filter(drip_1use_2useOwmDrip <=1) %>% 
  count(drip_1use_2useOwmDrip) %>% mutate(N=sum(n),pct=n/N)


# rmtl_srvy22
rmtl_In %>% filter(mm4==1) %>% count(drip_use) %>% mutate(N=sum(n),pct=n/N)

rmtl_In %>% filter(mm4==1, !a5 %in%c(
    "amaravathi","chinnapur","hungund" ,"hemavadagi","konnur", "marol","nidasanur","revadihal","thumba","yadahalli" ),
    ) %>% 
  count(drip_use) %>% mutate(N=sum(n),pct=n/N)


# intr 2024 vs survey 2022
q4 <- 
  rmtl_2024 %>% select(village,hh_id ,drip_1use_2useOwmDrip) %>%
  left_join(rmtl_In %>% select(hh_id, drip_use) ) %>%
  rename(intr2024=drip_1use_2useOwmDrip,srvy2022= drip_use) %>% 
  filter(intr2024<=1, srvy2022<=1 ) %>% 
  select(intr2024, srvy2022)

q4 %>% count(srvy2022) %>% mutate(prop = prop.table(n))

cor.test(q4$intr2024, q4$srvy2022, method = "pearson")



              





#______[14] "irri_jain_flood"  [12] "irri_source_NO_ramthal_system"  ----

freq(rmtl_2024$irri_jain_flood, cumul = FALSE) 
# 43.70
freq(rmtl_2024$irri_source_NO_ramthal_system, cumul = FALSE) 
# borewell 14.39
# Canal 7.92
# pond  1.44

# Farmers24 flood their fields using the Ramthal system  .....................

# Farmers22 using the Ramthal system as water source  ........................
a_source_irri %>% 
  right_join(rmtl_In) %>% 
  filter(mm4 == 1) %>% 
  count(source)  %>%
  mutate(N=sum(n), n/N)  # 0.463


# Farmers22_24  ..............................................................
q14=
  a_source_irri  %>%
  right_join(rmtl_2024 %>% select(hh_id,irri_jain_flood )) %>% 
  filter(!is.na(irri_jain_flood), !is.na(source_type)
         ) %>% 
  mutate(source22=ifelse(source == "gov_source",1,0 )) %>% 
  mutate(source24=as.numeric(irri_jain_flood))


# COR TEST ---
cor.test(q4$intr2024, q4$srvy2022, method = "pearson")
cor.test(q14$source22, q14$source24, method = "pearson")

# into a data 
df <- q4
df <- q14
  
# Pearson Correlation
pearson_corr <- cor(df$source22, df$source24, method = "pearson")
cat("Pearson Correlation:", pearson_corr, "\n")
  








#__________[26] "tap_status_1damged_0not"  ----
freq(rmtl_2024$tap_status_1damged_0not, cumul = FALSE)
N=149-16 =133

#survey 2022
# [m35] What is the status of the main pipe coming into your land ?
# [m35c] What is the status of the laterals?
m35 <- rmtl_srvy22 %>% select(farmers_hh,hh_id, starts_with("m35"))
#| m35 %>% filter(!is.na(m35c)) %>% count(m35c) %>% mutate(pct= n/sum(n))
#
m35 %>% filter(!is.na(m35)) %>% count(m35) %>% 
  mutate(N=sum(n), pct= n/N)
N=677

# Farmers22_24
m35  %>%
  right_join(rmtl_2024 %>% select(hh_id,tap_status_1damged_0not )) %>% 
  filter(!is.na(tap_status_1damged_0not), !is.na(m35)) %>% 
  count(m35 ) %>% 
  mutate(N=sum(n), n/N)

# [4]+[25]
# [4] "drip_1use_2useOwmDrip" 
# [25] tap_status_1damged_0not

rmtl_2024 %>% 
  filter(drip_1use_2useOwmDrip %in% c(0,1),
         !is.na(tap_status_1damged_0not)) %>%  
  count(drip_1use_2useOwmDrip,
        tap_status_1damged_0not) %>% mutate(N=sum(n),pct=n/N)

### Figure 301 |  first bar
rmtl_2024 %>%
  filter(drip_1use_2useOwmDrip %in% c(0,1),!is.na(irri_jain_flood)) %>%  
  count(drip_1use_2useOwmDrip,irri_jain_flood) %>% mutate(N=sum(n),pct=n/N)





# why not use  ----------------------------------------------------------------

# SAMPLE 2024 [30] "drip_why_not_use12"  ......................................
#| freq(rmtl_2024$drip_why_not_use1, cumul = FALSE)
#| freq(rmtl_2024$drip_why_not_use2, cumul = FALSE)
# freq(rmtl_2024$drip_why_not_use12, cumul = FALSE)

Q30=rmtl_2024 %>% 
  select(1,drip_why_not_use1,drip_why_not_use2,drip_1use_2useOwmDrip) 

no1=Q30 %>% filter(!is.na(drip_why_not_use1)) %>% 
  filter(drip_why_not_use1 != "damaged_by_farmers") %>% 
  count (drip_why_not_use1) %>% 
  rename(why_not_use_drip=drip_why_not_use1,n1=n)

no2=Q30 %>% filter(!is.na(drip_why_not_use2))  %>% 
  filter(drip_why_not_use2 != "damaged_by_farmers") %>% 
  count (drip_why_not_use2) %>% 
  rename(why_not_use_drip=drip_why_not_use2,n2=n)

Qq30=full_join(no1, no2) %>%
  mutate(n2 = ifelse(is.na(n2), 0, n2)) 

# water_supply
Qq30 %>% filter(why_not_use_drip !="damaged_by_farmers") %>% 
  group_by(why_not_use_drip) %>% 
  summarise(n1=sum(n1),n2=sum(n2)) %>% 
  mutate(n=n1+n2, N=sum(n), pct=n/N*100
         ) %>% 
  select(why_not_use_drip,n,N,pct ) %>% 
  mutate_at(4,round) %>% 
  kable() %>% kable_minimal()






#  SURVEY 2023  [mw1c] If No, Why?  ...........................................
# also Qs [mw7 mw8] 
rmtl_srvy22 %>% 
  select(hh_id,
         contains("mw1c"),-mw1c_other,-mw1c__888)%>% filter(mw1c != "") %>% 
  select(-mw1c ) %>% 
  pivot_longer(-hh_id,names_to = "ans",values_to = "value") %>% 
  group_by(ans) %>% summarise(pct_ans=mean(value ))

#### Adjustment to the answers of Q30 of 2024 questionnaire

## damaged_by_farmers
# mw1c_1	The main piping was never functional
# mw1c_3 The laterals in my field was damaged
# mw1c_5	I wanted to irrigate, but other farmers took all the water

## else
# mw1c_2 The laterals was never installed in my field
# mw1c_4 Rainfall was sufficient

## water_issues
# mw1c_6	Water was supplied only after I already sowed a rainfed crop
# mw1c_7	Water was not supplied when needed
# mw1c_8	I did not know when water was supplied
# mw1c_10	Water supply is unpredictable I cant count on it

mw1c_NEW <- 
  rmtl_srvy22 %>% 
  select(hh_id,
         contains("mw1c"),-mw1c_other,-mw1c__888)%>% filter(mw1c != "") %>% 
  select(-mw1c ) %>% 
  mutate(Damaged_by_farmers= mw1c_1+ mw1c_3 + mw1c_5,
         Water_issues =mw1c_6+mw1c_7+mw1c_8+mw1c_10,
         Else = mw1c_2 + mw1c_4
         ) %>% 
  select(hh_id,Damaged_by_farmers,Water_issues,Else) %>% 
  pivot_longer(-hh_id,names_to = "ans",values_to = "pct") %>% 
  mutate(pct=ifelse(pct>0,1,0))

mw1c_NEW %>% 
  group_by(ans) %>% summarise(pct_ans=mean(pct ))

# SAMPLE 2024 IN 2023  ........................................................  

rmtl_2024 %>% select(hh_id,drip_why_not_use12 )%>% 
               filter(!is.na(drip_why_not_use12)) %>% 
      left_join(mw1c_NEW)%>% 
  filter(!is.na(ans)) %>% # select(hh_id) %>% distinct() # N=41
  group_by(ans) %>% summarise(pct_ans=mean(pct ))



#  [26] "tap_cut_by_" =========================================================

# SAMPLE 2024 [26]What caused the damage to the tap?............
freq(rmtl_2024$tap_cut_by_, cumul = FALSE)
N=149-45 = 104

# SURVEY 2023 [m35a] What caused the damage?  ................................
# m35a: What caused the damage?	
rmtl_srvy22 %>% 
  select(farmers_hh,hh_id,m35a_1:m35a_7 ) %>% 
  pivot_longer(-c(farmers_hh,hh_id), names_to = "ans",values_to = "yn") %>%
  filter(!is.na(yn) ,farmers_hh=="inside_ramthal"
         ) %>% # select(hh_id) %>% distinct()
  group_by(ans) %>% summarise(pct=mean(yn))

## farmers ## self
#m35a_5	Other Farmers 

## shepherd
#m35a_1 Animals that graze in the field 

## tractor
#m35a_3	Machinery like tractors or threshers 
#m35a_6	Damaged during operation 

#m35a_2	Rodents #m35a_4	Thieves #m35a_7	Company 

m35a <- rmtl_srvy22 %>% 
  filter(farmers_hh=="inside_ramthal") %>% 
  select(farmers_hh,hh_id,m35a_1:m35a_7 ) %>% 
  mutate(farmers=m35a_5,shepherd=m35a_1,tractor=m35a_3) %>% 
  select(hh_id,farmers,shepherd,tractor) %>% 
  pivot_longer(-hh_id, names_to = "ans",values_to = "yn") %>%
  filter(!is.na(yn)) %>% 
  mutate(yn=ifelse(yn>0,1,0)) #%>% select(hh_id) %>% distinct()
m35a %>% group_by(ans) %>% summarise(n(),pct=mean(yn))

# SAMPLE 2024 IN 2023  ........................................................

m35a %>% 
  left_join(rmtl_2024 %>% select(hh_id,tap_cut_by_) ) %>% 
  filter(!is.na(tap_cut_by_)) %>%
  group_by(ans) %>% summarise(N=n(),pct=mean(yn))

#  [29] "trend_cut_why"  ====================================================
# SAMPLE 2024
freq(rmtl_2024$trend_cut_why, cumul = FALSE)







#  [28] when the "first_to_cut"  -=============================================
freq(rmtl_2024$first_to_cut, cumul = FALSE)


#  [16] "tap_damage_yr" =====================================================
# SAMPLE 2024
tap_d <- rmtl_2024 %>% 
  filter(!is.na(tap_damage_yr)) %>% 
  count(tap_damage_yr) %>% mutate(N=sum(n),pct=n/N)

plot(tap_d$tap_damage_yr, tap_d$pct, 
     type = "b", frame = F, pch = 19, col = "blue")


#  [9] "total_years_of_use" ====================================================
# SAMPLE 2024
freq(rmtl_2024$total_years_of_use, cumul = FALSE)
q9 <- 
  rmtl_2024 %>% 
  select(hh_id,total_years_of_use) %>% 
  mutate(
    years_use_2024=ifelse(total_years_of_use>=3,"3-5",total_years_of_use)) %>%  
  filter(!is.na(years_use_2024))

q9 %>% count(years_use_2024) %>% mutate(N=sum(n),pct=n/N)

# SURVEY 2023 ................................................................
# How many years in total did you ever make use of the water for irrigation 
#                                              during Kharif[mw5]/rabi[mw6] ?
mw5_mw6 <- 
  rmtl_srvy22 %>% 
  filter(farmers_hh=="inside_ramthal",mm4==1) %>%  
  select(hh_id,mw5,mw6) %>%
  mutate(year_of_use=pmax(mw5,mw6)) %>% 
  filter(year_of_use>=1) %>% 
  mutate(years_use_2022=ifelse(year_of_use>=3,"3-6",year_of_use))

mw5_mw6 %>% count(years_use_2022) %>% mutate(N=sum(n),pct=n/N)

# SAMPLE 2024 IN 2023  ........................................................

inner_join(mw5_mw6,q9) %>% 
  count(years_use_2024) %>% mutate(N=sum(n),pct=n/N)





#  [8]"drip_last_yr"[16]"tap_damage_yr" ======================================
# SAMPLE 2024
freq(rmtl_2024$drip_last_yr, cumul = FALSE)

q8 <- rmtl_2024 %>% 
  select(hh_id,drip_last_yr) %>% 
  filter(drip_last_yr != 2024) %>% 
  mutate(drip_last_yr_2024= ifelse(drip_last_yr==2016,2017,drip_last_yr),
         drip_last_yr_2024= ifelse(drip_last_yr_2024==2023,2022,
                                   drip_last_yr_2024)
         )
q8 %>% count(drip_last_yr_2024 )%>% mutate(N=sum(n), pct=n/N)

# [16] "tap_damage_yr"
dmg_yr <- rmtl_2024 %>% 
  filter(!is.na(tap_damage_yr)) %>% 
  mutate(year= ifelse(tap_damage_yr==2023,2022,tap_damage_yr),
         year=as.numeric(year)) %>% 
  count(year) %>% mutate(N=sum(n),pct_dmg=n/N) %>% 
  select(year,pct_dmg)




# SURVEY 2023 ................................................................
#[mw4 b] What was the last year you use of the water?

mw4b <- 
  rmtl_srvy22 %>%
  select(farmers_hh,hh_id,contains("mw4b")) %>% 
  filter(farmers_hh=="inside_ramthal", !is.na(mw4b))
mw4b %>% count(mw4b) %>% mutate(N=sum(n), pct=n/N)
A <- mw4b %>% count(mw4b) %>% 
  mutate(N=sum(n), pct=n/N) %>% 
  select(mw4b,pct) %>% 
  rename(pct_mw4b=pct,year= mw4b)


# [m35b] How long has the main pipes been damaged?[months/years]
m35b <- 
  rmtl_srvy22 %>%
  select(farmers_hh,hh_id,contains("m35b")) %>%
  mutate(m35b_month=ifelse(m35b_month<0,0,m35b_month)) %>%
  mutate(damge_year= floor(2023-((m35b_month/12)+m35b_year) )) %>% 
  filter(farmers_hh=="inside_ramthal", between(damge_year, 2017, 2022))
m35b %>% count(damge_year) %>% mutate(N=sum(n), pct=n/N)
B <- m35b %>% count(damge_year) %>% mutate(N=sum(n), pct=n/N) %>% 
  select(damge_year,pct) %>% 
  rename(pct_m35b=pct)

# Merge data frames by year to make plotting easier
data <- merge(A, B, by.x = "year", by.y = "damge_year")

# Create the plot
ggplot(data, aes(x = year)) +
  geom_bar(aes(y = pct_m35b), stat = "identity", fill = "lightblue", 
           width = 0.6, alpha = 0.7, 
           position = "dodge", show.legend = TRUE) +
  geom_line(aes(y = pct_mw4b, color = "mw4b"), size = 1.2) +
  geom_point(aes(y = pct_mw4b, color = "mw4b")) +
  labs(x = "Year", y = "Percentage", 
       title = "Comparison of Percentages for m35b and mw4b over Years") +
  scale_color_manual(name = "", values = c("mw4b" = "orange2")) +
  theme_classic() 



# SAMPLE 2024 IN 2023  ........................................................

inner_join(mw4b,q8) %>% count(mw4b) %>% mutate(N=sum(n),pct=n/N)

# PLOT 
s2024 <- 
  q8 %>% count(drip_last_yr_2024 )%>% mutate(N=sum(n), pct2024=n/N) %>% 
  select(drip_last_yr_2024,pct2024) %>% 
  rename(year=drip_last_yr_2024) %>% 
  mutate(year=as.numeric(year))

s2023 <- 
  mw4b %>% count(mw4b) %>% mutate(N=sum(n), pct2023=n/N) %>% 
  select(mw4b,pct2023) %>% 
  rename(year=mw4b)

s_23_24 <- 
  inner_join(mw4b,q8) %>% count(mw4b) %>% mutate(N=sum(n),pct=n/N) %>% 
  select(mw4b,pct) %>% rename(year=mw4b,pct_23_24=pct) %>% 
  mutate(year=as.numeric(year))


df <- 
  left_join(s2023,s2024) %>% 
  left_join(s_23_24) %>% 
  left_join(dmg_yr)
df[is.na(df)] <- 0


df_long <- df %>%
  pivot_longer(cols = c(pct2023, pct2024, pct_23_24), 
               names_to = "variable", 
               values_to = "value")
  
  # Plot
ggplot(df, aes(x = year)) +
  geom_area(aes(y = pct_dmg ), fill = "lightblue", alpha = 0.6) +
  geom_line(data = df_long, aes(x = year, y = value, color = variable), 
            size = 1.2) +
    labs(title = "Last year of DI use",
         subtitle = "The gray area indicates the year the system was damaged",
         x = "", y = "% of Households") +
    scale_color_manual(values = c("lightblue4", "lightblue3", "purple4")) +
    theme_minimal()
  


  
#  [5] "drip_1st_yr"  ==========================================================
# SAMPLE 2024
freq(rmtl_2024$drip_1st_yr, cumul = FALSE)
q5 <- rmtl_2024 %>% 
  select(hh_id,drip_1st_yr) %>% 
  filter(drip_1st_yr>2012, drip_1st_yr!=2023) %>% 
  mutate(drip_1st_yr= ifelse(drip_1st_yr==2016,2017,drip_1st_yr)) %>% 
  rename(drip_1st_yr_24=drip_1st_yr)
q5 %>% count(drip_1st_yr_24 )%>% mutate(N=sum(n), pct_24=n/sum(n))

# SURVEY 2023 ................................................................
#[mw1 a] If Yes, in which year did you first make use of the water?

mw1a <- rmtl_srvy22 %>%
  select(farmers_hh ,hh_id,mw1a) %>% 
  filter(farmers_hh=="inside_ramthal", !is.na(mw1a)) %>% 
  rename(drip_1st_yr_23=mw1a)
mw1a %>% count(drip_1st_yr_23) %>% mutate(N=sum(n), pct_23=n/sum(n))

# SAMPLE 2024 IN 2023  ........................................................

inner_join(mw1a,q5) %>% 
  count(drip_1st_yr_23) %>% mutate(N=sum(n), pct_23=n/sum(n))







# [3] "drip_trust"   =========================================================
                                         
# Is a drip system trustworthy?
freq(rmtl_2024$drip_trust, cumul = FALSE)
149-90

# [6] "drip_freq" =============================================================                             
# How many times in  season

freq(rmtl_2024$drip_freq, cumul = FALSE)

# Categorize drip_freq
Cat_drip_freq <- rmtl_2024 %>% select(hh_id,drip_freq) %>% 
  mutate(
    drip_freq_cat = case_when(
      drip_freq == 0 ~ "0",
      drip_freq == 1 ~ "1",
      drip_freq >= 2 & drip_freq <= 6 ~ "2-6",
      drip_freq >= 8 ~ "10-20",
      TRUE ~ NA_character_ ))

Cat_drip_freq %>% 
  filter(!is.na(drip_freq_cat)) %>% 
  count(drip_freq_cat) %>% mutate(N=sum(n), pct=n/sum(n))


# [7] "drip_gap_days"  Not enough answers =====================================
# [10] "farmers_B4_u"  ========================================================

# SAMPLE 2024
freq(rmtl_2024$farmers_B4_u, cumul = FALSE)

# Categorize farmers_B4_u
farmers_to_filter <- rmtl_2024 %>% 
  select(hh_id,farmers_B4_u,drip_1use_2useOwmDrip,total_years_of_use,
         tap_cut_by_, irri_jain_flood) %>% 
  mutate(
    farmers_B4_u_cat = case_when(
      farmers_B4_u %in% "dont_know" ~ "Don't know",
      !is.na(as.numeric(farmers_B4_u)) & as.numeric(farmers_B4_u) >= 1 & as.numeric(farmers_B4_u) <= 3 ~ "Close to filter [1-3]",
      !is.na(as.numeric(farmers_B4_u)) & as.numeric(farmers_B4_u) >= 4 ~ "Far from filter [4 onwards]",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(farmers_B4_u_cat)) 

farmers_to_filter %>% 
  filter(!is.na(farmers_B4_u_cat )) %>% 
  count(farmers_B4_u_cat ) %>% mutate(N=sum(n), pct=n/sum(n))



# SURVEY 2023 ................................................................
#[MM9] How many farmers are there between you and the valve/pipeline?


# location_on_pipe
distance_from_filter <- rmtl_srvy22 %>% 
  select(hh_id,mm4,mm9) %>% 
  mutate(location_on_pipe= mm9+1) %>% 
  inner_join(rmtl_In
             ) %>% 
  mutate(
    location_on_pipe_cat = case_when(
      location_on_pipe < 0  ~ "Don't know",
      location_on_pipe >= 1 & location_on_pipe <= 3 ~ "Close to filter [1-3]",
      location_on_pipe >= 4 ~ "Far from filter [4 onwards]",
      TRUE ~ NA_character_ )  ) %>%
  filter(!is.na(location_on_pipe)) 

freq(distance_from_filter$location_on_pipe_cat, cumul = FALSE)
  
  
# SAMPLE 2024 IN 2023  ........................................................

inner_join(distance_from_filter,farmers_to_filter) %>% 
  select(1,farmers_B4_u,location_on_pipe,
         farmers_B4_u_cat,location_on_pipe_cat
         ) %>% 
  count(farmers_B4_u_cat,location_on_pipe_cat) %>% mutate(N=sum(n), pct_23=n/sum(n))

# The two surveys do not have matching observations


# CROSS location on pipe with irrigation
# SAMPLE 2024

# drip/ not drip
farmers_to_filter %>% 
  filter(!is.na(farmers_B4_u_cat )) %>% 
  count(drip_1use_2useOwmDrip,farmers_B4_u_cat ) %>% 
  group_by(drip_1use_2useOwmDrip) %>%  
  mutate(N = sum(n), pct = paste0(round((n / sum(n)) * 100, 0), "%")) %>%
  arrange(desc(drip_1use_2useOwmDrip)) %>% 
  kable() %>% kable_minimal()

# flood/not flood
farmers_to_filter %>% 
  filter(!is.na(farmers_B4_u_cat ),!is.na(irri_jain_flood )) %>% 
  count(irri_jain_flood,farmers_B4_u_cat ) %>% 
  group_by(irri_jain_flood) %>%  
  mutate(N = sum(n), pct = paste0(round((n / sum(n)) * 100, 0), "%")) %>%
  arrange(desc(irri_jain_flood)) %>% 
  kable() %>% kable_minimal()

# SURVEY 2023 ................................................................

# drip/ not drip
distance_from_filter %>% 
  count(drip_use,location_on_pipe_cat) %>% 
  arrange(desc(drip_use)) %>% 
  group_by(drip_use) %>%  
  mutate(N = sum(n), pct = n / N)
  
freq(distance_from_filter$location_on_pipe_cat, cumul = FALSE)

# Farmers use ramthal system as water source
# USE IT
inner_join(a_source_irri,distance_from_filter) %>% 
  filter(source_type=="gov_source") %>% 
  count(location_on_pipe_cat)%>% 
  mutate(N = sum(n), pct = n / N)

# NOT USE IT
inner_join(a_source_irri,distance_from_filter) %>% 
  filter(source_type!="gov_source",ir_use==1) %>% 
  count(location_on_pipe_cat)%>% 
  mutate(N = sum(n), pct = n / N)


# ----
#| [11] "past_irri_NOTdrip"
# Did you irrigate in the past 5 years?  (NOT drip) 

freq(rmtl_2024$past_irri_NOTdrip, cumul = FALSE)


# ----
#| [12] "irri_source_NO_ramthal_system"
## follow up to [11]
# In what source? (Canal / borewell/pond/jain system)

freq(rmtl_2024$irri_source_NO_ramthal_system, cumul = FALSE)


# ----
# [13] "irri_source_NO_ramthal_system_YR" # Too few observations ----
# follow up to [12]
# In what years? (for every source )
rmtl_2024 %>% 
  select(irri_source_NO_ramthal_system_YR) %>%  
  filter(!is.na (irri_source_NO_ramthal_system_YR))

# [15] "irri_jain_flood_YR" # Too few observations ----
# Which years you used and which years you did not use?
freq(rmtl_2024$irri_jain_flood_YR, cumul = FALSE)


# [17] "why_farmer_cut_even_when_far_thefilter" ----
freq(rmtl_2024$why_farmer_cut_even_when_far_thefilter, cumul = FALSE)

rmtl_2024 %>% 
  select(irri_source_NO_ramthal_system_YR) %>%  
  filter(!is.na (irri_source_NO_ramthal_system_YR))

#    [18] "water_pressure_drip_compare_flood"                 ----
freq(rmtl_2024$water_pressure_drip_compare_flood, cumul = FALSE)

#    [19] "water_volume_drip_compare_flood"                   ----
freq(rmtl_2024$water_volume_drip_compare_flood, cumul = FALSE)

#    [20] "water_supply_info"                                 ----
freq(rmtl_2024$water_supply_info, cumul = FALSE)
149-33

# ----
#|    [21] "crop_under_drip"
#|    [22] "pomd_fill"


#    [27] "disputs_0non_1BcozWater_2els"                      ----
freq(rmtl_2024$disputs_0non_1BcozWater_2els, cumul = FALSE)



training=rmtl_srvy22 %>% select(hh_id, m51,m52, m56 ) %>% 
  right_join(rmtl_In) %>% filter(mm4==1)


freq(rmtl_srvy22$m51, cumul = FALSE)

freq(training$m51,cumul = F)

freq(training$m52,cumul = F)

freq(training$m56,cumul = F)
attr(rmtl_srvy22$m56, "labels")



