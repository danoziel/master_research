library(dplyr)
library(kableExtra)
library(tidyr)
library(haven)
library(summarytools)
# DFs [rmtl_srvy22_24] ============================================================

library(readr)
rmtl_srvy22 <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_srvy22.csv")

rmtl_srvy22_24 <- 
  rmtl_srvy22 %>% 
  filter(farmers_hh=="inside_ramthal",mm4==1, 
         !a5 %in%c( "AMARAVATHI","Amaravathi","Amaravati",
                    "hungund" ,"Hungund",
                    "Marol", "Chinnapur", "Hemavadagi", "Nidasanur", 
                    "Revadihal", "Thumba", "Yadahalli" ))

# location on pipe NERATIVE  --------------------------------------------------
# interviews 2024 ----
# I24 [10] "farmers_B4_u"
# S22 [MM9] How many farmers are there between you and the valve/pipeline?

freq(rmtl_2024$farmers_B4_u, cumul = FALSE)
freq(rmtl_srvy22$mm9, cumul = FALSE)


# df : Total farmers per location (continuous)
location_I24 <- 
  rmtl_2024 %>% 
  select(hh_id,farmers_B4_u, drip_1use_2useOwmDrip,irri_jain_flood) %>% 
  mutate(farmers_B4_u = ifelse(farmers_B4_u=="dont_know",1000,farmers_B4_u),
         location_on_pipe=as.numeric(farmers_B4_u),
         irri_jain_flood=as.numeric(irri_jain_flood) ) %>% 
  filter(!is.na(farmers_B4_u)
  ) %>% 
  group_by(location_on_pipe) %>%  
  summarise(n_loc=n(),
            drip_users=sum(drip_1use_2useOwmDrip),
            flood=sum(irri_jain_flood,na.rm = T)) %>% ungroup()

# df Total farmers Near/Far outlet (Binary)

location_I24_01 <- 
  location_I24 %>% 
  mutate(location=
           ifelse(location_on_pipe %in% c(4:30),"far",
                  ifelse(location_on_pipe<=3,"close","iDont_know"))) %>% 
  group_by(location) %>% 
  summarise(n_loc=sum(n_loc), drip_users=sum(drip_users), flood=sum(flood)
            ) %>% mutate(
    N=sum(n_loc), pct=n_loc/N, # total farmers on certain pipe-location out-of total sample
    pct_drip_users=drip_users/n_loc, # total DI users on certain pipe-location out-of total pipe-location
    pct_flood=flood/n_loc ) # total flood users on certain pipe-location out-of total pipe-location

#|----------------------------------------------------------------------------
# Survey 2022-23 

library(readr)
a_source_irri <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_source_irri.csv")
source_rmtl <- a_source_irri %>% 
  mutate(source_ramthal = ifelse(source_ramthal == "ramthal",1,0 )) %>% 
  select(hh_id,source_ramthal)

# NOTE  mm9 start from 0, 0 is the first, closest to outlet

location_S22 <- rmtl_srvy22_24  %>% 
  select(hh_id,mm9) %>% rename(location_on_pipe= mm9) %>% 
  left_join(source_rmtl) %>% 
  left_join(rmtl_InOut %>% select(hh_id,drip_use,ir_use,in1_out0)) %>% 
  mutate(
    location_on_pipe=ifelse(location_on_pipe==-999,NA,location_on_pipe),
    near1_far0_outlet = case_when(
      location_on_pipe %in% c(0:3)  ~ 1 ,
      location_on_pipe %in% c(4:50) ~ 0 , TRUE ~ NA) )
#
#
location_S22 <- rmtl_srvy22  %>% filter(in1_out0==1) %>% 
  select(hh_id,mm9) %>% rename(location_on_pipe= mm9) %>% 
  left_join(source_rmtl) %>% 
  left_join(rmtl_InOut %>% select(hh_id,drip_use,ir_use,in1_out0)) %>% 
  mutate(
    location_on_pipe=ifelse(location_on_pipe==-999,NA,location_on_pipe),
    near1_far0_outlet = case_when(
      location_on_pipe %in% c(0:4)  ~ 1 ,
      location_on_pipe %in% c(5:50) ~ 0 , TRUE ~ NA) )



# names(location_S22)
m1 <-  lm(source_ramthal  ~ near1_far0_outlet, data = location_S22)
# summary(m1)
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F )


df9 <-  location_S22 %>% 
  pivot_longer(-c(hh_id,location_on_pipe,in1_out0,near1_far0_outlet) ,
               names_to = "usage_type",
               values_to = "usage_value")

df9 %>% 
  mutate(Proximity_outlet = case_when(
    near1_far0_outlet == 1  ~ "# Near" ,
    near1_far0_outlet == 0 ~ "## Far" ,TRUE ~ NA) ) %>% 
  group_by(usage_type, Proximity_outlet) %>%  
  summarise( #n_loc=n(),
            usage_value=mean( usage_value) ) %>% 
  filter(usage_type != "drip_use", !is.na(Proximity_outlet))


library(tidyr)
# library(purrr)
# library(broom)

# 1) model formula inputs # 2) Nest by status and fit 
fits <- df9 %>%
  group_by(usage_type) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(
      usage_value ~ near1_far0_outlet, 
      data = .x)),
    coefs = map(model, broom::tidy),
    stats = map(model, broom::glance)
    )%>%
  mutate(
    Model = map(data, ~ lm(
      usage_value ~ location_on_pipe, 
      data = .x)),
    Coefs = map(Model, broom::tidy),
    Stats = map(Model, broom::glance))

# 3) Stacked outputs # 4) Join with model summary stats
# proximity from outlet as a binary variable
inproj_with_fit_A <- 
  fits %>% unnest(coefs) %>% 
  left_join(
    fits %>% unnest(stats) %>% select(usage_type,nobs, r.squared)
  ) %>% ungroup() %>% 
  select(term,usage_type, estimate, std.error, p.value,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared 
  ) %>% 
  pivot_longer(estimate:R2, names_to = "metric", values_to = "value") %>% 
  pivot_wider(names_from = usage_type, values_from = value ) %>% 
  filter(term == "near1_far0_outlet" | 
         term == "(Intercept)" & metric == "estimate" ) 

# proximity from outlet as a continuous
inproj_with_fit_B <- 
  fits %>% unnest(Coefs) %>% 
  left_join(
    fits %>% unnest(Stats) %>% select(usage_type,nobs, r.squared)
  ) %>% ungroup() %>% 
  select(term,usage_type, estimate, std.error, p.value,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared 
  ) %>% 
  pivot_longer(estimate:R2, names_to = "metric", values_to = "value") %>% 
  pivot_wider(names_from = usage_type, values_from = value ) %>% 
  filter(term == "location_on_pipe" | 
           term == "(Intercept)" & metric == "estimate" ) 


# reg table ----
inproj_with_fit_A %>%
  # rbind(control_mean) %>% 
  mutate(across(-c(term,metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  kable() %>% kable_minimal()


inproj_with_fit_B %>%
  # rbind(control_mean) %>% 
  mutate(across(-c(term,metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  kable() %>% kable_minimal()








# # "Proximity" and water source which not ramthal

flood_indi <- rmtl_InOut %>% 
  mutate(flood_Y6th=hh_drip_2021_22+hh_ir_2021_22,
         flood_6Ys=drip_use+ir_use,
         ) %>% 
  select(hh_id,flood_Y6th,flood_6Ys) %>% 
  left_join(a_source_irri %>% select(hh_id,source_pond,source_ramthal))%>% 
  mutate(flood_Y6th=ifelse(is.na(flood_Y6th),NA,
            ifelse(flood_Y6th==1 & !source_pond %in%  c("rain","ramthal"),1,0))
    ) %>% 
  mutate(flood_6Ys=ifelse(
    flood_6Ys==1 & !source_pond %in%  c("rain","ramthal"),1,0))


source_WA <- rmtl_srvy22_24  %>% 
  filter(farmers_hh=="inside_ramthal",
         !is.na(mm9)) %>% 
  select(hh_id,mm9) %>% 
  mutate(mm9= mm9+1) %>% 
  mutate(location=ifelse(mm9 == -998,"dont_know",
                         ifelse(mm9 %in% c(1:3),"Near","Far"))) %>% 
  left_join(flood_indi) %>% 
  mutate(source_indi=ifelse(source_pond %in% c("rain","ramthal"),0,1),
         source_any=ifelse(source_pond  == "rain",0 ,1)
         ) 


source_WA %>% group_by(location) %>% 
  mutate(N=n()) %>% 
  count(N,location,source_ramthal) %>% mutate(p=n/N*100)

source_WA %>% group_by(location) %>% 
  summarise( N=n(), sum=sum(source_indi)) %>% mutate(sum/N)

source_WA %>% group_by(location) %>% 
  summarise( N=n(), sum=sum(flood_Y6th,na.rm = T)) %>% mutate(sum/N)

source_WA %>% group_by(location) %>% 
  summarise( N=n(), sum=sum(flood_6Ys,na.rm = T)) %>% mutate(sum/N)


# mm10	-----
# Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?

# 1	It has never happened
# 2	It happens about once a season
# 3	It happens several times a season
# 4	It happens on a constant basis
# 5	Some seasons it happens and some it doesn't


rmtl_srvy22_24  %>% filter(!is.na(mm9)) %>% 
  select(hh_id,mm9,mm10) %>% 
  mutate(mm9= mm9+1, 
         location=ifelse(mm9 == -998,"dont_know",
                  ifelse(mm9 %in% c(1:3),"Near","Far")),
         mm10=ifelse(mm10==2,3,mm10)) %>% 
  filter(mm10 %in% c(1:4) ,location != "dont_know") %>%
  group_by(location) %>% mutate(N=n())%>% 
  count(N,location,mm10) %>% 
  mutate(pct = n/N*100) %>% 
  select(location,mm10,pct) %>% 
  pivot_wider(names_from = location,values_from = pct) %>% 
  select(mm10, Near,Far)


		
# How many years in total did you ever make 
# use of the water for irrigation during 
# [mw5] Kharif
# [mw6]	Rabi


years_use <- 
  rmtl_srvy22_24 %>% 
  select(hh_id, mm9, mw5, mw6 ) %>% 
  mutate(
    years_users = ifelse(mw5 > mw6, mw5, mw6),
    years_sample = ifelse(is.na(years_users), 0, years_users),
    location_on_pipe = ifelse(mm9 == -999, NA, mm9),
    near1_far0_outlet = case_when(
      location_on_pipe %in% c(0:3)  ~ 1 ,
      location_on_pipe %in% c(4:50) ~ 0 , TRUE ~ NA), 
    Years=ifelse(years_users %in% c(3:6),"3-6",years_users ),
    proximity_outlet = case_when(
      near1_far0_outlet == 1 ~ "|  Near" ,
      near1_far0_outlet == 0 ~ "|| Far" , TRUE ~ NA)
    ) %>% select(-c(mm9, mw5, mw6))

### Average years of water irrigation use
years_use %>% group_by(proximity_outlet) %>% 
  summarise(years_sample = mean(years_sample,na.rm=T),
            years_users = mean(years_users,na.rm=T)) %>% 
  filter(!is.na(proximity_outlet))


### Freq years of water irrigation use 
# NO 0's
years_use %>%
  mutate(Years = ifelse(Years==0,NA,Years)) %>% 
  count(proximity_outlet,Years) %>% drop_na() %>% 
  group_by(proximity_outlet) %>%
  mutate(n/sum(n)*100)

# YES 0's
years_use %>%
  mutate(Years= ifelse(is.na(Years),0,Years)) %>% 
  count(proximity_outlet,Years) %>% drop_na() %>% 
  group_by(proximity_outlet) %>%
  mutate(n/sum(n)*100)











############### Total respondents IN%
df1 <- location_Int_13 %>%   rename(pct_source_ramthal=pct_flood) %>% 
  select(location ,pct, pct_drip_users, pct_source_ramthal) %>% 
  mutate(dt="Interviews_2024")
df2 <- location_Sur_13 %>% 
  select(location ,pct, pct_drip_users, pct_source_ramthal)%>% 
  mutate(dt="Survey_2022-23")

df12 <- rbind(df1,df2) %>% 
  select(dt, everything()) %>% arrange(location)
df12$location[df12$location=="1000"] <- "Dont \nknow"

df12 %>%
  mutate(pct = pct * 100) %>%
  ggplot(aes(x=factor(location, level=c('1', '2', '3', '4','5','6','7','8','9','10','11','12','13+',"Dont \nknow")), 
             y = pct, fill = dt)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = 
    c("Interviews_2024" = "#3c78d8ff", "Survey_2022-23" = "#8e7cc3ff")) +
  labs(x = "Location on pipline",y = "% respondents", fill =NULL,
    title = "Total respondents in % of sample") +
  theme_minimal(base_family = "serif")+ theme(legend.position='none')


########### PLOT  [pct_drip_users] [pct_source_ramthal]
df10 <- location_Int_01 %>%   rename(pct_source_ramthal=pct_flood) %>% 
  select(location ,pct, pct_drip_users, pct_source_ramthal) %>% 
  mutate(dt="Interviews_2024")
df20 <- location_Sur_01 %>% 
  select(location ,pct, pct_drip_users, pct_source_ramthal)%>% 
  mutate(dt="Survey_2022-23")

df0 <- rbind(df10,df20) %>% 
  select(dt, everything()) %>% arrange(location)
df0$location[df0$location=="DN"] <- "Dont \nknow"
df0$location[df0$location=="close"] <- "Close from \nwater outlet"
df0$location[df0$location=="far"] <- "Far from \nwater outlet"

########### PLOT  [pct_drip_users]
df0 %>%
  mutate(pct_drip_users  = pct_drip_users  * 100) %>%
  ggplot(aes(x=factor(location, level=c("Close from \nwater outlet","Far from \nwater outlet","Dont \nknow")), y = pct_drip_users, fill = dt)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(pct_drip_users)), 
            position = position_dodge(width = 0.8), 
            vjust = 1.5, size = 3.5, family = "serif") +
  scale_fill_manual(values = c("Interviews_2024" = "#6d9eebff", 
                               "Survey_2022-23" = "#b4a7d6ff")) +
  labs(x = NULL,y = "% of Users", fill =NULL, title = "DI Users") +
  theme_minimal(base_family = "serif")

########### PLOT [pct_source_ramthal]
df0 %>%
  mutate(pct_source_ramthal  = pct_source_ramthal  * 100) %>%
  ggplot(aes(x=factor(location, level=c("Close from \nwater outlet","Far from \nwater outlet","Dont \nknow")), 
             y = pct_source_ramthal, fill = dt)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(pct_source_ramthal)), 
            position = position_dodge(width = 0.8), 
            vjust = 1.5, size = 3.5, family = "serif") +
  scale_fill_manual(values = c("Interviews_2024" = "#6d9eebff", 
                               "Survey_2022-23" = "#b4a7d6ff")) +
  labs(x = NULL,y = "% of Users", fill =NULL,
       title = "Ramthal as water source") +
  theme_minimal(base_family = "serif")



# ==17.09.2025 ===================================

library(googlesheets4)

gs4_auth()  # browser pops up -> choose your Google account (one-time per session)
ss_url <- "https://docs.google.com/spreadsheets/d/12CU3bkAGrlywwUxqqxVqerI6X9AKhyXsTtBxqQyDQIs/edit?gid=579200740#gid=579200740"
why_no_use <- read_sheet(ss_url, sheet = "why_no_use")        # tab name

# pipeline_location_24 
pipeline_location_24 <- 
  rmtl_2024 %>% 
  select(hh_id,farmers_B4_u ) %>% 
  rename(location_on_pipe=farmers_B4_u) %>% 
  mutate(location_on_pipe = ifelse(location_on_pipe=="dont_know",-999,location_on_pipe),
         location_on_pipe=as.numeric(location_on_pipe)) %>% 
  filter(!is.na(location_on_pipe)) %>% 
  mutate(
    location = ifelse(location_on_pipe %in% c(4:30),"Far",
                      ifelse(location_on_pipe %in% c(1:3),"Close","dont_know")))

# pipeline_location_22 

pipeline_location_22 <- 
  rmtl_srvy22_24  %>% 
  filter(farmers_hh=="inside_ramthal",!is.na(mm9)) %>% 
  select(hh_id,mm9) %>% 
  mutate(location_on_pipe= mm9+1) %>% 
  select(hh_id,location_on_pipe) %>%
  mutate(
    location = ifelse(location_on_pipe %in% c(4:51),"Far",
                      ifelse(location_on_pipe %in% c(1:3),"Close","dont_know")))


# why no use drip      .................................................... ----

# [Int24] why_no_use    __________________________ 

names(why_no_use)
pipeline_location_24 %>% 
  left_join(why_no_use) %>% filter(!is.na(`5.WHY`)) %>% 
  count( location ,no_water_supply) %>% 
  group_by(location) %>% mutate(N=sum(n),n/N ) %>% 
  filter( location != "dont_know", !is.na(no_water_supply))

# [Srvy22] mw1c         __________________________ 

###  If No (use the drip), Why?
# 1	The main piping was never functional
# 2	The laterals was never installed in my field
# 3	The laterals in my field was damaged
# 4	Rainfall was sufficient
# 5	I wanted to irrigate, but other farmers took all the water
# 6	Water was supplied only after I already sowed a rainfed crop
# 7	Water was not supplied when needed
# 8	I did not know when water was supplied
# 9	I do not trust the company
# 10	Water supply is unpredictable I cant count on it
# -888 other

library(dplyr)
library(tidyr)

# mapping table
map_tbl <- tibble::tribble(
  ~why, ~group,
  "2",  "company","9", "company","8",  "company",
  "1",  "pipe_damage","3",  "pipe_damage",
  "4",  "rain",
  "5",  "farmer_took",
  "6",  "supply","7",  "supply","8",  "supply", "10", "supply",
  "6",  "timing","8",  "timing","10", "timing")

# get total N of original separated reasons
N_total <- rmtl_srvy22_24 %>%
  select(hh_id, mw1c) %>%filter(!is.na(mw1c)) %>%
  separate_rows(mw1c, sep = " ") %>%
  filter(mw1c != "") %>%
  nrow()

# count groups using that fixed N_total
mw1c_counts <- rmtl_srvy22_24 %>%
  select(hh_id, mw1c) %>% rename(why = mw1c) %>% filter(!is.na(why)) %>%
  separate_rows(why, sep = " ") %>% filter(why != "")

mw1c_pct <- mw1c_counts %>% 
  left_join(map_tbl, by = "why",relationship = "many-to-many") %>%
  filter(!is.na(group)) %>%
  count(group, name = "n") %>%
  mutate(pct = round(n / N_total * 100, 2) )

pipeline_location_22 %>% 
  inner_join(mw1c_counts,relationship = "many-to-many") %>% 
  left_join(map_tbl, by = "why",relationship = "many-to-many") %>% 
  filter(!is.na(group)
  ) %>%
  group_by(group,location) %>% summarise(n=n()) %>% 
  group_by(location) %>% 
  mutate(n_loc=sum(n),pct = round(n / n_loc * 100) ) %>% 
  filter(location != "dont_know")



# pipline damages       ................................................... ----

# [Int24]   tap_status_1damged_0not
tap_status_count <- 
  rmtl_2024 %>% 
  select(hh_id,tap_status_1damged_0not ) %>% 
  filter(!is.na(tap_status_1damged_0not)) %>% 
  inner_join(pipeline_location_24 ) %>% 
  rename(status = tap_status_1damged_0not )%>% 
  mutate(status=ifelse(status==1,"Damaged","Works")) %>% 
  count(location,status) %>% 
  group_by(location) %>% 
  mutate( pct = round(n / sum(n) * 100,2) ) %>% ungroup() %>% 
  filter(location != "dont_know")
  


# [Survy22] m35 
# What is the status of the main pipe coming into your land ?

m35_counts <- rmtl_srvy22_24 %>%
  select(hh_id, m35) %>% filter(!is.na(m35)) %>% 
  rename(status = m35 )%>% 
  mutate(status=ifelse(status==1,"Works","Damaged"))

m35_pct <- pipeline_location_22 %>% 
  inner_join(m35_counts) %>% 
  group_by(status,location) %>% summarise(n=n()) %>% 
  group_by(location) %>% 
  mutate(pct = round(n / sum(n) * 100,2) ) %>% 
  filter(location != "dont_know")


# REG ---- 

pipe_status_S22 <- rmtl_srvy22 %>%
  select(hh_id, m35)  %>% 
  mutate(pipe_status =ifelse(m35==2,0,m35) ) %>% 
  left_join(rmtl_cntrl_vars) %>% rename(Elevation=elevation) %>% 
  left_join(rmtl_InOut) %>% filter(in1_out0==1,mm4==1) 

pipe_status_S22 %>%  filter(!is.na(pipe_status)) %>% 
  group_by(ir_use) %>% mutate(N=n()) %>% 
  count(N,pipe_status) %>% mutate(n/N)



m1 <-  lm(ir_use  ~ pipe_status, 
          pipe_status_S22)
# summary(m1)
sjPlot::tab_model(m1 ,  show.se = T,digits = 5, show.stat  = F )

m2 <-  lm(ir_use  ~ pipe_status + # dist_Km_boundary +
            hh_haed_age + hh_haed_gendar + hh_haed_edu_level + 
            total_acre16 + housing_str321 + 
            livestock_dairy + Bullock + Tractor + Plough + 
            Thresher + Seed_drill + Motorcycle + Fridge, 
          pipe_status_S22)
# summary(m1)
sjPlot::tab_model(m2 ,  show.se = T,digits = 5, show.stat  = F )

library(broom)
tidy(m1)
tidy(m1) |> as_tibble()

m11 <- tidy(m1) %>% select(term ,estimate ,std.error ,p.value) %>%
  pivot_longer(-term,names_to = "metric",values_to = "value" ) %>% 
  filter(term=="(Intercept)" & metric == "estimate")
m12 <- tidy(m1) %>% select(term ,estimate ,std.error ,p.value) %>%
  pivot_longer(-term,names_to = "metric",values_to = "value" ) %>% 
  filter(term=="pipe_status")
m13 <- glance(m1) %>% select(r.squared,nobs) %>% mutate(term="") %>% 
  pivot_longer(-term,names_to = "metric",values_to = "value" )

rbind(m11,m12,m13) %>% 
  mutate(across(-c(term,metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% kable() %>% kable_minimal()









# [Int24] tap_damage_yr ....................................................----      

tap_damage_yr_pct <- rmtl_2024 %>% count(tap_damage_yr) %>% 
  filter(!is.na(tap_damage_yr)) %>%
  mutate(tap_damage_yr = as.integer(tap_damage_yr),
         tap_damage_yr = if_else(tap_damage_yr == 2017,2017L, tap_damage_yr - 1L) ) %>% 
  mutate(pct = round(n / sum(n) * 100, 2) )
tap_damage_yr_pct$pct[tap_damage_yr_pct$tap_damage_yr==2019] <- 25.2
tap_damage_yr_pct$pct[tap_damage_yr_pct$tap_damage_yr==2017] <- 3.84

tap_damage_yr_pct %>% mutate(pct = round(pct) )

ggplot(tap_damage_yr_pct, aes(x = factor(tap_damage_yr), y = pct)) +
  geom_col(width = 0.5,fill = "lightgray") +
  labs(x =NULL,y = "Households (%)",title = "Plotpipe Damage by Year (Int24)" ) +
  theme_classic(base_family = "serif") +
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 13))




# [Survy22] last/first year you         ..............................................----

# [mw1a] in which year did you first make use of the water? 
# [mw4b]	What was the last year you use of the water? 

mw1a_counts <- rmtl_srvy22_24 %>%
  select(hh_id, mw1a) %>% filter(!is.na(mw1a))

pipeline_location_22 %>% 
  inner_join(mw1a_counts) %>% 
  rename(first_use=mw1a) %>% 
  group_by(first_use,location) %>% summarise(n=n()) %>% 
  group_by(location) %>% 
  mutate(n_loc=sum(n),pct = round(n / n_loc * 100,2) ) %>% 
  filter(location != "dont_know")


mw4b_counts <- rmtl_srvy22_24 %>%
  select(hh_id, mw4b) %>% filter(!is.na(mw4b))

mw4b_pct <- 
  pipeline_location_22 %>% 
  inner_join(mw4b_counts) %>% 
  rename(last_use=mw4b) %>% 
  group_by(last_use,location) %>% summarise(n=n()) %>% 
  group_by(location) %>% 
  mutate(n_loc=sum(n),pct = round(n / n_loc * 100) ) %>% ungroup() %>% 
  filter(location != "dont_know")


library(ggplot2)

ggplot(mw4b_pct, aes(x = last_use, y = pct, color = location)) +
  geom_line(linewidth = 2) +
  geom_point(shape = 21,  size = 3.5, stroke = 1.2, fill = "white" ) +
  scale_color_manual( values = c("Close" = "darkblue", "Far" = "lightblue")
  ) +
  scale_x_continuous(breaks = unique(mw4b_pct$last_use)) +
  labs( x = "",y = "Households (%)",color = NULL,
    title = "Last Use by Proximity to Water Outlet (Int24)" ) +
  theme_classic(base_family = "serif") +
  theme(axis.text = element_text(size = 11),axis.title = element_text(size = 11) )






# [mw14]  often water is provided   ........................................----
# Typically, in your experience, during the period water is provided, how often is it provided?


mw14_counts <- rmtl_srvy22_24 %>%
  select(hh_id, mw14,mw14_int) %>% filter(!is.na(mw14))

mw14_counts %>% count(mw14)
















# [3] "drip_trust"   =========================================================

# Is a drip system trustworthy?
freq(rmtl_2024$drip_trust, cumul = FALSE)
149-90


rmtl_2024 %>% 
  select(drip_trust,drip_1use_2useOwmDrip) %>% 
  mutate(drip_use=ifelse(drip_1use_2useOwmDrip==2,1,drip_1use_2useOwmDrip)) %>% 
  drop_na() %>% 
  count(drip_use, drip_trust) %>% 
  group_by(drip_use) %>% mutate(N=sum(n),pct=n/N)



# Correlation damage with usage ----

# interviews 2024

freq(rmtl_2024$tap_status_1damged_0not, cumul = FALSE)
freq(rmtl_2024$drip_1use_2useOwmDrip, cumul = FALSE)

dw1 <- rmtl_2024 %>% 
  select(tap_status_1damged_0not,drip_1use_2useOwmDrip) %>% 
  mutate(drip_use=ifelse(drip_1use_2useOwmDrip==2,1,drip_1use_2useOwmDrip)) %>% 
  drop_na()

dw2 <- rmtl_2024 %>% 
  select(tap_status_1damged_0not,irri_jain_flood) %>% 
  drop_na()

dw1 %>% count(drip_use, tap_status_1damged_0not) %>% 
  group_by(drip_use) %>% mutate(N=sum(n),pct=n/N) %>% filter(tap_status_1damged_0not==1)

dw2 %>% count(irri_jain_flood, tap_status_1damged_0not) %>% 
  group_by(irri_jain_flood) %>% mutate(N=sum(n),pct=n/N) %>% filter(tap_status_1damged_0not==1)

# survey 2022-23
# [m35] What is the status of the main pipe coming into your land ?
attr(Ramthal_Karnataka_Cleaned_Data$m35, "labels")
# Works # Damaged 
# 1     # 2 

m35 <- 
  rmtl_srvy22_24 %>% 
  select(hh_id, "m35") %>% filter(!is.na(m35)) %>%
  left_join(
    rmtl_InOut %>% select(hh_id,drip_use)
  ) %>% left_join(source)

m35 %>% count(m35) %>% mutate(N=sum(n), pct= n/N) %>% filter(m35==2)

m35 %>% count(drip_use ,m35) %>% 
  group_by(drip_use ) %>% 
  mutate (N=sum(n), pct= n/N) %>% filter(m35==2)

m35 %>% count(source_ramthal ,m35) %>% 
  group_by(source_ramthal ) %>% 
  mutate (N=sum(n), pct= n/N) %>% filter(m35==2)








# DI advantages -----
#Are you aware of any advantages of drip irrigation over other irrigation methods?	
# 0	No knowledge 
# 1	Increased yields 
# 2	Water saving
# 3	Fertilizer saving 
# 4	Less weeds 
# 5	Less labor requirements # 6	Other

rmtl_srvy22_24 %>%
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ) %>% 
# filter(mm5 == 1 ) %>% 
  select(hh_id,m3_0:m3_5 ) %>%
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>% summarise(n=sum(yn),N=n()) %>%  
  mutate(pct=n/N) %>% 
  kbl() %>% kable_styling()




# Have you used it in [ _____ ]?
# [mw1c] If No, Why? (in Rabi)
# [mw7] In Kharif, when you did NOT irrigate, what are the reasons?

rmtl_srvy22_24$mw1c
# Sr=
rmtl_srvy22_24 %>% 
  select(hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  group_by(ans) %>% summarise(n=sum(yn,na.rm = T),N=n()) %>%  
  mutate(pct=n/N) %>%  
  mutate(TH=c("a", "j", "b","c","d","e",  "f","g","h","i")) %>% 
  select(-ans) %>% 
  rename(Rn=n,RN=N,Rpct=pct)
# 1	The main piping was disfunctional
# 2	The laterals was not installed in my field or was damaged
### 3	Rainfall was sufficient
# 4	I wanted to irrigate, but company did not supply water 
# 5	I wanted to irrigate, but other farmers took all the water
# 6	Water was supplied only after I already sowed a rainfed crop
# 7	Water was not supplied when needed
# 8	I did not know when water was supplied
# 9	I do not trust the company
# 10	Water supply is unpredictable I cant count on it

rmtl_srvy22 %>% 
  filter(mm4 == 1, farmers_hh=="inside_ramthal" ,a5 !="Hungund") %>% 
  select(hh_id,starts_with("mw1c") ) %>%filter( mw1c != "" ) %>% 
  select(-mw1c__888 , -mw1c_other ,-mw1c) %>% 
  mutate(
    damages = mw1c_1 + mw1c_2,
    no_water = mw1c_4,
    farmers_took_all_water = mw1c_5,
    uncertainty_supply=+mw1c_6+mw1c_7+mw1c_8+mw1c_10,
    company_service = mw1c_9 + mw1c_4
         ) %>% 
  pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>% 
  filter(yn==1) %>% 
  group_by(ans) %>% count(ans) %>% ungroup() %>% 
  mutate(pct=n/284)
  






#### rmtl_srvy22$mw7_kharif_2022
##### Sk=
####   rmtl_srvy22 %>%
####   filter(mm4 == 1, farmers_hh=="inside_ramthal" ,a5 !="Hungund") %>%
####   select(hh_id,mw7_kharif_2022,mw7_kharif_2022_1:mw7_kharif_2022_10 ) %>%
####   filter( mw7_kharif_2022 != "" ) %>%
####   select(-mw7_kharif_2022  ) %>%
####   pivot_longer(-c(hh_id), names_to = "ans",values_to = "yn") %>%
####   group_by(ans) %>% summarise(n=sum(yn,na.rm = T),N=n()) %>%
####   mutate(pct=n/N)%>%
####   mutate(TH=c("a", "j", "b","c","d","e",  "f","g","h","i")) %>%
####   select(-ans)
#### 
#### full_join(Sr,Sk) %>% mutate(Rpct-pct) %>% arrange(TH)



# mw 2		Have you used it in [ _____ ]?		
# 1	Kharif # 2	Rabi # 3	Both #

rmtl_srvy22_24  %>% select(mm5,starts_with("mw2")) %>% 
  count(mm5,mw2) %>% filter(!is.na(mw2) | mm5 == 0) %>%  
  mutate(n1=sum(n)) %>% 
  group_by(mm5) %>%  mutate(n2=sum(n)) %>% ungroup() %>% 
  mutate( mw2 = case_when(mw2 == 1 ~ "Kharif",
                          mw2 == 2 ~ "Rabi",
                          mw2 == 3 ~ "Both",
                          TRUE ~ "Didn't use drip"),
          N=sum(n),pct1=n/N*100, 
          pct2=ifelse(mm5==1,n/n2*100,NA)) %>% 
  select(mw2,n,pct1,pct2
         ) %>% 
  rename(`When do you water, in what season?`=mw2) %>% 
  mutate(pct1 = paste0(round(pct1), "%"),
         pct2 = paste0(round(pct2), "%")) %>% 
  kable() %>% kable_minimal()

#mw1 a	------
# If Yes, in which year did you first make use of the water?

rmtl_srvy22_24  %>% select(mm5,starts_with("mw1a")) %>% 
  count(mm5,mw1a) %>% filter(!is.na(mw1a) | mm5 == 0) %>%  
    mutate(n1=sum(n)) %>% 
  group_by(mm5) %>%  mutate(n2=sum(n)) %>% ungroup() %>% 
  mutate( mw1a = ifelse(is.na(mw1a),"Didn't use drip",mw1a),
          N=sum(n),pct1=n/N*100, 
          pct2=ifelse(mm5==1,n/n2*100,NA)) %>% 
  select(mw1a,n,pct1,pct2
         )%>% 
  filter(!is.na(pct2)) %>% 
  ggplot(aes(x = factor(mw1a), y = pct2)) +
  geom_bar(stat = "identity",fill = "steelblue4") +
  labs(x =NULL,y="% of Respondents",
       title = "First use of system water") + 
  theme_minimal(base_family = "serif")


# mw12		----
# Typically, in your experience, when water is provided in a particular year, in which month does it start?
		 		
season_colors <- c("Kharif"="dodgerblue4","Rabi"="burlywood4", "Summer"="gray70")

rmtl_srvy22_24  %>% select(starts_with("mw12")) %>% 
  count(mw12) %>% filter(mw12 %in% c(1:12)) %>% 
  mutate(N=sum(n),pct=n/N*100) %>%
  mutate(N = sum(n),pct = n / N,
    month = month.abb[mw12] ,        # Convert to abbreviated month names
    season = case_when(mw12 %in% c(6, 7, 8, 9, 10) ~ "Kharif",
      mw12 %in% c(11, 12, 1, 2, 3) ~ "Rabi",TRUE ~ "Summer")
  ) %>% 
  ggplot(aes(x = factor(month, levels = month.abb), y = pct * 100, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = season_colors) +
  labs(x =NULL,y="% of Respondents",
    title = "Month in which water supply begins") + 
  theme_minimal(base_family = "serif")


# mw14		----
# Typically, in your experience, during the period water is provided, how often is it provided?
# 1	Every  _____ Days
# 2	Very unpredictable and irreular
# 3	Dont know

mw14_123 <- rmtl_srvy22 %>% select(starts_with("mw14")) %>% 
  count(mw14) %>% filter(mw14 %in% c(2,4)) %>% 
  rename(freq=mw14) %>% select(freq,n)
mw14_123$freq[mw14_123$freq==2] <- "Unpredictable"
mw14_123$freq[mw14_123$freq==4] <- "Don't know"

mw14_DAYS <- rmtl_srvy22 %>% select(starts_with("mw14")) %>% 
  filter(!is.na(mw14_int)) %>% 
  mutate(freq = case_when(
    mw14_int %in% c(1:6) ~ "1-2 a week",
    mw14_int %in% c(9:40) ~ "Once a month",
    mw14_int %in% c(50:100) ~ "Once every 2-3 months",
    TRUE ~ "T")
  ) %>% count(freq) 

rbind(mw14_DAYS, mw14_123) %>%  
  mutate(N=sum(n),pct=n/N*100) %>% 
  select(freq,pct) %>% 
  mutate(pct = paste0(round(pct), "%")) %>% 
  kable() %>% kable_styling()


# mw4		Are you still making use of the water from the project to irrigate your land?	
#	1	Yes, Everytime Water is provided
# 2	No, I stopped, even if water is provided
# 3	Sometimes, Depends 

mw4 <- rmtl_srvy22_24  %>% 
  select(mm2, mm4,mm5,starts_with("mw4"))%>% 
  mutate(last_yr_use=ifelse(mw4 == 1,2022,mw4b))  

mw4 %>% filter(!is.na(last_yr_use) ) %>% 
  count(last_yr_use) %>% 
  mutate(N=sum(n),pct=n/N*100)%>% 
  ggplot(aes(x = factor(last_yr_use), y = pct)) +
  geom_bar(stat = "identity",fill = "steelblue4") +
  labs(x =NULL,y="% of Respondents",
       title = "Last use of system water") + 
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 11))

# "mw5", "mw6" ---- 
# How many years in total did you ever make use of the water for irrigation
rmtl_srvy22_24 %>% 
  select(mm5,"mw5", "mw6") %>% 
  mutate(yr=ifelse(mw5 > mw6,mw5,mw6)) %>% 
  count(yr) %>% filter(yr>0) %>%   
  mutate(N=sum(n),pct = paste0(round(n/N*100), "%")
         ) # %>% kable() %>% kable_styling()

rmtl_srvy22_24 %>% 
  select(mm5,"mw5", "mw6") %>% 
  mutate(yr=ifelse(mw5 > mw6,mw5,mw6),
         Years=ifelse(yr %in% c(3:6),"3-6",yr )
         ) %>% 
  count(Years) %>% filter(Years>0) %>%   
  mutate(N=sum(n),pct = paste0(round(n/N*100), "%")
  )# %>% kable() %>% kable_styling()




rmtl_srvy22_24  %>%
  select(hh_id, starts_with("m20_")) %>%
  pivot_longer(-hh_id,names_to = "M20",values_to = "val") %>%
  separate(M20, into = c("var", "season", "year"), sep = "_", remove = F) %>%
  group_by(hh_id,year) %>% summarise(use=sum(val,na.rm = T)) %>%
  mutate(use=ifelse(use==2,1,use)) %>%
  summarise(use_times=sum(use)) %>%
  count(use_times) %>% filter(use_times>0) %>%
  mutate(N=sum(n),pct = paste0(round(n/N*100), "%"))
# 
# 
rmtl_srvy22_24  %>%
  select(hh_id, starts_with("m20_")) %>%
  pivot_longer(-hh_id,names_to = "M20",values_to = "val") %>%
  separate(M20, into = c("var", "season", "year"), sep = "_", remove = F) %>%
  group_by(hh_id,year) %>% summarise(use=sum(val,na.rm = T)) %>%
  mutate(use=ifelse(use==2,1,use)) %>% ungroup() %>%
  filter(use > 0) %>%
  count(year) %>%
  mutate(N=sum(n),pct = paste0(round(n/N*100), "%"))











