# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(summarytools)

library(readr)
BL_2015_16_crop_IRsource_IRmethod <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL_2015_16_crop_IRsource_IRmethod.csv")
rmtl_16_18_22_sample <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_16_18_22_sample.csv")

# The DiD Model                     ----
# Ensure your data frame (df) contains these variables:
# Treatment: 1 if inside the project area, 0 otherwise
# Post: 1 for endline, 0 for baseline
# Y: Outcome variable (e.g., DI adoption)
# Control variables: Age, Gender, Caste, Education (or others)

#| DiD Model:
#| 𝑌𝑖𝑡 =𝛽0 +𝛽1 Treatment𝑖 +𝛽2 Post t +𝛽3 (Treatment𝑖 ×Post𝑡)+γX 𝑖𝑡 + ϵ 𝑖𝑡
# Treatment: 1 if inside the project area, 0 otherwise.
# Post: 1 for endline, 0 for baseline.
# Treatment x Post: Interaction term for DiD effect.
# X: Vector of control variables (age, gender, caste, education).

names(df1)

# organize clean df                 ----
rmtl_16_18_22_sample # ramthal treatment and control groups
 
demographic_vars_2016 <- # baseline vars
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/demographic_vars_2016.csv")

control_vars <- demographic_vars_2016 %>% #former demog_vars
  mutate(hh_haed_edu_level=
           ifelse(is.na(hh_haed_edu_level),0,
                  hh_haed_edu_level ) ) 

treatment <- rmtl_16_18_22_sample %>% select(hh_id, sample) %>%
  rename(in_project=sample)

# LAND HOLDING
# landholding is economic measurement for impact
# names(land_holding_2022)
land_holding_2022=
  a_plots_size %>% 
  filter(!plotStatus %in% c("1","6")) %>% 
  group_by(hh_id) %>% 
  summarise(total_num_plots=n(),total_acre=sum(acres,na.rm = T)) %>%
  left_join(hh_2022) %>% 
  rename( land_holding=total_acre)

hist(land_holding_2022$land_holding)
quantile(land_holding_2022$land_holding, 0.99,na.rm = T)

# names(land_holding_2016)
land_holding_2016 <- 
  bl6_plotAcre %>% 
  group_by(hh_id) %>% 
  summarise(land_holding= sum(plot_acre))

hist(land_holding_2016$land_holding)
quantile(land_holding_2016$land_holding, 0.99,na.rm = T)



#### IRRIGATION   _________________________________________________________ ----
irrigation_2022         # in part1_WaterUsage.R
irrigation_2018_2020   # in part1_WaterUsage.R
irrigation_2017       # in part1_WaterUsage.R
irrigation_BL        # in part1_WaterUsage.R
irrigation_BL_to_22 <- full_join(irrigation_2022, irrigation_2018_2020) %>% full_join(irrigation_2017) %>% full_join(irrigation_BL)

water_6Y <- # water and geo 2021-22
  rmtl_InOut %>% 
  select (hh_id, farmers_hh, in1_out0, mm4, mm5, drip_use, ir_use) %>% 
  rename(in_project = in1_out0)

ir_bl <- irrigation_BL %>% rename(drip_use=drip_use_BL,ir_use=ir_use_BL)%>%  
  select(hh_id, drip_use,ir_use) %>% mutate(Post=0,Year=2016)

ir_end <-irrigation_2022 %>%rename(drip_use=drip_use_2022,ir_use=ir_use_2022) %>%  
  select(hh_id, drip_use,ir_use) %>% mutate(Post=1,Year=2022)

ir_all <- water_6Y %>% select(hh_id, drip_use, ir_use )%>% 
  mutate(Post=1,Year=201722)

ir_17 <- irrigation_2017 %>% rename(drip_use=drip_use_2017,ir_use=ir_use_2017) %>% 
  select(hh_id, drip_use, ir_use )%>% 
  mutate(Post=1,Year=2017)

ir_18 <- irrigation_2018_2020 %>% rename(drip_use=drip_use_2018,ir_use=ir_use_2018) %>% 
  select(hh_id, drip_use, ir_use )%>% mutate(Post=1,Year=2018)

ir_19 <- irrigation_2018_2020 %>% rename(drip_use=drip_use_2019,ir_use=ir_use_2019) %>% 
  select(hh_id, drip_use, ir_use )%>% mutate(Post=1,Year=2019)

ir_20 <- irrigation_2018_2020 %>% rename(drip_use=drip_use_2020,ir_use=ir_use_2020) %>% 
  select(hh_id, drip_use, ir_use )%>% mutate(Post=1,Year=2020)


# library(summarytools)
freq(df$drip_use, plain.ascii = FALSE,cumul = T, style = "rmarkdown")

# Cross Tab ----
crs_tab= ir_bl %>% full_join(treatment) %>% 
  rename(`Use DI`= drip_use,`Use IR`= ir_use,
         `In Project`=in_project) # %>% right_join(hh_2022 )

crs_tab= ir_end %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)
crs_tab= ir_all %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)

crs_tab= ir_17 %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)

crs_tab= ir_18 %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)
crs_tab= ir_19 %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)
crs_tab= ir_20 %>% full_join(treatment) %>% rename(`Use DI`= drip_use,`Use IR`= ir_use,`In Project`=in_project)

# Use DI
sjPlot::tab_xtab(var.row=crs_tab$`In Project`,var.col=crs_tab$`Use DI`) # title = "Table | Baseline", show.row.prc = TRUE
# Use IR
sjPlot::tab_xtab(var.row=crs_tab$`In Project`,var.col=crs_tab$`Use IR`)

#    DiD           ----

# df to DiD ...........................................
df1=rbind(ir_bl,ir_end) %>% 
  full_join(treatment) %>% 
  full_join(control_vars) %>% 
  mutate(inProject_Post = in_project * Post)

df1=rbind(ir_bl, ir_all ) %>% full_join(treatment) %>% full_join(control_vars) %>% mutate(inProject_Post = in_project * Post) 

df1=rbind(ir_bl,ir_17) %>% full_join(treatment) %>% full_join(control_vars) %>% mutate(inProject_Post=in_project*Post)

df1=rbind(ir_bl,ir_18) %>% full_join(treatment) %>% full_join(control_vars) %>% mutate(inProject_Post=in_project*Post)
df1=rbind(ir_bl,ir_19) %>% full_join(treatment) %>% full_join(control_vars) %>% mutate(inProject_Post=in_project*Post)
df1=rbind(ir_bl,ir_20) %>% full_join(treatment) %>% full_join(control_vars) %>% mutate(inProject_Post=in_project*Post)


# DiD regression model to drip_use   HH FE
did_model_drip <- 
  lm(drip_use ~ in_project + Post + in_project * Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments+ 
       factor (hh_id), # Adding fixed effects
       data = df1)

summary(did_model_drip)
sjPlot::tab_model(did_model_drip ,  show.se = T,digits = 5,     show.stat  = TRUE )
# modelsummary(list("Simple" = model_small, "Full" = model_big))
modelsummary(did_model_drip)
tidy(model_1, conf.int = TRUE) %>% kable() %>% kable_paper()

# Simple linear regression
mod<- lm(drip_use ~ in_project + Post + in_project*Post+ 
               hh_haed_edu_level, 
             data = df1)






# DiD regression model to ir_use

did_model_ir <- 
  felm(ir_use ~ in_project + Post + inProject_Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments | hh_id, # Adding fixed effects
       data = df1)

sjPlot::tab_model(did_model_ir ,  show.se = T,digits = 3,     show.stat  = TRUE )


#### HIGH VALUE CROP   ____________________________________________________ ----
# High-value crops: share of farmers cultivating

hv22 <- plots_crop_2022 %>% # in DF.22.R
  filter(season != "kharif_2021") %>% 
  select(hh_id,common_n2_family) %>% distinct() %>% 
  mutate(value = 1) %>%
  pivot_wider(
    names_from = common_n2_family, 
    values_from = value,
    values_fill = list(value = 0)) %>% 
  select(hh_id,"Sunflower","VegetablesANDFruits","Oil seeds","Sugarcane") %>% 
  mutate(cashcrop_total = rowSums(select(., Sunflower:Sugarcane))) %>% 
  mutate(cashcrop_yn = ifelse(cashcrop_total==0,0,1)) %>% 
  rename(Oilseeds=`Oil seeds`) %>% 
  mutate(Post=1,Year=2022)

BL_2015_16_crop_IRsource_IRmethod %>% count(crop_common)
hv15 <- BL_2015_16_crop_IRsource_IRmethod %>% 
  filter(season != "rabi_2015_16") %>% 
  select(hh_id,crop_common) %>% distinct() %>% 
  mutate(value = 1) %>%
  pivot_wider(
    names_from = crop_common, 
    values_from = value,
    values_fill = list(value = 0)) %>% 
  select(hh_id,"Sunflower","VegetablesANDFruits","Oilseeds","Sugarcane") %>% 
  mutate(cashcrop_total = rowSums(select(., Sunflower:Sugarcane))) %>% 
  mutate(cashcrop_yn = ifelse(cashcrop_total==0,0,1)
         )%>% mutate(Post=0,Year=2015)

#    summary stat  ----
rbind(hv15,hv22)%>% 
  left_join(treatment)%>%
  pivot_longer(
    cols = c(Sunflower, VegetablesANDFruits, Oilseeds, Sugarcane, 
             cashcrop_total, cashcrop_yn),
    names_to = "crop",
    values_to = "value"
  ) %>% group_by(crop,Year, in_project) %>% 
  summarise (N=n(),n=sum(value)) %>% ungroup() %>% 
  mutate(pct=n/N) %>%  kableExtra:: kable()

#    DiD           -----
#     DiD regression model to High-value crops   HH FE
df2=rbind(hv15,hv22) %>% 
  left_join(treatment) %>% 
  left_join(control_vars) %>% 
  mutate(inProject_Post = in_project * Post)
  
library(lfe)
did_model_hv <- # Sunflower, VegetablesANDFruits, Oilseeds, Sugarcane, cashcrop_yn
  felm(Sugarcane    ~ in_project + Post + inProject_Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments | hh_id, # Adding fixed effects
       data = df2)

summary(did_model_hv)
sjPlot::tab_model(did_model_hv ,  show.se = T,digits = 5,     show.stat  = TRUE )

#### TRADITIONAL CROP   ____________________________________________________ ----
# share of farmers cultivating

traditional_2022 <- plots_crop_2022 %>% # in DF.22.R
  # filter(season != "kharif_2021") %>% 
  select(hh_id,common_n2_family) %>% distinct() %>% 
  mutate(value = 1) %>%
  pivot_wider(
    names_from = common_n2_family, 
    values_from = value,
    values_fill = list(value = 0)) %>% 
  select(hh_id,"Toor","Bengal gram","Sorghum/jowar","Greengram") %>% 
  mutate(traditional_crop_total = rowSums(select(., Toor:Greengram))) %>% 
  mutate(traditional_crop_yn = ifelse(traditional_crop_total==0,0,1)) %>% 
  rename(Sorghum_jowar =`Sorghum/jowar`, Bengal_gram = `Bengal gram`) %>% 
  mutate(Post=1,Year=2022)

BL_2015_16_crop_IRsource_IRmethod %>% count(crop_common)
traditional_2015 <- BL_2015_16_crop_IRsource_IRmethod %>% 
  # filter(season != "rabi_2015_16") %>% 
  select(hh_id,crop_common) %>% distinct() %>% 
  mutate(value = 1) %>%
  pivot_wider(
    names_from = crop_common, 
    values_from = value,
    values_fill = list(value = 0)) %>% 
  select(hh_id,"Toor","Bengal_gram","Sorghum_jowar","Greengram") %>% 
  mutate(traditional_crop_total = rowSums(select(., Toor:Greengram))) %>% 
  mutate(traditional_crop_yn = ifelse(traditional_crop_total==0,0,1)) %>% 
 mutate(Post=0,Year=2015)

#    summary stat  ----
rbind(traditional_2015,traditional_2022) %>% 
  left_join(treatment)%>%
  pivot_longer(
    cols = c(Toor, Bengal_gram, Sorghum_jowar, Greengram, 
             traditional_crop_total, traditional_crop_yn),
    names_to = "crop",
    values_to = "value"
  ) %>% group_by(crop,Year, in_project) %>% 
  summarise (N=n(),n=sum(value)) %>% ungroup() %>% 
  mutate(pct=n/N) %>%  kableExtra:: kable()

#    DiD           -----
#     DiD regression model to traditional crops   HH FE
traditional_df = rbind(traditional_2015,traditional_2022) %>% 
  left_join(treatment) %>% 
  left_join(control_vars) 

mod_toor <- # Toor, Bengal_gram, Sorghum_jowar, Greengram, cashcrop_yn
  lm(Toor    ~ in_project + Post + in_project * Post  + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments+ factor(hh_id), # Adding fixed effects
       data = traditional_df)

# summary(did_model)
# sjPlot::tab_model(did_model , show.se= T,digits= 5, show.stat= T )
library(modelsummary)
modelsummary(list("Toor"=mod_toor,"Chickpea"=mod_bg,"Sorghum (Jowar)"=mod_sj,"Greengram"=mod_gg), 
             fmt = fmt_decimal(3, 3), 
             statistic = c("{std.error}","{p.value}"), # no more parentheses
             stars = F,
             coef_map=c("Post"= "Post","in_project:Post"="In Project × Post"))






#### CULTIVATION IRRIGATION LAND   ________________________________________ ----
# Cultivated area (acres)

# Make sure that the traditinal crop categorize in the same season
plots_crop_2022 %>% select(1,crop_name,season) %>% 
  distinct() %>% count(crop_name) %>%  arrange(desc(n))

           # Bengal gram V V Rabi crop. 
           # chickpea or chick pea or Chanaa
           # sown from September to November.  harvested in April.
plots_crop_2022 %>% select(1,crop_name,season) %>% 
  filter(crop_name== "Bengal gram") %>% distinct() %>% count(season)
BL_2015_16_crop_IRsource_IRmethod %>% select(1,crop_name,season) %>% 
  filter(crop_name=="Bengal_gram") %>% distinct() %>% count(season)

           # Toor V X Kharif crop. 
           # Red Gram or Pigeonpea or Arhar Dal or Toor Dal
           #  sowing-MAY | harvested between December and January
plots_crop_2022 %>% select(1,crop_name,season) %>% filter(crop_name== "Toor") %>% 
  distinct() %>% count(season)  # kharif_2021
BL_2015_16_crop_IRsource_IRmethod %>% select(1,crop_name,season) %>% 
  filter(crop_name=="Toor") %>% distinct() %>% 
  count(season) # rabi_2015_16

           # Greengram  Kharif
plots_crop_2022 %>% select(1,crop_name,season) %>% filter(crop_name== "Greengram") %>% 
  distinct() %>% count(season)  # kharif_2021
BL_2015_16_crop_IRsource_IRmethod %>% select(1,crop_name,season) %>% 
  filter(crop_name=="Greengram") %>% distinct() %>% 
  count(season) # kharif_2015

           # Sorghum_jowar  rabi
plots_crop_2022 %>% select(1,crop_name,season) %>% filter(crop_name== "Sorghum (jowar)") %>% 
  distinct() %>% count(season)  # rabi_2021_22
BL_2015_16_crop_IRsource_IRmethod %>% select(1,crop_name,season) %>% 
  filter(crop_name=="Sorghum_jowar") %>% distinct() %>% 
  count(season) # rabi_2015_16


# cultivated_land_2022
cultivated_land_2022 <- a_irri_rain_method %>% 
  select(hh_id,season,plotID,irri_method)%>% distinct() %>% 
  left_join( a_plots_size  %>% select(hh_id, plotID, acres)
  ) %>% 
  mutate(acre_drip=ifelse(irri_method=="drip",acres,0),
         acre_ir=ifelse(irri_method=="rain",0,acres)
         ) %>% 
  group_by(hh_id, season ) %>% summarise(
    acre_ir=sum(acre_ir,na.rm = T),
    acre_drip=sum(acre_drip,na.rm = T),
    acre_cult=sum(acres,na.rm = T)) %>% ungroup(
    )%>% 
  mutate(season = sub("_.*", "", season)) %>% 
  group_by(hh_id, season ) %>% summarise(
    acre_ir=mean(acre_ir,na.rm = T),
    acre_drip=mean(acre_drip,na.rm = T),
    acre_cult=mean(acre_cult,na.rm = T)) %>% ungroup(
    )%>% 
  left_join(land_holding_2022 %>% select(hh_id,land_holding)) %>% 
  mutate(pct_cult_land= acre_cult/land_holding,
         pct_ir_land=acre_ir/acre_cult,
         pct_drip_land=acre_drip/acre_cult ) %>% 
  mutate(Post=1,Year=2022)

# cultivated_land_2015
cultivated_land_2015 <-  BL_2015_16_crop_IRsource_IRmethod %>% 
  mutate(season= # Remove toor to Kharif
           ifelse(crop_name == "Toor" ,"kharif_2015",season) ) %>% 
  select(hh_id,season,plot_acre, irri_method
         ) %>% 
  mutate(acre_drip=ifelse(irri_method=="Drip",plot_acre,0),
         acre_ir=ifelse(irri_method=="Rain",0,plot_acre)
  ) %>%   
  group_by(hh_id, season ) %>% summarise(
    acre_ir=sum(acre_ir,na.rm = T),
    acre_drip=sum(acre_drip,na.rm = T),
    acre_cult=sum(plot_acre,na.rm = T)) %>% ungroup(
    ) %>% 
  mutate(season = sub("_.*", "", season)) %>% 
  group_by(hh_id, season ) %>% summarise(
    acre_ir=mean(acre_ir,na.rm = T),
    acre_drip=mean(acre_drip,na.rm = T),
    acre_cult=mean(acre_cult,na.rm = T)) %>% ungroup(
    )%>% 
  left_join(land_holding_2016) %>% filter(!is.na(land_holding) # omit 1 hh
                                          ) %>%
  mutate(pct_cult_land= acre_cult/land_holding,
         pct_ir_land=acre_ir/acre_cult,
         pct_drip_land=acre_drip/acre_cult ) %>% 
  mutate(Post=0,Year=2015)

#    summary stat  ----
cultBIND <- rbind(cultivated_land_2015,cultivated_land_2022)
# hist(cultBIND$acre_cult)
# max(cultBIND$acre_cult)
quantile(cultBIND$acre_drip, 0.995)
quantile(cultBIND$acre_ir , 0.995)
quantile(cultBIND$acre_cult, 0.995)
cultBIND$acre_drip <- ifelse(cultBIND$acre_drip > 7.1 , NA, cultBIND$acre_drip)
cultBIND$acre_ir <- ifelse(cultBIND$acre_ir  > 17.9, NA, cultBIND$acre_ir )
cultBIND$acre_cult <- ifelse(cultBIND$acre_cult > 53.8, NA, cultBIND$acre_cult)

cultBIND %>% 
  left_join(treatment)%>%
  select(hh_id,season,Year,Post, in_project,acre_ir:acre_cult) %>% 
  pivot_longer(
    cols = c(acre_ir:acre_cult),
    names_to = "land",
    values_to = "value"
  ) %>% group_by(Year,season,land, in_project) %>% 
  summarise (N=n(),Mean=mean(value,na.rm = T)) %>% ungroup() %>% 
  kableExtra:: kable()

#    DiD           -----
#     DiD reg to acre_drip / acre_ir / acre_cult  HH as FE 
library(readr)
rmtl_InOut <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_InOut.csv")

df4 <- 
  cultBIND %>% left_join(treatment) %>% 
  mutate(inProject_Post = in_project * Post) %>% 
  left_join(control_vars)%>% 
  left_join(
    rmtl_InOut %>% select(hh_id,distance_km,around_boundary,south1_north0)
  )

df4_kharif <- df4 %>% filter(season =="kharif" )
df4_rabi <- df4 %>% filter(season == "rabi" ) 


# library(lfe)
did_model_df4 <-  # acre_drip acre_ir acre_cult
  felm(acre_drip  ~ in_project + Post + inProject_Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments | hh_id, # Adding fixed effects
       data = df4a_rabi)
summary(did_model_df4)
sjPlot::tab_model(did_model_df4 ,  show.se = T,digits = 5, show.stat  = F )


# Gross Cropped Area (GCA) / Cropping_Intensity
df_gca <- 
  cultBIND %>% 
  select( hh_id,acre_cult,land_holding,Post, Year) %>%   
  group_by(hh_id,Year) %>% 
  mutate( gca=sum(acre_cult,na.rm = T),
          maxNCA=land_holding*2,
          nca=ifelse(gca > maxNCA,gca,land_holding)) %>%ungroup() %>% 
  select(hh_id, Post,Year, gca, nca ) %>% distinct() %>% 
  group_by(hh_id,Year,Post) %>% 
  mutate( Cropping_Intensity=(gca/nca)*100) %>% ungroup() %>% 
  left_join(treatment) %>% 
  mutate(inProject_Post = in_project * Post) %>% 
  left_join(control_vars) %>% 
  left_join(
    rmtl_InOut %>% select(hh_id,distance_km,around_boundary,south1_north0))

df_gcaA <- df_gca 

df_gcaA %>% 
  left_join(treatment)%>%
  select(hh_id,Year, in_project,gca, Cropping_Intensity) %>% 
  pivot_longer(
    cols = c(gca, Cropping_Intensity),
    names_to = "land",
    values_to = "value"
  ) %>% group_by(Year,land, in_project) %>% 
  summarise (N=n(),Mean=mean(value,na.rm = T)) %>% ungroup() %>% 
  kableExtra:: kable()

  # library(lfe)
did_gcaA <-  # gca Cropping_Intensity
  felm(gca  ~ in_project + Post + inProject_Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments | hh_id, # Adding fixed effects
       data = df_gcaA)
summary(did_gcaA)
sjPlot::tab_model(did_gcaA ,  show.se = T,digits = 5, show.stat  = F )


#### Share of area cultivated (%) ----

#    summary stat  ----

land_pct <- rbind(cultivated_land_2015,cultivated_land_2022) %>% 
  mutate(pct_cult_land=ifelse(pct_cult_land>1,1,pct_cult_land))
# [quantile 0.995]
land_pct$acre_drip <- ifelse(land_pct$acre_drip > 7.1 , NA, land_pct$acre_drip)
land_pct$acre_ir <- ifelse(land_pct$acre_ir  > 17.9, NA, land_pct$acre_ir )
land_pct$acre_cult <- ifelse(land_pct$acre_cult > 53.8, NA, land_pct$acre_cult)

land_pct %>% 
  left_join(treatment)%>%
  select(hh_id,season,Year,Post, in_project,pct_cult_land:pct_drip_land) %>% 
  mutate(pct_cult_land=ifelse(pct_cult_land>1,1,pct_cult_land)) %>% 
  pivot_longer(
    cols = c(pct_cult_land:pct_drip_land),
    names_to = "land",
    values_to = "value"
  ) %>% group_by(Year,season,land, in_project) %>% 
  summarise (N=n(),Mean=mean(value,na.rm = T)) %>% ungroup() %>% 
  kableExtra:: kable()

#     DiD ----
#     DiD reg to "pct_drip_land" / "pct_ir_land" / "pct_cult_land" + HH as FE 
df4 <- 
  land_pct %>% left_join(treatment) %>% 
  mutate(inProject_Post = in_project * Post) %>% 
  left_join(control_vars)

df4_kharif <- df4 %>% filter(season =="kharif" )
df4_rabi <- df4 %>% filter(season == "rabi" ) 

# library(lfe)
did_model_df4 <-  # "pct_drip_land  pct_ir_land  pct_cult_land
  felm(pct_drip_land  ~ in_project + Post + inProject_Post + 
         hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
         housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
         total_livestock + total_farm_equipments | hh_id, # Adding fixed effects
       data = df4_rabi)
summary(did_model_df4)
sjPlot::tab_model(did_model_df4 ,  show.se = T,digits = 5, show.stat  = F )


#### YIELD		         ----					



total_acre_22 <- a_plots_size %>% 
  select(hh_id,plotID,acres)

# yield_per_acre_2022

yield_per_acre_2022 <- 
  a_total_yield %>% 
  select(hh_id, season,plotID, kg_crop ) %>% 
  left_join(total_acre_22) %>% 
  filter(kg_crop>0) %>% 
   mutate(season = sub("_.*", "", season)) %>% 
  group_by(hh_id, season) %>% 
  summarise(kg_crop=sum(kg_crop,na.rm = T),acres=sum(acres,na.rm = T),.groups = "drop" ) %>% 
  mutate(kg_per_acre= kg_crop/acres) %>% 
  mutate(Post=1)

yield_per_acre_2022_99 <- quantile(yield_per_acre_2022$kg_per_acre, 0.99)
yield_per_acre_2015_99 <- quantile(yield_per_acre_2015$kg_per_acre, 0.99)

#    summary stat  ----
yield_per_acre_2022B <- yield_per_acre_2022 %>% 
  mutate( kg_per_acre= ifelse(kg_per_acre > yield_per_acre_2022_99 , NA, kg_per_acre))

yield_per_acre_2015B <- 
  yield_per_acre_2015 %>% mutate(Post=0) %>% 
  mutate( kg_per_acre= ifelse(kg_per_acre > yield_per_acre_2015_99 , NA, kg_per_acre))

yield_per_acre <-  rbind(yield_per_acre_2015B,yield_per_acre_2022B)

yield_per_acre %>% group_by(season,Post ) %>% 
  summarise(Mean=mean(kg_per_acre,na.rm=T ),
            SD=sd(kg_per_acre,na.rm=T),
            Min=min(kg_per_acre,na.rm=T),
            Max=max(kg_per_acre,na.rm=T),
            .groups = 'drop') 





#    DiD           -----
#     DiD regression model to yield per acre   HH FE
yield_per_acre_df = yield_per_acre %>% 
  left_join(treatment) %>% 
  left_join(control_vars) 

yield_kharif_df <- yield_per_acre_df %>% filter(season=="kharif")
yield_rabi_df <- yield_per_acre_df %>% filter(season=="rabi")

mod_yield_rabi <- 
  lm(kg_per_acre ~ in_project + Post + in_project * Post  + 
       hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
       housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
       total_livestock + total_farm_equipments+ factor(hh_id), # Adding fixed effects
     data = yield_rabi_df)

# summary(did_model)
# sjPlot::tab_model(did_model , show.se= T,digits= 5, show.stat= T )
library(modelsummary)
modelsummary( list("Rabi | Yield per Acre (in Kg)"=mod_yield_rabi,"Kharif | Yield per Acre (in Kg)"=mod_yield_kharif), 
             fmt = fmt_decimal(3, 3), 
             statistic = c("{std.error}","{p.value}"), # no more parentheses
             stars = F,
             coef_map=c("Post"= "Post","in_project:Post"="In Project × Post"))




# YIELD Sold Kept Lost             ----					
# How much of the yield was [%]	# [percentage at Season-Crop]
# [L52] Sold # [L53] Kept for HH consumption # [L54] Lost in post-harves

L52_stored <- 
  rmtl_srvy22 %>% select(hh_id, starts_with( "L52")) %>%
  select(hh_id, contains("stored")) %>% 
  pivot_longer(!hh_id, names_to = "POV", values_to = "count") %>% 
  mutate(yld_status="stored")

L52_Sold <- 
  rmtl_srvy22 %>% select(hh_id, starts_with( "L52")) %>%
  select(-contains("stored", ignore.case = TRUE)) %>%  # then drop the “stored” ones
  pivot_longer(!hh_id, names_to = "POV", values_to = "count") %>% 
    mutate(yld_status="sold")

L53_Kept <- 
  rmtl_srvy22 %>% select(hh_id, starts_with( "L53")) %>%
  pivot_longer(!hh_id, names_to = "POV", values_to = "count")%>% 
  mutate(yld_status="kept")
  
L54_Lost <- 
  rmtl_srvy22 %>% select(hh_id, starts_with( "L54")) %>%
  pivot_longer(!hh_id, names_to = "POV", values_to = "count") %>% 
  mutate(yld_status="lost")

library(stringr)
L54_yield_status <- 
  rbind(L52_Sold,L52_stored,L53_Kept,L54_Lost) %>% 
  mutate(
    season = case_when(
      str_detect(POV, regex("rab",   ignore_case = TRUE)) ~ "rabi_2021_22",
      str_detect(POV, regex("kha22", ignore_case = TRUE)) ~ "kharif_2022",
      str_detect(POV, regex("kha",   ignore_case = TRUE)) ~ "kharif_2021",
      TRUE~ NA_character_))

yield_status <- L54_yield_status %>% 
  filter(!is.na(count) ) %>% 
  mutate(season = sub("_.*", "", season)) %>% 
  group_by(hh_id,yld_status,season) %>% 
  summarise(prt=mean(count), .groups = "drop")

yield_status %>% 
  left_join(treatment) %>% 
  group_by(season,in_project,yld_status) |>rmtl_midline2018 %>% select(  contains("d55"))
rmtl_midline2018 %>% select(  contains("d60"))
rmtl_midline2018 %>% select(  contains("d60a"))
rmtl_midline2018 %>% select(  contains("d60b"))

  summarise(prt=mean(prt)) 
            


#| D29	55	What was the total yield?
#| D31.2.1	60a	Sold
#| D31.2.2	60b	Kept for HH consumption
#| D31.2.3	60c	Lost in post-harvest
#| D31.2.4	60d	Other (specify)








# % Households using improved seeds Year




# # Visualization -----

df %>%
  group_by(Treatment, Post) %>%
  summarise(Mean_Y = mean(ir_use, na.rm = TRUE)) %>%
  mutate(Group = ifelse(Treatment == 1, "Treatment (Inside Project)", "Control (Outside Project)"),
         Time = ifelse(Post == 1, "Endline (2022)", "Baseline (2016)")) %>% 
  
  ggplot(aes(x = Time, y = Mean_Y, group = Group, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Difference-in-Differences: Irrigation Adoption",
       x = "Time",
       y = "Mean DI Adoption Rate") +
  scale_color_manual(values = c("Treatment (Inside Project)" = "darkblue", 
                                "Control (Outside Project)" = "gray")) +
  theme_classic()

























# Load necessary libraries
library(tibble)
library(htmlTable)

# Extract coefficients and standard errors from the model
model_summary <- summary(did_model)
coef_table <- model_summary$coefficients

# Create a data frame for the table
results_table <- data.frame(
  Predictors = rownames(coef_table),
  Estimates = round(coef_table[, "Estimate"], 3),
  `Std. Error` = round(coef_table[, "Std. Error"], 3)
)


# Combine Estimates and Std. Errors into a formatted table
results_table <- results_table %>%
  mutate(`Estimates with SD` = paste0(Estimates, "\n(", `Std..Error`, ")")) %>%
  select(Predictors, `Estimates with SD`)

# Convert the table into an HTML format
html_table <- htmlTable(
  results_table,
  rnames = FALSE,
  header = c("Predictors", "Estimates\n(SD)"),
  caption = "Regression Results"
)

# Save the HTML table to a file
html_file <- "Regression_Results.html"
writeLines(html_table, html_file)

# Notify the user
cat("Table saved as", html_file)














