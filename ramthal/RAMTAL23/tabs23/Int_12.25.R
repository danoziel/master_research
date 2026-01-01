library(dplyr)
library(summarytools )

library(haven)
library(readxl)
c("#a2c4c9","#76a5af" ,"#45818e")

# [A] Damages and maintenance [] [] [] [] [] [] [] [] [] [] [] [] ----

# [B] Irregular Water supply  [] [] [] [] [] [] [] [] [] [] [] [] ----

# [C] Using Ramthal system as a water source [] [] [] [] [] [] [] ----

## DF [proximity_to_outlet]  ----
proximity_to_outlet <- 
  rmtl_cntrl_vars %>% select(hh_id ,location_on_pipe) %>%
  left_join(rmtl_2024 %>% select( hh_id,farmers_B4_u ) ) %>% 
  mutate(farmers_B4_u=ifelse(farmers_B4_u=="dont_know",NA,farmers_B4_u )) %>% 
  mutate(
    proximity_location = if_else(
      is.na(location_on_pipe) | location_on_pipe == -998,
      suppressWarnings(as.numeric(farmers_B4_u)),
      location_on_pipe)
  ) %>% select(-location_on_pipe ,-farmers_B4_u) %>%
  mutate(proximity_near1_far0=ifelse(proximity_location < 4,1,0)) 

## DF [cha_C]

cha_C <- 
  rmtl_InOut %>% select(hh_id,mm4,in1_out0, ir_use ) %>% 
  filter(in1_out0==1,mm4==1) %>% 
  left_join(proximity_to_outlet) %>% 
  left_join(a_source_irri %>% select(hh_id,source_ramthal)) %>%
  left_join(rmtl_srvy22 %>% select(hh_id, mw4b,m35)) %>% 
  left_join(pipe_status) %>% 
  left_join(df_ir_use_total_years ) %>% 
  left_join(df_ir_use_last_year) %>% 
  
  mutate(source_ramthal=ifelse(source_ramthal=="ramthal",1,0))%>% 
  rename(last_year_use=mw4b, pipe_status_sur22 = m35)
  

######## % of users - Ramthal as source  ----
### % of Farmers Using Ramthal as water source by Location on pipe


summary_data <- cha_C %>% 
  mutate(proximity_location_f = factor(proximity_location)) %>%
  group_by(proximity_location_f) %>%
  summarise(
    N = n(),
    mean_source = mean(source_ramthal),
    se = sqrt((mean_source * (1 - mean_source)) / N),
    t_val = qt(0.975, df = max(0, N - 1)),
    ci_lower = mean_source - (t_val * se),
    ci_upper = mean_source + (t_val * se),
    .groups = 'drop' # Ungroup the data after summarising
  ) %>%
  mutate(
    ci_lower = if_else(ci_lower < 0, 0, ci_lower),
    ci_upper = if_else(ci_upper > 1, 1, ci_upper)
  )


# 500 X 250
summary_data %>% filter(as.integer(proximity_location_f) < 14) %>% 
  ggplot(aes(x = proximity_location_f, y = mean_source)) +
  geom_col(fill = "#45818e") +
  geom_text(aes(label = scales::percent(mean_source, accuracy = 1)),
            position = position_dodge(width = 0.9),vjust = 2,size = 3,color = "white"
            ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.2,color = "gray75",
  ) +
  labs(
    x = "Location on Pipeline",
    y = "% of HH",
    title = "Ramthal Source by Proximity to water outlet (95% CI)",
    caption = paste0("N = ", sum(summary_data$N)) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = scales::percent # Converts decimals to percentage format
  ) +
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 11),
        panel.grid = element_blank())

summary_data$N


######## % of users -  irrigation  ----
### % of Farmers irrigate by Location on pipe


summary_data <- cha_C %>% 
  mutate(proximity_location_f = factor(proximity_location)) %>%
  group_by(proximity_location_f) %>%
  dplyr::summarise(
    N = n(),
    Mean = mean(ir_use),
    se = sqrt((Mean * (1 - Mean)) / N),
    t_val = qt(0.975, df = max(0, N - 1)),
    ci_lower = Mean - (t_val * se),
    ci_upper = Mean + (t_val * se), .groups = 'drop'
  ) %>%
  mutate(
    ci_lower = if_else(ci_lower < 0, 0, ci_lower),
    ci_upper = if_else(ci_upper > 1, 1, ci_upper)
  )

# 500 X 250 ----
summary_data %>% filter(as.integer(proximity_location_f) < 14) %>% 
  ggplot(aes(x = proximity_location_f, y = Mean)) +
  geom_col(fill = "#45818e") +
  geom_text(aes(label = scales::percent(Mean, accuracy = 1)),
            position = position_dodge(width = 0.9),vjust = 2,size = 3,color = "white"
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.2,color = "gray75",
  ) +
  labs(
    x = "Location on Pipeline",
    y = "% of HH",
    title = "Irrigation by Proximity to water outlet (95% CI)",
    caption = paste0("N = ", sum(summary_data$N)) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = scales::percent 
  ) +
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 11),
        panel.grid = element_blank())

summary_data$N





######## Years of water use for irrigation  ----

df_ir_use_total_years %>% 
  left_join(rmtl_InOut %>% select(hh_id,mm4,mm5,in1_out0)) %>% 
  filter(in1_out0==1) %>% 
  filter(mm4==1) %>% 
  count(ir_total_years) %>% 
  mutate(N=sum(n),pct= n/N*100) 

library(Rmisc)
summary_data <- cha_C %>% 
  select(proximity_location,ir_total_years) %>% 
  filter(!is.na(proximity_location)) %>% 
  mutate(proximity_location = 
           ifelse(proximity_location == 13,12,
           ifelse(proximity_location %in%c(14:16),13,proximity_location ))
         ) %>% 
  filter(!is.na(proximity_location)) %>% 
  mutate(proximity_location_f = factor(proximity_location)) %>%
  group_by(proximity_location_f) %>%
  dplyr::summarise(
    N = n(),
    Mean =  CI(ir_total_years)[2],
    ci_lower =  CI(ir_total_years)[1],
    ci_upper =  CI(ir_total_years)[3],  .groups = 'drop'
  )


summary_data %>% filter(as.integer(proximity_location_f) < 14) %>% 
  ggplot(aes(x = proximity_location_f, y = Mean)) +
  geom_col(fill = "#45818e") +
  geom_text(aes(label = round(Mean, 2)),
            position = position_dodge(width = 0.9),vjust = 2,size = 3,color = "white"
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.2,color = "gray75",
  ) +
  labs(
    x = "Location on Pipeline",
    y = "% of HH",
    title = "Avg. Years of water usage (95% CI)",
    caption = paste0("N = ", sum(summary_data$N)) 
  ) +
  scale_y_continuous(
    breaks = seq(0,4, 1,)  ) +
  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 11),
        panel.grid = element_blank())

######## Have farmers before you on the pipe ever taken all the water? 

# OPINION mm10	-----
# Has it ever happened to you that farmers "before" you have used up a lot of the water from the pipe, so you did not have enough?

# 1	It has never happened
# 2	It happens about once a season
# 3	It happens several times a season
# 4	It happens on a constant basis
# 5	Some seasons it happens and some it doesn't


MM10 <- rmtl_srvy22   %>% 
  select(hh_id,mm10) %>% 
  mutate(mm10= ifelse(mm10 %in% c(2:3),"somtimes",
               ifelse(mm10==1,"naver",
               ifelse(mm10==4,"constantly",NA )))) %>% 
  filter(!is.na(mm10)) %>% 
  left_join(
    cha_C %>% select(hh_id,proximity_location) 
  ) %>% 
  mutate(proximity_location=ifelse(proximity_location>44,NA,proximity_location)  ) %>%  
  group_by(mm10) %>% 
  summarise(n=n(),Mean=mean(proximity_location,na.rm=T))




######## Are farmers "near the outlet" less likely to abandon the project?
  
# farmer who still make use in the last year 2022

df_still <- 
  irrigation_BL_to_22 %>% 
  left_join(rmtl_InOut %>% select(hh_id,mm4,mm5,in1_out0)) %>% 
  filter(in1_out0==1,mm4==1) %>% 
  select(hh_id,ir_use_2022) %>% 
  right_join(cha_C %>% select(hh_id ,proximity_location)) %>% 
  filter(!is.na(ir_use_2022)) %>% 
  mutate(proximity_location = 
           ifelse(proximity_location == 13,12,
           ifelse(proximity_location %in%c(14:16),13,proximity_location ))
  ) 

# TABLE  
df_still %>% count(proximity_location,ir_use_2022) %>% 
  group_by(proximity_location) %>% mutate(N=sum(n),n/N*100) %>% 
  filter(ir_use_2022==1, proximity_location<14)


# PLOT
library(Rmisc)

df_still %>% 
  mutate(proximity_location_f = factor(proximity_location)) %>%
  group_by(proximity_location_f) %>%
  dplyr::summarise(
    N = n(),
    Mean =  CI(ir_total_years)[2],
    ci_lower =  CI(ir_total_years)[1],
    ci_upper =  CI(ir_total_years)[3],  .groups = 'drop'
  )

library(Rmisc)
summary_data <- df_still %>% 
  filter(!is.na(proximity_location)) %>% 
  mutate(proximity_location_f = factor(proximity_location)) %>%
  group_by(proximity_location_f) %>%
  dplyr::summarise(
    N = n(),
    Mean =  CI(ir_use_2022)[2],
    ci_lower =  CI(ir_use_2022)[1],
    ci_upper =  CI(ir_use_2022)[3],  .groups = 'drop'
  )


summary_data %>% filter(as.integer(proximity_location_f) < 14) %>% 
  ggplot(aes(x = proximity_location_f, y = Mean)) +
  geom_col(fill = "#45818e") +
  geom_text(aes(label = round(Mean, 2)),
            position = position_dodge(width = 0.9),vjust = 2,size = 3,color = "white"
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.2,color = "gray75",
  ) +
  labs(
    x = "Location on Pipeline",
    y = "% of HH",
    title = "Ramthal's water usage in last year 2022 (95% CI)",
    caption = paste0("N = ", sum(summary_data$N)) 
  ) +

  theme_minimal(base_family = "serif")+
  theme(axis.text.x = element_text(size = 11),
        panel.grid = element_blank())




# [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] ----


df_ir_use_last_year <- 
  irrigation_BL_to_22 %>% 
  select(hh_id, starts_with("ir_use_2"),-ir_use_2021_22 
         ) %>% 
  mutate(last_use = ifelse(ir_use_2022 == 1,2022,
                ifelse(ir_use_2021 == 1,2021,
                ifelse(ir_use_2020 == 1,2020,
                ifelse(ir_use_2019 == 1,2019,
                ifelse(ir_use_2018 == 1,2018,
                ifelse(ir_use_2017 == 1,2017,0))))))
         ) %>% 
  select(hh_id,last_use)

df_ir_use_last_year %>% 
  left_join(rmtl_InOut %>% select(hh_id,mm4,mm5,in1_out0)) %>% 
  filter(in1_out0==1,last_use>0) %>% 
  # filter(mm4==1) %>% 
  count(last_use) %>% mutate(N=sum(n),n/N*100)

# [mw4b]	What was the last year you use of the water? 
rmtl_srvy22 %>% select(hh_id, mw4b) %>% 
  count(mw4b) %>% filter(!is.na(mw4b)) %>% 
  mutate(N=sum(n),n/N*100)



# total years of irrigation
df_ir_use_total_years <- 
  irrigation_BL_to_22 %>% 
  select(hh_id, starts_with("ir_use_2"),- ir_use_2021_22
  ) %>% 
  select(hh_id,starts_with("ir")) %>% 
  pivot_longer(-hh_id,names_to = "year",values_to = "use") %>% 
  group_by(hh_id) %>% 
  summarise(ir_total_years=sum(use,na.rm = T))


  
  



dt <- irrigation_BL_to_22 %>% 
  select(hh_id, starts_with("ir_use_2"),- ir_use_2021_22
  ) %>% 
  pivot_longer(-hh_id,names_to = "year",values_to = "usage") %>% 
  left_join(rmtl_InOut %>% select(hh_id,mm4,mm5,farmers_hh)) %>% 
  filter(!is.na(farmers_hh)) %>% 
  group_by(year,farmers_hh) %>% summarise(n=sum(usage,na.rm = T)) %>% 
  mutate(N=ifelse(farmers_hh== "inside_ramthal",929,645),
         pct=n/N
         
         )


# _________________________________________ ----


hh_cult_21_22 <- a_plots_crop %>% select(hh_id) %>% distinct()

smpl_in_mm4 <- 
  rmtl_InOut %>% select(in1_out0,hh_id,mm4,mm5) %>%
  right_join(hh_cult_21_22) %>% 
  filter(in1_out0==1) %>%
  filter(mm4==1) %>%
  select(hh_id)

cv = # FREQ cultivated crop 2021-2022
  a_plots_crop %>% select(hh_id ,season,plotID  ,crop_common ) %>% 
  right_join(smpl_in_mm4)
cv$season[cv$season=="kha"] <- "kharif_2021"
cv$season[cv$season=="rabi"] <- "rabi_2021_22"
cv$season[cv$season=="KHA22"] <- "kharif_2022"

cv_3 <- # ACRE cultivated crop 2021-2022
  a_plots_crop %>% select(hh_id, season,plotID  ,crop_common ) %>% 
  left_join(a_plots_size %>% select(hh_id,plotID,acres)) %>% 
  right_join(smpl_in_mm4) %>% 
  group_by(hh_id ,season, plotID  ) %>% 
  mutate(n=n(), Acre =acres/n ) %>% ungroup()
cv_3$season[cv_3$season=="kha"] <- "kharif_2021"
cv_3$season[cv_3$season=="rabi"] <- "rabi_2021_22"
cv_3$season[cv_3$season=="KHA22"] <- "kharif_2022"

# % of acre crop/ season
cv_3 %>%  group_by(season,crop_common) %>% 
  summarise(acre=sum(Acre)) %>% 
  group_by(season) %>% 
  mutate(acre_season=sum(acre),pct=acre/acre_season) %>% 
  kable() %>% kable_minimal()

attr(rmtl_srvy22$l_plot_status_1, "labels")
cv_land_holding <- 
  a_plots_size %>% 
  filter(plotStatus %in% c("2", # Currently owned plots
                           "3", # leased In 
                           "4", # Fallow Land 
                           "5") ) %>% # Partial Sold
  right_join(smpl_in_mm4) %>% 
  summarise(land_holding = sum(acres, na.rm = T)
                                ) %>% pull()

cv_3 %>%
  filter(season != "kharif_2022") %>% 
  group_by(hh_id,season) %>% summarise(acre = sum(Acre, na.rm = T)) %>% 
  group_by(season) %>% summarise(acre = sum(acre, na.rm = T) ) %>% 
  mutate(pct=acre/ cv_land_holding)

###
Mung_kharif_2021 <- cv_3_ir %>%  
  filter(crop_common=="Greengram",season=="kharif_2021" ) %>% 
  group_by(season) %>% 
  summarise(ir_acre=sum(Acre)) %>% pull(ir_acre)

cv_3_ir <- 
  cv_3 %>% left_join(
  a_irri_rain_method %>% 
    select(hh_id , season ,  plotID,irri_method ) 
) %>% 
  filter(irri_method != "rain")

Mung_kharif_2021 <- cv_3_ir %>%  
  filter(crop_common=="Greengram",season=="kharif_2021" ) %>% 
  summarise(ir_acre=sum(Acre)) %>% pull(ir_acre)

cv_3_ir %>%  group_by(season,crop_common) %>% 
  summarise(ir_acre=sum(Acre)) %>%
  group_by(season) %>% 
  summarise(acre_ir_season=sum(ir_acre)) %>% 
  mutate(Acre_ir_season=ifelse(season=="rabi_2021_22",
                   acre_ir_season+Mung_kharif_2021,
                   acre_ir_season))


###
sWS <- a_source_irri %>% 
  mutate(self_WS=
           ifelse(source_borwell =="borwell","sws",
                  ifelse(source_canal=="canal","sws",
                         ifelse(source_pond == "pond_Owell","sws",
                                ifelse(source_ramthal=="ramthal","ramthal","rain"))))
         )

sWS |> count(self_WS) %>% mutate(n/1574)

sWS |> right_join(smpl_in_mm4) %>% count(self_WS) %>% mutate(n/585)





