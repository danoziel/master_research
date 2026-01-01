# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(summarytools)
library(ggplot2)

df_ramthal_01012026 <- irrigation_BL_to_22 %>% left_join(rmtl_cntrl_vars)
library(writexl)
write_xlsx(df_economic, "C:/Users/Dan/Downloads/df_economic.xlsx")

## General DFs  ________________________________________________________ _ ----
library(readr)
BL_2015_16_crop_IRsource_IRmethod <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL_2015_16_crop_IRsource_IRmethod.csv")

rmtl_16_18_22_sample <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_16_18_22_sample.csv")

demographic_vars_2016 <- # baseline vars
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/demographic_vars_2016.csv")


library(readxl)
elevation_points_tbl<- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_tbl.xlsx")

elevation_points_tbl <- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_tbl.xlsx")

Ramthal_dist_elev <- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/Ramthal_dist_elev.xlsx")

#
#________________
control_vars <- #  1,612
  demographic_vars_2016 %>% #former demog_vars
  mutate(hh_haed_edu_level=
           ifelse(is.na(hh_haed_edu_level),0,
                  hh_haed_edu_level ) ) 
#
#________________
dist_elev_var  <-  # former [dist_var]
  Ramthal_dist_elev  %>% left_join(hh_2022) %>% 
  select(in1_out0,hh_id, dist_to_boundary_m, elevation_m) %>% # former var dist_to_south_m
  mutate(dist_Km_boundary =  dist_to_boundary_m*0.001,
         dist_Km_boundary = ifelse(in1_out0==0, dist_Km_boundary*-1,dist_Km_boundary)
         ) %>% 
  mutate(elevation = elevation_m-506,
         elevation= ifelse(in1_out0==0, elevation*-1 ,elevation))%>% 
  select(-in1_out0)
#
#________________
geo_var <- #  1,612
  rmtl_InOut %>% 
  select(hh_id, elevation , south1_north0, a5) %>% 
  rename(elev_cat = elevation,village=a5) %>% 
  mutate(elev_cat=ifelse(elev_cat=="7+",7,elev_cat),
         elev_cat=as.numeric(elev_cat))
#
#________________
Qmm2=rmtl_srvy22 %>% select(hh_id,mm2) %>% 
  mutate(im_in_project = ifelse(mm2==1,1,0)) %>% select(hh_id,im_in_project)

#
#________________
location_pipe <- 
  rmtl_srvy22  %>% select(hh_id,mm9) %>% 
  mutate(mm9= mm9+1,
         mm9=ifelse(mm9 ==-998,NA,mm9 ))%>% 
  left_join(rmtl_2024 %>% select( hh_id,farmers_B4_u ) )%>% 
  mutate(farmers_B4_u=ifelse(farmers_B4_u=="dont_know",NA,farmers_B4_u ),
    proximity_location = 
      if_else(is.na(mm9),suppressWarnings(as.numeric(farmers_B4_u)),mm9),
    proximity_near1_far0=ifelse(proximity_location < 4,1,0)
  ) %>% select(-mm9 ,-farmers_B4_u) 

#________________
rmtl_con_vars <- read_excel(
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_con_vars.xlsx")

#_________________#_________________  #__________________#
rmtl_con_vars <- 
  as_tibble(control_vars) %>% rename(in_project= in1_out0) %>% 
  left_join(dist_elev_var) %>% 
  left_join(geo_var) %>% 
  left_join(Qmm2)%>% 
  left_join(BL_total_assets)


# UPDATE 10/11/2025  [rmtl_cntrl_vars] #__________________#
rmtl_cntrl_vars <- 
  rmtl_con_vars %>% 
  rename(Elevation-elevation) %>% 
  mutate(
    cardinal_direction = case_when(
      south1_north0 == 1 ~ "south",
      south1_north0 == 0 ~ "north",
      TRUE ~ "center") ) %>% 
  left_join(location_pipe) %>% 
  left_join(rmtl_block_zone %>% dplyr:: select(hh_id, zone) )


library(writexl)
write_xlsx(rmtl_cntrl_vars, 
           "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_cntrl_vars.xlsx")




#________________________________ IRRIGATION   ___________________________ ----

# df to drip use [irrigation_BL_to_22]       ----        
irrigation_2022         # in part1_WaterUsage.R
irrigation_2017       # in part1_WaterUsage.R
irrigation_2018_2020   # in part1_WaterUsage.R
irrigation_BL        # in part1_WaterUsage.R

irrigation_6y <- 
  rmtl_InOut %>% 
  select (hh_id,mm4, drip_use, ir_use) %>% 
  rename(DI_installed=mm4,
         drip_use_6y = drip_use, 
         ir_use_6y = ir_use)

### df 1
library(readr)
irrigation_BL_to_22 <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/irrigation_BL_to_22.csv")
irrigation_BL_to_22 <- 
  full_join(irrigation_2022, irrigation_2018_2020) %>% 
  full_join(irrigation_2017) %>% 
  full_join(irrigation_BL)%>% 
  full_join(irrigation_6y) 

### df for reg ----
# irrigation_dist <- rmtl_con_vars %>% left_join(irrigation_BL_to_22)

impact_ir <- irrigation_BL_to_22 %>% 
  right_join(rmtl_con_vars) %>% as_tibble()

# Histogram                              ----
### df [dist]    500m bins | up to 10 Km
dist <- 
  irrigation_dist %>%
  mutate(dist_bin= cut(
    dist_to_south_km, breaks = seq(
      floor(min(dist_to_south_km, na.rm= T)/0.5)*0.5,
      ceiling(max(dist_to_south_km, na.rm= T)/0.5)*0.5, by=0.5
    ),include.lowest= T, right= F)) %>% 
  filter(dist_to_south_km >= (-10)) %>% 
  select(in_project,dist_bin,DI_installed,drip_use_6y ) %>% 
  group_by(in_project,dist_bin) %>% 
  summarise(N=n(),DI_installed=sum(DI_installed),drip_use=sum(drip_use_6y),
            .groups="drop") %>% 
  mutate(pct_drip=drip_use/N, pct_installed=DI_installed/N)

# Pivot to long format
library(tidyr)
library(scales)
dist_long <- dist %>%
  select(in_project, dist_bin, pct_installed, pct_drip) %>%
  pivot_longer(cols = c(pct_installed, pct_drip),
               names_to= "metric", values_to="pct") %>%
  mutate(fill_group = paste(metric, in_project, sep = "_"))

### df [dist_1.5] 50m bins | upto 1.5 Km
library(tidyr)
library(scales)

dist_1.5 <- 
  irrigation_dist %>% filter(dist_to_south_km<1.5, dist_to_south_km>(-1.5)) %>% 
  mutate(dist_bin= cut(
    dist_to_south_km, breaks = seq(
      floor(min(dist_to_south_km, na.rm= T)/0.05)*0.05,
      ceiling(max(dist_to_south_km, na.rm= T)/0.05)*0.05, by=0.05
    ),include.lowest= T, right= F)) %>%
  select(in_project,dist_bin,DI_installed,drip_use_6y ) %>% 
  group_by(in_project,dist_bin) %>% 
  summarise(N=n(),DI_installed=sum(DI_installed),drip_use=sum(drip_use_6y), 
            .groups = "drop") %>%
  mutate(pct_drip=drip_use/N, pct_installed=DI_installed/N)

dist_1.5_long <- dist_1.5 %>%
  select(in_project, dist_bin, pct_installed, pct_drip) %>%
  pivot_longer(cols = c(pct_installed, pct_drip),
               names_to= "metric", values_to="pct") %>%
  mutate(fill_group = paste(metric, in_project, sep = "_"))




# PLOT

# P1 = dist_1.5_long %>% 
# P2 = dist_long %>% 
  ggplot(aes(x = dist_bin, y = pct, fill = fill_group)) +
  geom_col(position = position_identity()) +
  scale_fill_manual( name   = NULL,
    breaks = c( "pct_installed_1","pct_installed_0", # order
                "pct_drip_1","pct_drip_0"),
    labels = c("DI Installed | In","DI Installed | Out",
               "DI Use | In","DI Use | Out"),
    values = c("pct_installed_0"= "burlywood1","pct_installed_1"= "slategray1",
               "pct_drip_0"= "orange3","pct_drip_1"= "dodgerblue3") ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Distance to Southen Boundary",y="Percent of Farms") 
  

#custom theme to format 
theme=theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.x =element_blank(),
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 10))

P1 + theme + labs(title = "Distance Distribution | Up to 1.5 Km in 50m bins") 
P2 + theme + labs(title = "Distance Distribution | Up to 10 Km in 500m bins") 
 








# function [func_drip_dist] [func_drip_elv]  ----

func_drip_elv <- function(data, outcome) {
  covariates <- c(
    "in_project","elevation","drip_use_BL",
    "hh_haed_age",
    "hh_haed_gendar",
    "hh_haed_edu_level",
    "total_acre16",
    "housing_str321",
    "job_income_sourceS",
    "govPnsin_scheme",
    "rent_property",
    "total_livestock",
    "total_farm_equipments") 
  form <- reformulate(covariates, response = outcome)
  lm(form, data = data)
}
func_drip_dist <- function(data, outcome) {
  covariates <- c(
    "in_project","dist_Km_boundary","drip_use_BL",
    "hh_haed_age",
    "hh_haed_gendar",
    "hh_haed_edu_level",
    "total_acre16",
    "housing_str321",
    "job_income_sourceS",
    "govPnsin_scheme",
    "rent_property",
    "total_livestock",
    "total_farm_equipments") 
  form <- reformulate(covariates, response = outcome)
  lm(form, data = data)
}
func_drip <- function(data, outcome) {
  covariates <- c("in_project") 
  form <- reformulate(covariates, response = outcome)
  lm(form, data = data)
}

# reg for drip       ----
#| "MODEL 0" in_project only
#| "MODEL 1" elevation        TO impact_ir # former elevation_0m
#| "MODEL 2" dist_Km_boundary TO impact_ir # dist_to_south_km
#| "MODEL 3" dist_Km_boundary TO impact_ir_south


# df <- irrigation_dist %>% filter(dist_to_south_m <1500)
# df <- irrigation_dist %>% filter(south1_north0 == 1)

impact_ir_south <- impact_ir %>% filter(dist_to_boundary_m <1500,south1_north0 == 1)
impact_ir_south <- impact_ir %>% filter(south1_north0 == 1)


mod_10 <-  # acre_drip acre_ir acre_cult
  lm(drip_use_6y  ~ in_project + dist_Km_boundary + drip_use_BL + 
       hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +total_livestock + total_farm_equipments,
     data = impact_ir)
summary(mod_10)
sjPlot::tab_model(mod_10 ,  show.se = T,digits = 5, show.stat  = F )

m1_iam <- impact_ir %>% func_drip_elv("im_in_project")
m1_ins <- impact_ir %>% func_drip_elv("DI_installed")
m1_17 <- impact_ir %>% func_drip_elv("drip_use_2017")
m1_18 <- impact_ir %>% func_drip_elv("drip_use_2018")
m1_19 <- impact_ir %>% func_drip_elv("drip_use_2019")
m1_20 <- impact_ir %>% func_drip_elv("drip_use_2020")
m1_22 <- impact_ir %>% func_drip_elv("drip_use_2022")
m1_6y <- impact_ir %>% func_drip_elv("drip_use_6y")

m2_iam <- impact_ir %>% func_drip_dist("im_in_project")
m2_ins <- impact_ir %>%  func_drip_dist("DI_installed")
m2_17 <- impact_ir %>%  func_drip_dist("drip_use_2017")
m2_18 <- impact_ir %>%  func_drip_dist("drip_use_2018")
m2_19 <- impact_ir %>%  func_drip_dist("drip_use_2019")
m2_20 <- impact_ir %>%  func_drip_dist("drip_use_2020")
m2_22 <- impact_ir %>%  func_drip_dist("drip_use_2022")
m2_6y <- impact_ir %>%  func_drip_dist("drip_use_6y")

m3_iam <- impact_ir_south %>% func_drip_dist("im_in_project")
m3_ins <- impact_ir_south %>%  func_drip_dist("DI_installed")
m3_17 <- impact_ir_south %>%  func_drip_dist("drip_use_2017")
m3_18 <- impact_ir_south %>%  func_drip_dist("drip_use_2018")
m3_19 <- impact_ir_south %>%  func_drip_dist("drip_use_2019")
m3_20 <- impact_ir_south %>%  func_drip_dist("drip_use_2020")
m3_22 <- impact_ir_south %>%  func_drip_dist("drip_use_2022")
m3_6y <- impact_ir_south %>%  func_drip_dist("drip_use_6y")


m0_iam <- impact_ir %>% func_drip("im_in_project")
m0_ins <- impact_ir %>% func_drip("DI_installed")
m0_6y <- impact_ir %>% func_drip("drip_use_6y")
m0_17 <- impact_ir %>% func_drip("drip_use_2017")
m0_18 <- impact_ir %>% func_drip("drip_use_2018")
m0_19 <- impact_ir %>% func_drip("drip_use_2019")
m0_20 <- impact_ir %>% func_drip("drip_use_2020")
m0_22 <- impact_ir %>% func_drip("drip_use_2022")

# reg TABLE          ----

library(modelsummary)

# "MODEL 1" elevation     TO impact_ir # former elevation_0m

# modelsummary(
#   list("I'm in Project"= m1_iam, "DI installed"= m1_ins,
#        "DI usage 2021-22"= m1_22, "DI usage 2020"= m1_20,
#        "DI usage 2019"= m1_19, "DI usage 2018"= m1_18,
#        "DI usage 2017"= m1_17, "DI usage 6 years"= m1_6y),
#   coef_map = c( in_project = "In Project",elevation = "Elevation",
#                 drip_use_BL= "Baseline Value",`(Intercept)`="Constant"), 
#   estimate  = "{estimate}",statistic = c("{std.error}", "{p.value}"),
#   title= "Drip‐Use Regression Results | Elevation")

usage_tbl_1 <- 
  modelsummary(
    list("I'm in Project"= m1_iam, "DI installed"= m1_ins, "DI usage 6 years"= m1_6y,
         "DI usage 2021-22"= m1_22,"DI usage 2020"= m1_20,"DI usage 2019"= m1_19, 
         "DI usage 2018"= m1_18,"DI usage 2017"= m1_17),
    coef_map = c( in_project = "In Project"), 
    estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
    gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",
    output = "data.frame")
usage_tbl_1$term[usage_tbl_1$term=="In Project"] <- "In Project \n[elev]"

usage_tbl_2 <- 
  modelsummary(
    list("I'm in Project"= m2_iam, "DI installed"= m2_ins, "DI usage 6 years"= m2_6y,
         "DI usage 2021-22"= m2_22,"DI usage 2020"= m2_20,"DI usage 2019"= m2_19, 
         "DI usage 2018"= m2_18,"DI usage 2017"= m2_17),
    coef_map = c( in_project = "In Project"), 
    estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
    gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",
    output = "data.frame")
usage_tbl_2$term[usage_tbl_2$term=="In Project"] <- "In Project \n[dist]"

usage_tbl_3 <- 
  modelsummary(
    list("I'm in Project"= m3_iam, "DI installed"= m3_ins, "DI usage 6 years"= m3_6y,
         "DI usage 2021-22"= m3_22,"DI usage 2020"= m3_20,"DI usage 2019"= m3_19, 
         "DI usage 2018"= m3_18,"DI usage 2017"= m3_17),
    coef_map = c( in_project = "In Project"), 
    estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
    gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",
    output = "data.frame")
usage_tbl_3$term[usage_tbl_3$term=="In Project"] <- "In Project \n[dist south]"

usage_tbl_0 <- 
  modelsummary(
    list("I'm in Project"= m0_iam, "DI installed"= m0_ins, "DI usage 6 years"= m0_6y,
         "DI usage 2021-22"= m0_22,"DI usage 2020"= m0_20,"DI usage 2019"= m0_19, 
         "DI usage 2018"= m0_18,"DI usage 2017"= m0_17),
    coef_map = c(`(Intercept)`= "Constant", in_project = "In Project"), 
    estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
    gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",
    output = "data.frame")
usage_tbl_0$term[usage_tbl_0$term=="In Project"] <- "In Project \n[model 0]"

## CONTROL MEAN
control_mean <- 
  impact_ir %>%
  filter(in_project==0, !is.na(elevation) ) %>% 
  select(im_in_project, DI_installed, drip_use_6y,starts_with("drip_use_20")) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3))) %>% 
  mutate(term="Control mean") %>% select(term,everything())
control_mean[] <- lapply(control_mean, as.character)
names(control_mean) <- c('term', "I'm in Project", "DI installed", "DI usage 6 years",
                         "DI usage 2021-22", "DI usage 2020",
                         "DI usage 2019", "DI usage 2018","DI usage 2017")


# REG TABLE FOR PAPER
usage_tbl_1 %>% # usage_tbl_2 %>%  # usage_tbl_3 %>% 
  select(-part, -statistic) %>% 
  bind_rows(control_mean
  ) %>% 
  kable("html", caption = "",align = "c") %>%
  kable_classic( full_width = F) %>%
  add_header_above(c(" ", "(1)" = 1, "(2)" = 1, "(3)" = 1, "(4)" = 1,
                     "(5)" = 1, "(6)" = 1, "(7)" = 1, "(8)" = 1))%>%   
  collapse_rows(columns = 1, valign = "top")


usage_tbl_0 %>%
  select(-part, -statistic) %>% 
  kable("html", caption = "",align = "c") %>%
  kable_classic( full_width = F) %>%
  add_header_above(c(" ", "(1)" = 1, "(2)" = 1, "(3)" = 1, "(4)" = 1,
                     "(5)" = 1, "(6)" = 1, "(7)" = 1, "(8)" = 1))%>%   
  collapse_rows(columns = 1, valign = "top")





#|=============================================================================


# PLOT reg           ----
library(ggplot2)
library(jtools)
library(ggstance)
library(RColorBrewer)

display.brewer.pal(n = 11, name = 'Dark2')
my_colors <- brewer.pal(8, "Dark2") # Paired, Set1

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


m1_plot <- 
  plot_summs(m1_iam,m1_ins,m1_6y,m1_22, m1_20, m1_19,m1_18, m1_17,
             coefs = c("In Project" = "in_project"),
             model.names = c("Im in Project","DI installed","DI usage 6 years",
                             "DI usage 2021-22", "DI usage 2020",
                             "DI usage 2019","DI usage 2018","DI usage 2017"),
             inner_ci_level = NULL,
             point.shape = FALSE,
             colors = my_colors # cbbPalette
  ) + labs(
    # subtitle = "Elevation as an Exogenous Variable [N=1,612]",
    # caption = "Note: Estimates reflect standardized coefficients with 95% CIs.",
    x = NULL, y = NULL)

m2_plot <- 
  plot_summs(
    m2_iam, m2_ins, m2_6y, m2_22, m2_20, m2_19, m2_18, m2_17, 
    coefs = c("In Project" = "in_project"),
    model.names = c("Im in Project","DI installed","DI usage 6 years",
                    "DI usage 2021-22", "DI usage 2020",
                    "DI usage 2019","DI usage 2018","DI usage 2017"),
    inner_ci_level = NULL,
    point.shape = FALSE,
    colors = my_colors) +
  labs(x = NULL,y = NULL)


m3_plot <- 
  plot_summs(m3_iam,m3_ins,  m3_6y, m3_22, m3_20, m3_19, m3_18, m3_17,
             coefs = c("In Project" = "in_project"),
             model.names = c("Im in Project","DI installed","DI usage 6 years",
                             "DI usage 2021-22", "DI usage 2020",
                             "DI usage 2019","DI usage 2018","DI usage 2017"),
             inner_ci_level = NULL,
             point.shape = FALSE,
             colors = my_colors
  ) + labs(x = NULL, y = NULL)




#custom theme to format 
mp_theme=theme_bw()+
  theme(
    panel.grid.major.y =element_blank(),
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))


# PLOT THE REG COFF
m1_plot + mp_theme + ggtitle("`Elevation` as an Exogenous Variable [Entire sample]")+ xlim(-0.06,0.45)
m2_plot + mp_theme + ggtitle("`Distance to Boundary` as an Exogenous Variable [Entire sample]")+ xlim(-0.06,0.45)
m3_plot + mp_theme+  ggtitle("`Distance to Boundary` as an Exogenous Variable [Southern sample]")+ xlim(-0.06,0.45)
# + xlim(-0.1,0.55)

#|=============================================================================


# PLOT dist_rd       ----

dist_rd <- 
  irrigation_dist %>%
  mutate(dist_to_south_km = ifelse(in_project==0,dist_to_south_km*-1,dist_to_south_km)) %>% 
  mutate(dist_1.5Km = ifelse(dist_to_south_km <= 1.5, dist_to_south_km,NA ) 
         ) %>% 
  mutate(dist_1.5Km = ifelse(dist_1.5Km >= (-1.5), dist_1.5Km,NA ) 
  ) %>% 
  mutate(dist_1.5Km_sq= dist_1.5Km^2,
         dist_1.5Km_sq= ifelse(in_project==0,dist_1.5Km_sq*(-1),dist_1.5Km_sq ))




# rd_drip_dist <- rdrobust(y = dist_rd$drip_use_2022, x = dist_rd$dist_to_south_km)
# summary(rd_drip_dist)


## custom theme to RD plot format
themeRD=theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12),
    legend.position='none'
    )

custom_layers <- list(
  scale_color_manual(
    values = c("0" = "orange3", "1" = "dodgerblue3"),
    labels= c("Out project","In project")),
  labs(x= "Km Distance to Boundary",y= "Percent of Farm"),
  themeRD) # add custom theme 

impact_ir_south
impact_ir
dist_to_south_km
elevation_0m

##########  Im in Project  elevation_0m
ggplot(impact_ir,
       aes(x = elevation_0m, y= im_in_project, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + labs(x= "Elevation (m)")+
  ggtitle("RD | Iam in Project")
##########  DI Installed  elevation_0m
ggplot(impact_ir,aes(x = elevation_0m, y= DI_installed, col= factor(in_project))) +geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + labs(x= "Elevation (m)")+ ggtitle("RD | DI Installed")
##########  drip_use_2022  elevation_0m
ggplot(impact_ir,aes(x = elevation_0m, y= drip_use_2022, col= factor(in_project))) +geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + labs(x= "Elevation (m)")+ ggtitle("RD | drip_use_2022") +coord_cartesian(ylim = c(-0.05, 0.25))
########## drip_use_6y  elevation_0m
ggplot(impact_ir,aes(x = elevation_0m, y= drip_use_6y, col= factor(in_project))) +geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + labs(x= "Elevation (m)")+ ggtitle("RD | drip_use_6y") +coord_cartesian(ylim = c(-0.05, 0.5))



##########  Im in Project  dist_to_south_km
ggplot(impact_ir_south,aes(x = dist_to_south_km,  y= im_in_project, col= factor(in_project))) +geom_point(color='white') +geom_smooth(method = "loess")+ 
  custom_layers + ggtitle("RD im_in_project")
########## DI_installed  dist_to_south_km
ggplot(impact_ir_south,aes(x = dist_to_south_km,  y= DI_installed, col= factor(in_project))) + geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + ggtitle("RD | DI installed")
########## drip_use_2022  dist_to_south_km
ggplot(impact_ir_south,aes(x = dist_to_south_km,  y= drip_use_2022, col= factor(in_project))) + geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers + ggtitle("RD | DI usage 2021-22") + coord_cartesian(ylim = c(-0.05, 0.25)) # Plot window to go from 0 to 0.5 without dropping any LOESS points
########## drip_use_6y  dist_to_south_km
ggplot(impact_ir_south,aes(x = dist_to_south_km,  y= drip_use_6y, col= factor(in_project))) + geom_point(color='white') +geom_smooth(method = "loess")+ 
  custom_layers + ggtitle("RD | DI usage 6 years") +coord_cartesian(ylim = c(0, 0.5))




rdplot(dist_rd$drip_use_6y, dist_rd$dist_1.5Km,
       title="Drip Installation | Distance to Boundary",
       x.label=NULL,y.label="Users", y.lim = c(0, 0.5))




# ___________________[[[[[[[[[  Agricultural level ]]]]]]]] _________   ###-----


reg_table_crop_acre
plot_df_crop_acre

reg_table_crop_freq
plot_df_crop_freq

reg_table_drip_land
plot_df_drip_land

reg_table_agri_vars
plot_df_agri_vars



plot_df <- 
plot_df_crop_acre %>% rename(agri_var=crop_type) %>% 
  mutate(
    agri_var=ifelse(agri_var == "Traditional","CT_acre","CC_acre")) %>% 
  rbind(
    plot_df_crop_freq %>%  rename(agri_var=crop_type) %>%
      mutate(
        agri_var=ifelse(agri_var == "Traditional","CT_hh%","CC_hh%"))
  ) %>% 
  rbind(plot_df_agri_vars )%>%
  mutate(
    agri_var=ifelse(agri_var == "ton_per_acre","A.ton_per_acre",agri_var)) %>%
  mutate(color_group = case_when(
    agri_var == "A.ton_per_acre" ~ "#bf9000",
    agri_var %in% c("CC_acre", "CC_hh%") ~ "#38761d",
    agri_var %in% c("CT_acre", "CT_hh%") ~ "#93c47d",
    agri_var %in% c("lost_pct", "improved_seeds") ~ "#674ea7",
    TRUE ~ "white")
    )



  
  
  
  
  
plot_df %>%  
  filter(!agri_var %in% c("CT_acre","CC_acre","A.ton_per_acre" )) %>% 
  ggplot(aes(x = agri_var, y = estimate, color = color_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 3) +
  geom_point(size = 6) +
  scale_color_identity() +
  labs(title = 'Impact of "in_project" (Balanced Sample)', 
       y = "Change in Coefficient Estimate", x = "") +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 20, color = "gray25"),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14)
  ) + ylim(-.15,.2)






# _____________________________  CULTIVATED LAND  ______________________   ###-----
#### DS {season} {acre cult} --- --- ----

# CULTIVETED LAND A YEAR new df 23.12.2025

crop_acre_22 <-  # in DF.22.R
  plots_crop_2022 %>% 
  filter(season != "kharif_2022") %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")
         ) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop=acres/n
         ) %>% 
  group_by(hh_id,crop_common) %>% 
  summarise(acre_crop_22 = sum(acre_crop),.groups="drop") 



crop_acre_BL <- 
  BL_2015_16_crop_IRsource_IRmethod %>% 
  mutate(crop_common = ifelse(crop_name %in% c("Chillies", "Onion"),crop_name, crop_common)) %>% 
  filter(season != "rabi_2014_15") %>% 
  mutate(season=ifelse(season=="rabi_2015_16","Rabi","Kharif")
  ) %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop = plot_acre/n 
         ) %>% 
  group_by(hh_id,crop_common) %>% 
  summarise(acre_crop_bl = sum(acre_crop),.groups="drop") 



crop_acre <- 
  crop_acre_22 %>% left_join(crop_acre_BL) %>% 
  mutate(crop_type = case_when(
    crop_common %in% c("Chillies","Onion","Sunflower","Oilseeds", "Sugarcane","Horticulture","Pearl.millet_bajra","Vegetables") ~ "HighValue",
    TRUE ~ "Traditional")
         )%>% 
  left_join(rmtl_cntrl_vars)


library(writexl)
write_xlsx(crop_acre, "C:/Users/Dan/Downloads/crop_acre.xlsx")


# Balanced Panel Logic in R
library(tidyverse)

# 1. Get unique HHs and their static characteristics
df <- crop_acre

df_2 <- crop_acre %>% 
  group_by(hh_id,crop_type) %>% 
  summarise(acre_crop_22= sum(acre_crop_22,na.rm = T),
            acre_crop_bl= sum(acre_crop_bl,na.rm = T),.groups="drop" 
  ) %>% left_join(rmtl_cntrl_vars)

hh_info <- df %>%
  select(hh_id, in_project, dist_Km_boundary, all_of(controls)) %>%
  distinct(hh_id, .keep_all = TRUE)

# 2. Create the "Skeleton" (Every HH x Every Crop)
target_crops <- c('Toor', 'Sorghum_jowar', 'Bengal_gram', 'Greengram', 'Onions', 'Sunflower', 'Sugarcane', 'Chillies', 'Oilseeds')
target_crops_2 <- c('Traditional', 'HighValue')

skeleton <- expand_grid(hh_id = unique(hh_info$hh_id),crop_common = target_crops)
skeleton_2 <- expand_grid(hh_id = unique(hh_info$hh_id),crop_type  = target_crops_2)

# 3. Merge original data and fill missing with 0
balanced_df <- skeleton %>%
  left_join(df %>% select(hh_id, crop_common, acre_crop_22, acre_crop_bl), by = c("hh_id", "crop_common")) %>%
  left_join(hh_info, by = "hh_id") %>%
  mutate(acre_crop_22 = replace_na(acre_crop_22, 0),acre_crop_bl = replace_na(acre_crop_bl, 0)
  )
balanced_df_2 <- skeleton_2 %>%
  left_join(df_2 %>% select(hh_id, crop_type, acre_crop_22, acre_crop_bl), by = c("hh_id", "crop_type")) %>%
  left_join(hh_info, by = "hh_id") %>%
  mutate(acre_crop_22 = replace_na(acre_crop_22, 0),acre_crop_bl = replace_na(acre_crop_bl, 0)
  )

# Now you can run the same map/nest regression logic on 'balanced_df'

library(tidyverse)
library(broom)

# 4. Define Renaming and Order
rename_map <- c("Sorghum_jowar" = "Jowar","Bengal_gram"= "Chickpea","Greengram"= "Moong")
ordered_labels <- c("Toor", "Jowar", "Chickpea", "Moong", "Onions", "Sunflower", "Sugarcane", "Chillies", "Oilseeds")

# Prepare for visualization/tables
balanced_df <- balanced_df %>%
  mutate(
    crop_display = ifelse(crop_common %in% names(rename_map), rename_map[crop_common], crop_common),
    crop_display = factor(crop_display, levels = ordered_labels)
  )

# --- STEP 1: DESCRIPTIVE TABLE ---

descriptive_table <- balanced_df %>%
  group_by(crop_display, in_project) %>%
  summarise(
    mean_acre = mean(acre_crop_22, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = in_project, 
    values_from = c(mean_acre, n),
    names_glue = "{.value}_{in_project}"
  ) %>%
  rename(
    `Mean (Out)` = mean_acre_0,
    `Mean (In)` = mean_acre_1,
    `N (Out)` = n_0,
    `N (In)` = n_1
  ) %>%
  arrange(crop_display)



# --- STEP 2: DESCRIPTIVE BAR PLOT ---

plot_stats <- balanced_df %>%
  group_by(crop_display, in_project) %>%
  summarise(
    mean_val = mean(acre_crop_22, na.rm = TRUE),
    sd_val   = sd(acre_crop_22, na.rm = TRUE),
    n_val    = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd_val / sqrt(n_val),
    ci_upper = mean_val + (1.96 * se),
    ci_lower = mean_val - (1.96 * se),
    Project_Status = ifelse(in_project == 1, "In Project", "Out of Project")
  )

ggplot(plot_stats, aes(x = crop_display, y = mean_val, fill = Project_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.8), width = 0.2, color = "black") +
  scale_fill_manual(values = c("In Project" = "steelblue4", "Out of Project" = "#BDBDBD")) +
  labs(title = "Mean Crop Acreage (Balanced Sample)", y = "Mean Acreage (2022)", x = "") +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

# --- STEP 3: REGRESSIONS & SUMMARY TABLE ---


fits_2 <- balanced_df_2 %>%
  group_by(crop_type) %>%
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "acre_crop_bl")
      fml <- as.formula(paste("acre_crop_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance) )

fits <- balanced_df %>%
  group_by(crop_display) %>%
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "acre_crop_bl")
      fml <- as.formula(paste("acre_crop_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance)
  )



# Constructing the Polished Reg Table logic
reg_table_crop_acre <- fits_2 %>%
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(crop_type, estimate, std.error, p.value) %>%
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate = paste0(sprintf("%.3f", estimate), stars),
    std.error = paste0("(", sprintf("%.3f", std.error), ")"),
    p.value = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join(fits_2 %>% unnest(stats) %>% select(crop_type, nobs, r.squared)) %>%
  left_join( # Join control means
    balanced_df_2 %>% 
      filter(in_project == 0) %>% 
      group_by(crop_type) %>% 
      summarise(c_mean = mean(acre_crop_22))
    )
plot_df_crop_acre <- fits_2 %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project") %>%
  mutate(
    color_group = ifelse(crop_type == "Traditional", "saddlebrown", "darkgreen")
  )


reg_table <- fits %>%
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(crop_display, estimate, std.error, p.value) %>%
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate_fmt = paste0(sprintf("%.3f", estimate), stars),
    se_fmt = paste0("(", sprintf("%.3f", std.error), ")"),
    pv_fmt = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join( # Join control means
    fits %>% unnest(stats) %>% select(crop_display, nobs, r.squared), by = "crop_display") %>%
  left_join(
    balanced_df %>% filter(in_project == 0) %>% group_by(crop_display) %>% 
      summarise(c_mean = mean(acre_crop_22)),
    by = "crop_display"
  )






# --- STEP 4: COEFFICIENT PLOT ---

plot_df <- fits %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project") %>%
  mutate(
    color_group = ifelse(crop_display %in% c("Toor", "Jowar", "Chickpea", "Moong"), "saddlebrown", "darkgreen")
  )

ggplot(plot_df, aes(x = crop_display, y = estimate, color = color_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = 1) +
  geom_point(size = 3) +
  scale_color_identity() +
  labs(title = 'Impact of "in_project" on Crop Acreage (Balanced Sample)', 
       y = "Acreage Change (Coefficient Estimate)", x = "") +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  ) 


# ________________ CROP HH FREQUANCY new df 30.12.2025  ------

skeleton_2
hh_info

crop22_N <-  crop_acre_22 %>% 
  mutate(crop_type = case_when(
    crop_common %in% c("Chillies","Onion","Sunflower","Oilseeds", "Sugarcane","Horticulture","Pearl.millet_bajra","Vegetables") ~ "HighValue",
    TRUE ~ "Traditional")
  ) %>% select(hh_id, crop_type) %>% distinct() %>% mutate(crop_freq_22=1)

cropBL_N <-  crop_acre_BL %>% 
  mutate(crop_type = case_when(
    crop_common %in% c("Chillies","Onion","Sunflower","Oilseeds", "Sugarcane","Horticulture","Pearl.millet_bajra","Vegetables") ~ "HighValue",
    TRUE ~ "Traditional")
  ) %>% select(hh_id, crop_type) %>% distinct() %>% mutate(crop_freq_BL=1)


crop_freq <- skeleton_2 %>% left_join(crop22_N) %>% left_join(cropBL_N) %>% 
  mutate(crop_freq_22 = replace_na(crop_freq_22, 0),
         crop_freq_BL = replace_na(crop_freq_BL, 0)
  ) %>%  left_join(hh_info)

fits_crop_freq <- crop_freq %>%  
  group_by(crop_type) %>%
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "crop_freq_BL")
      fml <- as.formula(paste("crop_freq_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance) )

reg_table_crop_freq <- fits_crop_freq %>%
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(crop_type, estimate, std.error, p.value) %>%
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate = paste0(sprintf("%.3f", estimate), stars),
    std.error = paste0("(", sprintf("%.3f", std.error), ")"),
    p.value = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join(fits_crop_freq %>% unnest(stats) %>% select(crop_type, nobs, r.squared)) %>%
  left_join( # Join control means
    crop_freq %>% 
      filter(in_project == 0) %>% 
      group_by(crop_type) %>% 
      summarise(c_mean = mean(crop_freq_22))
  )

plot_df_crop_freq <- fits_crop_freq %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project") %>%
  mutate(
    color_group = ifelse(crop_type == "Traditional", "saddlebrown", "darkgreen")
  )









#________________ IRRIGATED LAND new df 23.12.2025  ------

crop_acre_drip = plots_crop_2022 %>%
  filter(season != "kharif_2022") %>%
  group_by(hh_id, season, plotID) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop=acres/n) %>% 
  select(hh_id, season,plot_crop,crop_common, acre_crop ) %>% 
  left_join(a_irri_rain_method %>% 
              select(plot_crop,season,hh_id,irri_method) ) %>% 
  mutate(acre_drip_22=ifelse(irri_method=="drip",acre_crop ,0)) %>% 
  group_by(hh_id, crop_common) %>% summarise(acre_drip_22=sum(acre_drip_22))

crop_acre_drip_BL <- 
  BL_2015_16_crop_IRsource_IRmethod%>% 
  filter(season != "rabi_2014_15") %>% 
  group_by(hh_id, season, plot_num) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acre_crop = plot_acre/n ,
         drip_plot= ifelse(irri_method== "Drip",acre_crop,0)
  ) %>% group_by(hh_id) %>% 
  summarise(acre_crop_bl = sum(acre_crop),
            drip_acre = sum(drip_plot),.groups="drop") %>% 
  mutate(
    drip_pct =drip_acre/acre_crop_bl
  ) %>% select(-acre_crop_bl) %>% 
  pivot_longer(-hh_id,names_to = "drip_var",values_to = "drip_bl")

crop_acre_drip_22 <-
  crop_acre_22 %>% left_join(crop_acre_drip) %>% 
  group_by(hh_id) %>% 
  summarise(acre_crop_22 = sum(acre_crop_22  ),
            drip_acre = sum(acre_drip_22),.groups="drop") %>% 
  mutate(drip_pct =drip_acre / acre_crop_22
  ) %>% select(-acre_crop_22) %>% 
  pivot_longer(-hh_id,names_to = "drip_var",values_to = "drip_22")
  
  
drip_irrigated_acre <-
  crop_acre_drip_22 %>% left_join(crop_acre_drip_BL) %>% 
  left_join(hh_info)

fits_drip_land <- drip_irrigated_acre %>%  
  group_by(drip_var) %>%
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "drip_bl")
      fml <- as.formula(paste("drip_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance) )

reg_table_drip_land <- fits_drip_land %>%
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(drip_var, estimate, std.error, p.value) %>%
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate = paste0(sprintf("%.3f", estimate), stars),
    std.error = paste0("(", sprintf("%.3f", std.error), ")"),
    p.value = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join(fits_drip_land %>% unnest(stats) %>% select(drip_var, nobs, r.squared)) %>%
  left_join( # Join control means
    drip_irrigated_acre %>% 
      filter(in_project == 0) %>% 
      group_by(drip_var) %>% 
      summarise(c_mean = mean(drip_22))
  )

plot_df_drip_land <- fits_drip_land %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project") %>%
  mutate( color_group = "steelblue3")
















# 1. Load the data 
df <- crop_acre_22 %>% left_join(crop_acre_drip) %>% 
  left_join(rmtl_cntrl_vars)

library(writexl)
write_xlsx(irrigated_acre, "C:/Users/Dan/Downloads/irrigated_acre.xlsx")

# 2. Process Data to Household Level
# We aggregate by hh_id to get the total area per farmer
hh_df <- df %>%
  group_by(hh_id) %>%
  summarise(
    acre_crop_22 = sum(acre_crop_22, na.rm = TRUE),
    acre_drip_22 = sum(acre_drip_22, na.rm = TRUE),
    in_project   = first(in_project),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate percentage of area under drip
    pct_drip_22 = ifelse(acre_crop_22 > 0, (acre_drip_22 / acre_crop_22) * 100, 0),
    # Create labels for the groups
    Project_Status = factor(ifelse(in_project == 1, "In Project", "Out of Project"),
                            levels = c( "In Project","Out of Project"))
  )

# 3. Calculate Descriptive Stats and Confidence Intervals (95%)
plot_stats <- hh_df %>%
  pivot_longer(cols = c(acre_drip_22, pct_drip_22), 
               names_to = "Metric", values_to = "Value") %>%
  group_by(Project_Status, Metric) %>%
  summarise(
    mean_val = mean(Value, na.rm = TRUE),
    sd_val   = sd(Value, na.rm = TRUE),
    n_val    = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se = sd_val / sqrt(n_val),
    ci_upper = mean_val + (1.96 * se),
    ci_lower = mean_val - (1.96 * se),
    # Labels for the subplots
    Metric_Label = ifelse(Metric == "acre_drip_22", 
                          "Mean Area Under Drip Irrigation (Acres)", 
                          "Mean % Area Under Drip Irrigation")
  )

# 4. Generate the Descriptive Plots
# We'll use facet_wrap to create side-by-side plots like in the Python version
ggplot(plot_stats, aes(x = Project_Status, y = mean_val, fill = Project_Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  # Add text labels on top rounded to 2 digits
  geom_text(aes(label = sprintf("%.2f", mean_val)), 
            vjust = -2, size = 5, fontface = "bold", family = "serif") +
  facet_wrap(~Metric_Label, scales = "free_y") +
  # Colors: Steelblue4 for In Project, Gray for Out
  scale_fill_manual(values = c("In Project" = "steelblue4", "Out of Project" = "gray80")) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.grid = element_blank(),     # Delete all gridlines
    axis.title = element_blank(),     # Remove axis titles
    axis.text.y = element_blank(),    # Hide Y-axis labels as values are on top
    axis.text.x = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  # Increase top margin of the Y-axis to prevent label truncation
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


 
controls <- c(
  "dist_Km_boundary", "hh_haed_age", "hh_haed_gendar", "hh_haed_edu_level", 
  "total_acre16", "housing_str321", "job_income_sourceS", "govPnsin_scheme", 
  "rent_property", "livestock_dairy", "Bullock", "Tractor", "Plough", 
  "Thresher", "Seed_drill", "Motorcycle", "Fridge"
)











#_______________________________  YIELD df_agri_vars	________________________________  ----					


# yield_per_acre

yield_per_acre_BL <- 
  BL_plot.Crop.Yield %>% 
  filter(season != "rabi_2014_15") %>% 
  select(hh_id,season,plotID, yield) %>% filter (yield >0) %>% 
  left_join(BL_plotAcre) %>%  filter(plot_acre>0) %>% 
  group_by(hh_id,season,plotID) %>% 
  mutate(n=n(),
         plot_acre = plot_acre/n,
         yield =ifelse(yield<50,yield*100,yield)) %>% 
  group_by(hh_id) %>% 
  summarise(yield=sum(yield,na.rm = T),
            plot_acre=sum(plot_acre,na.rm = T)) %>% ungroup() %>% 
  mutate(yield_per_acre_bl=yield/plot_acre) %>% 
  mutate(yield_per_acre_bl= 
           ifelse(yield_per_acre_bl>3000,NA,
           ifelse(yield_per_acre_bl==0,NA, yield_per_acre_bl ))) %>% 
  select( hh_id, yield_per_acre_bl)

yield_per_acre_22 <- 
  plots_crop_2022 %>% 
  filter(season != "kharif_2022") %>% 
  select(hh_id,season,plotID,crop_number,acres) %>% 
  group_by(hh_id, season,plotID) %>% 
  mutate(n=n(),
         plot_acre=acres/n) %>% 
  left_join(
    a_total_yield %>% select(-farmers_hh ) ) %>%
  group_by(hh_id) %>% 
  summarise(kg_crop=sum(kg_crop,na.rm = T),
            plot_acre=sum(plot_acre,na.rm = T)) %>% ungroup() %>% 
  mutate(yield_per_acre_22=kg_crop/plot_acre)%>% 
  mutate(yield_per_acre_22= 
           ifelse(yield_per_acre_22>20000,NA,
           ifelse(yield_per_acre_22==0,NA, yield_per_acre_22 )))%>% 
  select( hh_id, yield_per_acre_22)




# lost_in_harvest_22
L54= rmtl_srvy22 %>% select(hh_id, starts_with("L54")) %>% 
  pivot_longer(cols = -hh_id,names_to = c("observation")) %>% 
  separate(observation, into = c("L" ,"pn","season", "plot", "crop"), sep = "_") %>% 
  filter(!is.na(value) ) %>%
  mutate(prt_lost=value/100 ) %>% 
  select(hh_id,season, plot, prt_lost)
L54$plotID <- sub("^(\\d{1,2})","plot_\\1",  L54$plot)

lost_in_harvest_22 <- 
  L54 %>% filter(season != "KHA22") %>% 
  group_by(hh_id) %>% 
  summarise(lost_pct=sum(prt_lost,na.rm = T)) %>% ungroup() 

# seeds_22
L58 <- rmtl_srvy22 %>% 
  select(hh_id, starts_with("l58")  ) %>% 
  pivot_longer(-hh_id, names_to = "season_crop", values_to = "improved_is2") 
seeds_22 <- L58 %>%
  mutate(improved_seeds= ifelse(improved_is2 ==2,1,0 )) %>% 
  group_by(hh_id) %>%  
  summarise(improved_seeds=sum(improved_seeds,na.rm = T )) %>% 
  mutate(improved_seeds= ifelse(improved_seeds>0,1,0))


# combine df
# val_bl
BL_vals <- yield_per_acre_BL %>% 
  mutate(ton_per_acre=yield_per_acre_bl/1000) %>% select(-yield_per_acre_bl) %>% 
  mutate(lost_pct=0,improved_seeds=0 ) %>% 
  pivot_longer(-hh_id, names_to = "agri_var",values_to = "val_bl")

# val_22
val_22 <- yield_per_acre_22 %>% 
  mutate(ton_per_acre=yield_per_acre_22/1000) %>% select(-yield_per_acre_22) %>% 
  left_join(lost_in_harvest_22) %>% 
  left_join(seeds_22) %>% 
  pivot_longer(-hh_id,names_to = "agri_var",values_to = "val_22" )

df_agri_vars <- 
  val_22 %>% left_join(BL_vals) %>% 
  left_join(hh_info) 

fits_agri_vars <- df_agri_vars %>%  
  group_by(agri_var) %>%
  nest() %>%
  mutate(
    model = map(data, ~ {
      f_vars <- c("in_project", "dist_Km_boundary", controls, "val_bl")
      fml <- as.formula(paste("val_22 ~", paste(f_vars, collapse = " + ")))
      lm(fml, data = .x)
    }),
    coefs = map(model, tidy),
    stats = map(model, glance) )

reg_table_agri_vars <- fits_agri_vars %>% 
  unnest(coefs) %>%
  filter(term == "in_project") %>%
  select(agri_var, estimate, std.error, p.value) %>%
  mutate(
    stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ ""),
    estimate = paste0(sprintf("%.3f", estimate), stars),
    std.error = paste0("(", sprintf("%.3f", std.error), ")"),
    p.value = paste0("[", sprintf("%.3f", p.value), "]")
  ) %>%
  left_join(fits_agri_vars %>% unnest(stats) %>% select(agri_var, nobs, r.squared)) %>%
  left_join( # Join control means
    df_agri_vars %>% 
      filter(in_project == 0) %>% 
      group_by(agri_var) %>% 
      summarise(c_mean = mean(val_22,na.rm = T))
  )

plot_df_agri_vars <- fits_agri_vars %>%
  mutate(results = map(model, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(results) %>%
  filter(term == "in_project") %>%
  mutate( color_group = "brown4")






#_______________________________  YIELD by crop 	________________________________  ----					

#  VegetablesANDFruits Sunflower Oilseeds Sugarcane 
#  Toor Bengal_gram Sorghum_jowar Greengram
df100 <- 
  BL_plot.Crop.Yield %>% 
  filter(crop_common %in% c(
    "VegetablesANDFruits", "Sunflower", "Oilseeds", "Sugarcane", 
    "Toor", "Bengal_gram", "Sorghum_jowar", "Greengram"    
  )) %>% 
  select(hh_id,season,plotID,crop_common, yield) %>% 
  filter (yield >0) %>% 
  left_join(BL_plotAcre) %>%  filter(plot_acre>0
                                     ) %>% 
  group_by(hh_id,season,plotID) %>% 
  mutate(n=n(),
         plot_acre = plot_acre/n,
         yield =ifelse(yield<50,yield*100,yield)
         ) %>% 
  group_by(hh_id,season,crop_common) %>% 
  summarise(yield=sum(yield),plot_acre=sum(plot_acre)) %>% ungroup() %>% 
  mutate(yield_per_acre=yield/plot_acre
         ) %>% 
  group_by(hh_id,crop_common) %>% 
  summarise(yield_per_acre=mean(yield_per_acre)) %>% ungroup() %>% 
  rename(crop=crop_common, kg_per_acre_BL =yield_per_acre)
##
##
##
plots_crop_2022 %>% count(common_n2_family)

df300 <- 
  plots_crop_2022 %>% 
  filter(common_n2_family %in% c(
    "VegetablesANDFruits", "Sunflower", "Oil seeds", "Sugarcane", 
    "Toor", "Bengal gram", "Sorghum/jowar", "Greengram"    
  )) %>% 
  select(hh_id,season,plotID,crop_number ,common_n2_family) %>% 
  left_join(
    a_total_yield %>% select(-farmers_hh ) ) %>% 
  left_join(total_acre_22) %>% 
  filter(kg_crop>0,acres>0)%>% select(-crop_number 
                                      ) %>% 
  group_by(hh_id, season, plotID) %>% 
  mutate(n=n(),
         plot_acre = acres/n ) %>% ungroup() %>% select(-n) %>% 
  
  group_by(hh_id,season,common_n2_family) %>% 
  summarise(kg_crop=sum(kg_crop),plot_acre=sum(plot_acre)) %>% ungroup() %>% 
  mutate(yield_per_acre=kg_crop/plot_acre
  ) %>% 
  group_by(hh_id,common_n2_family) %>% 
  summarise(yield_per_acre=mean(yield_per_acre)) %>% ungroup() %>% 
  rename(crop=common_n2_family, kg_per_acre =yield_per_acre) 
df300$crop[df300$crop=="Bengal gram"] <- "Bengal_gram"
df300$crop[df300$crop=="Sorghum/jowar"] <- "Sorghum_jowar"
df300$crop[df300$crop=="Oil seeds"] <- "Oilseeds"

# Analysis for season level ----
#-#-#-#  [kharif] = [rabi]      
df100 %>% count(crop) %>% as.data.frame()
df300 %>% count(crop) %>% as.data.frame()

#-# df300 %>% select(hh_id,plotID,season,crop) %>% group_by(hh_id,plotID,crop) %>% mutate(n=n())
#-#-#
#-#-#  אין חפיפה בין עונות - אף חלקה לא תיסכם פעמיים
#-#-#  גידולים שנתיים ירשמו כדלקמן 
#-#-# 
#-#-#  [kharif_2015] = [rabi_2015_16]
#-#-#  [kharif_2021] = [rabi_2021_22]

#       ### Sorghum_jowar RABI
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Sorghum_jowar") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Sorghum_jowar") %>% distinct() %>% count(season)
df100$season[df100$crop=="Sorghum_jowar" & df100$season=="kharif_2015"] <- "rabi_2015_16"
df300$season[df300$crop=="Sorghum_jowar" & df300$season=="kharif_2021"] <- "rabi_2021_22"

#       ### Bengal_gram RABI
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Bengal_gram") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Bengal_gram") %>% distinct() %>% count(season)
df100$season[df100$crop=="Bengal_gram" & df100$season=="kharif_2015"] <- "rabi_2015_16"
df300$season[df300$crop=="Bengal_gram" & df300$season=="kharif_2021"] <- "rabi_2021_22"

#       ### Toor KHARIF
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Toor") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Toor") %>% distinct() %>% count(season)
df100$season[df100$crop=="Toor" & df100$season=="rabi_2015_16"] <- "kharif_2015"
df300$season[df300$crop=="Toor" & df300$season=="rabi_2021_22"] <- "kharif_2021"

#v      ### Greengram KHARIF
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Greengram") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Greengram") %>% distinct() %>% count(season)
df100$season[df100$crop=="Greengram" & df100$season=="rabi_2015_16"] <- "kharif_2015"
df300$season[df300$crop=="Greengram" & df300$season=="rabi_2021_22"] <- "kharif_2021"

#       ### Maize KHARIF
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Maize") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Maize") %>% distinct() %>% count(season)
df100$season[df100$crop=="Maize" & df100$season=="rabi_2015_16"] <- "kharif_2015"
df300$season[df300$crop=="Maize" & df300$season=="rabi_2021_22"] <- "kharif_2021"

#       ### Sunflower KHARIF
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Sunflower") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Sunflower") %>% distinct() %>% count(season)
df100$season[df100$crop=="Sunflower" & df100$season=="rabi_2015_16"] <- "kharif_2015"
df300$season[df300$crop=="Sunflower" & df300$season=="rabi_2021_22"] <- "kharif_2021"

#       ### Wheat RABI
# df100 %>% select(hh_id,season,crop) %>% filter(crop=="Wheat") %>% distinct() %>% count(season)
# df300 %>% select(hh_id,season,crop) %>% filter(crop=="Wheat") %>% distinct() %>% count(season)
df100$season[df100$crop=="Wheat" & df100$season=="kharif_2015"] <- "rabi_2015_16"
df300$season[df300$crop=="Wheat" & df300$season=="kharif_2021"] <- "rabi_2021_22"





# Analysis for  kg per crop [df_kg_acre]  ----

df400 <- full_join(df300,df100 )
df400[is.na(df400)] <- 0 
df_kg_acre <- rmtl_con_vars %>% left_join(df400) %>% 
  filter(!is.na(kg_per_acre),!is.na (kg_per_acre_BL),!is.na (crop))


df_kg_acre$crop[df_kg_acre$crop == "Greengram"] <- "Mung"
df_kg_acre$crop[df_kg_acre$crop == "VegetablesANDFruits"] <- "Vegetables"
df_kg_acre$crop[df_kg_acre$crop == "Bengal_gram"] <- "Chickpea"
df_kg_acre$crop[df_kg_acre$crop == "Sorghum_jowar"] <- "Jowar"



# TEST
ml <- lm(kg_per_acre ~ in_project + dist_Km_boundary + kg_per_acre_BL +
      hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
      housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
      total_livestock + total_farm_equipments,
    data = df_kg_acre  %>% filter(crop=="Toor") )
# summary(ml)
sjPlot::tab_model(ml ,  show.se = T,digits = 5, show.stat  = F )


library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# Your formula (response is also named kg_per_acre)
fml <- kg_per_acre ~ in_project + dist_Km_boundary + kg_per_acre_BL +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 1) Keep only needed columns (+ crop) and drop rows with NA in model vars
dat_clean <- df_kg_acre %>%
  select(all_of(c("crop", vars))) %>%
  drop_na(all_of(vars))
  
# 2) Nest by crop and fit
fits <- dat_clean %>%
  group_by(crop) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance)
  )

# 3) Stacked outputs
coef_tbl  <- fits %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(crop, estimate, std.error, p.value) %>% 
  ungroup()
  
fit_stats <- fits %>% unnest(stats) %>%  select(crop,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit <- coef_tbl %>%
  left_join(fit_stats,by = "crop") %>% 
  ungroup()

# 5) Bild df for control group
control_mean <- 
  df_kg_acre %>% group_by(in_project,crop) %>% 
  summarise(`Control mean`=mean(kg_per_acre)) %>% ungroup() %>% 
  filter(in_project == 0) %>% 
  select(-in_project)

#     creat rows YES
twoRows <- 
  tribble( ~metric, ~Vegetables,  ~Sunflower, ~Oilseeds, ~Sugarcane	,~Toor, ~Chickpea, ~Jowar, ~Mung,
           "Control vars", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes",
           "Dist. boundary", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes")


# LM Outcomes Table ----
  
df_html <- inproj_with_fit %>%
  left_join(control_mean) %>% 
  pivot_longer(-crop, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = crop, values_from = value) %>% 
  select(metric, Vegetables, Sunflower, Oilseeds, Sugarcane,
         Toor, Chickpea, Jowar, Mung) %>%
  # Format values by row type; keep as characters for kable
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)


df_html[c(1:3,7:8,4:6),] %>% 
  kable("html", caption = "Yield | Kg per Acre",align = "c") %>%
  kable_classic( full_width = F) %>%
  add_header_above(c(" ", "(1)" = 1, "(2)" = 1, "(3)" = 1, "(4)" = 1,
                     "(5)" = 1, "(6)" = 1, "(7)" = 1, "(8)" = 1))%>%   
  add_header_above(c("Crop type", "High-value" = 4, "Traditional" = 4))%>%
  collapse_rows(columns = 1, valign = "top")


# PLOT reg          ----


library(jtools)
# library(rlang)  # for !!!
library(RColorBrewer)

my_colors <- brewer.pal(8,"Paired")
"Vegetables","#A6CEE3", 
"Sunflower",  "#1F78B4",  
"Oilseeds", "#B2DF8A",
"Sugarcane",   "#33A02C",
"Toor",  "#FB9A99",
"Chickpea",  "#E31A1C",
"Jowar",   "#FDBF6F", 
"Mung" , "#FF7F00"


my_5colors <- c("#1F78B4",   "#B2DF8A", "#FB9A99", "#E31A1C", "#FDBF6F")
my_2colors <- c("#A6CEE3",  "#FF7F00")
my_1colors <- c("#33A02C", "#33A02C") # second color is dummy


# Put models in a named list in the order you want to show
models_list <- fits %>% 
  filter(! crop %in% c("Sugarcane","Mung","Vegetables")) %>% 
  arrange(crop) %>%                # choose your order
  { setNames(.$model, .$crop) }    # names become model names in the legend

models_veg_mung <- fits %>% 
  filter(crop %in% c("Mung","Vegetables")) %>% 
  arrange(crop) %>%                # choose your order
  { setNames(.$model, .$crop) }    # names become model names in the legend

models_Sugarcane <- fits %>%
  filter(crop %in% c("Sugarcane","Oilseeds")) %>% # Oilseeds is dummy
  arrange(crop) %>%                # choose your order
  { setNames(.$model, .$crop) }    # names become model names in the legend


# library(jtools)
# library(ggstance)

m1_plot <- 
  plot_summs(models_list ,
             coefs = c("In Project" = "in_project"),
             model.names = names(models_list),
             inner_ci_level = NULL, point.shape = FALSE, colors = my_5colors) + 
  labs(x = "Kg per Acre", y = NULL) 

m2_plot <- 
  plot_summs(models_veg_mung ,
             coefs = c("In Project" = "in_project"),
             model.names = names(models_veg_mung),
             inner_ci_level = NULL, point.shape = FALSE, colors = my_2colors) + 
  labs(x = "Kg per Acre", y = NULL) 

m3_plot <- 
  plot_summs(models_Sugarcane ,
             coefs = c("In Project" = "in_project"),
             model.names = names(models_Sugarcane),
             inner_ci_level = NULL, point.shape = FALSE, colors = my_1colors) + 
  labs(x = "Kg per Acre", y = NULL) 

# PLOT THE REG COFF  #custom theme in IRRIGATIN in this  R script
m1_plot + mp_theme + ggtitle("`Distance from boundary` as an Exogenous Variable [Entire sample]")
m2_plot + mp_theme + ggtitle("`Distance from boundary` as an Exogenous Variable [Entire sample]")
m3_plot + mp_theme + ggtitle("`Distance from boundary` as an Exogenous Variable [Entire sample]")




# ____________________________  CROPPING PATTERN____________________________----

library(readr)
list_crop <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.csv")

# Cotton IS Oilseeds

#🟡 BASELINE
crop_2016_A= bl_crop_plot_3s %>% select(hh_id,season,"crop_name","crop_common") 
crop_2016_A$crop_common[crop_2016_A$crop_name=="ONION"] <- "Onions"
crop_2016_A$crop_common[crop_2016_A$crop_name=="Chillies"] <- "Chillies"
crop_2016_A$crop_common[crop_2016_A$crop_name=="GREENCHILLY"] <- "Chillies"
crop_2016_A$crop_common[crop_2016_A$crop_common =="VegetablesANDFruits"] <- "Vegetables"

crop_2016_A <- crop_2016_A %>% 
  filter(!is.na(crop_common),crop_common !="Other") %>% 
  select(hh_id,crop_common) %>% distinct()


croping_pattern_2016 <- crop_2016_A %>% 
  mutate(
    crop = ifelse(crop_common %in% c("Chillies","Onions") ,
                  "Vegetables", crop_common))  %>% 
  mutate(
    crop = case_when( # Recode crop names
      crop == "Bengal_gram"   ~ "Chickpea",
      crop == "Sorghum_jowar" ~ "Jowar",
      crop == "Greengram"     ~ "Mung",
      TRUE ~ crop
    ),
    crop_type = case_when( # Classify crop type
      crop %in% c("Vegetables","Sunflower","Oilseeds","Sugarcane") ~ "High-value",
      crop %in% c("Toor","Chickpea","Jowar","Mung")               ~ "Traditional",
      TRUE~ NA_character_) ) %>% 
  left_join(rmtl_16_18_22_sample) %>%
  mutate(Project= ifelse(sample == 1, "In","Out")) %>% 
  select(hh_id, Project, crop, crop_type)


## Cropping pattern at Baseline ----
crop_summary_bl <- croping_pattern_2016 %>% 
  filter(!is.na(crop_type) ) %>% 
  count(Project,crop) %>% 
  group_by(Project) %>% 
  mutate(pct=n/sum(n),pct=round(pct, 2)) 

my_crop_colors <- c(
  Vegetables   = "#A6CEE3",
  Sunflower    = "#1F78B4",
  Oilseeds     = "#B2DF8A",
  Sugarcane    = "#33A02C",
  Toor         = "#FB9A99",
  Chickpea     = "#E31A1C",
  Sorghum      = "#FDBF6F",
  Mung         = "#FF7F00"
)

library(ggplot2)
library(treemapify)

ggplot(crop_summary_bl,
       aes(area = pct, fill = crop, label = paste0(crop, "\n", pct*100, "%"))) +
  geom_treemap(color = "white") +
  geom_treemap_text(colour = "black", place = "centre", grow = TRUE, family = "serif") +
  scale_fill_manual(values = my_crop_colors, guide = "none") +  # guide="none" removes legend
  facet_wrap(~Project) +
  theme_minimal(base_size = 14, base_family = "serif") +
  labs(
    title = "Cropping pattern at Baseline"
  )







# High-value crop adoption

# dfs----
#🟡 BASELINE

crop_2016 = crop_2016_A %>% 
  filter(crop_common %in% c("Onions","Chillies","Sunflower","Oilseeds","Sugarcane") ) %>% 
  mutate(n16=1)


# 🟠 MIDELINE  t6ty6

crop_2018 <- ml18_crop_plot_3s %>% 
  select(hh_id,crop_ml18) %>% 
  rename(crop_code= crop_ml18) %>% 
  left_join(list_crop %>% select(crop_code,crop_name ,crop_common)  ) %>%
  select(hh_id,crop_common) %>% distinct() %>% 
  filter(crop_common %in% c("Onions","Chillies","Sunflower","Oilseeds","Sugarcane") ) %>% 
  mutate(n18=1)


#🟣 SURVEY 22

crop_2022 <- plots_crop_2022 %>% 
  select(hh_id,crop_code,crop_name, common_n_family) %>% 
  left_join(list_crop %>% select(crop_code ,crop_common)  ) %>% 
  select(hh_id,crop_common) %>% distinct() %>% 
  filter(crop_common %in% c("Onions","Chillies","Sunflower","Oilseeds","Sugarcane") ) %>% 
  mutate(n22=1)


  
  crop_2022_id <- plots_crop_2022  %>% select(hh_id) %>% distinct()
  
  high_value_crops <- c("Onions","Chillies", "Sunflower", "Oilseeds", "Sugarcane")
  
  crop_2022_expanded <- crop_2022_id %>%
    mutate(dummy = 1) %>%
    left_join( tibble(crop_common = high_value_crops, dummy = 1), by = "dummy") %>%
    select(-dummy)
  

cropping_16_18_22 <- crop_2022_expanded %>% 
  left_join(crop_2016) %>% 
  left_join(crop_2018) %>% 
  left_join(crop_2022) %>% 
  left_join(rmtl_16_18_22_sample %>% select(hh_id,sample)) %>% 
  mutate(sample = ifelse(sample==1,"In","Out")) 
  


library(tidyr)
library(scales)

transitions <- cropping_16_18_22 %>%
  mutate(
    n16 = replace_na(n16, 0),
    n18 = replace_na(n18, 0),
    n22 = replace_na(n22, 0),
    status_6Y= case_when(
      n16 == 0 & n22 == 0 ~ "Never cultivated",
      n16 == 0 & n22 == 1 ~ "Adopted",
      n16 == 1 & n22 == 0 ~ "Dropped",
      n16 == 1 & n22 == 1 ~ "Kept cultivating" 
      ),
      status_1Y= case_when(
      n16 == 0 & n18 == 0 ~ "Never cultivated",
      n16 == 0 & n18 == 1 ~ "Adopted",
      n16 == 1 & n18 == 0 ~ "Dropped",
      n16 == 1 & n18 == 1 ~ "Kept cultivating" ) )

# PLOT high_value_crops ----

### Order rows nicely
status_levels <-  # for bars
  c("Never cultivated", "Adopted","Dropped", "Kept cultivating")
# status_levels <- # for legend
#   c("Kept cultivating", "Adopted","Dropped","Never cultivated")


crop_transitions_6 <- transitions %>%
  group_by(sample, crop_common, status_6Y) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>% mutate(survey="6Y") %>% 
  mutate(
    status = factor(status_6Y, levels = status_levels),
    crop_common = factor(crop_common)  )

crop_transitions_1 <- transitions %>%
  group_by(sample, crop_common, status_1Y) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%  mutate(survey="1Y") %>% 
  mutate(
    status = factor(status_1Y, levels = status_levels),
    crop_common = factor(crop_common)  )

crop_transitions <-
  crop_transitions_1 %>% select(-status_1Y) %>% 
  rbind(
    crop_transitions_6 %>% select(-status_6Y)
  )

### arrange color to crop
status_cols <- c(
  "Never cultivated"  = "#C0C0C0",  # slightly lighter gray
  "Adopted"            = "#66BD66",  # medium-light green
  "Dropped"             = "#F26C6C",  # medium-light red
  "Kept cultivating"    = "#5BA3E0"   # medium-light blue
)


### plot
crop_transitions %>% filter(status != "Never cultivated"
                            ) %>% 
  ggplot(aes(x = crop_common, y = pct, fill = status)) +
  geom_col(color = "white") +
  scale_fill_manual(values = status_cols, name = NULL) +
  coord_flip() +
  facet_grid(survey ~ sample) +  # ⬅️ Rows = survey (1Y vs 6Y), columns = sample (In vs Out)
  labs(x = NULL, y = "Households (%)",
       title = "Crop transitions 1 and 6 years after DI installed") +
  theme_minimal(base_family = "serif") +
  theme(legend.text = element_text(size = 16))



 # REG ----

adopte_crop <- cropping_16_18_22 %>% 
  mutate(in_project=ifelse(sample == "In",1,0)) %>%
  select(-sample) %>% 
  rename(adopte_6Y=n22,adopte_1Y=n18, adopte_BL=n16,crop=crop_common)
adopte_crop[is.na(adopte_crop)] <- 0 
adopte_crop <- adopte_crop %>% left_join(rmtl_con_vars) 


# df400 <- full_join(df300,df100 ) %>% 
# df400[is.na(df400)] <- 0 
# df_kg_acre <- rmtl_con_vars %>% left_join(df400) %>% 
#   filter(!is.na(kg_per_acre),!is.na (kg_per_acre_BL),!is.na (crop))

  
  
# TEST
ml <- lm(adopte_6Y ~ in_project + dist_Km_boundary + adopte_BL +
           hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
           housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
           total_livestock + total_farm_equipments,
         data = adopte_crop  %>% filter(crop =="Sunflower") )
summary(ml)
sjPlot::tab_model(ml ,  show.se = T,digits = 5, show.stat  = F )

library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# model formula 
fml_adopte_6Y <- adopte_6Y ~ in_project + dist_Km_boundary + adopte_BL +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

fml_adopte_1Y <- adopte_1Y ~ in_project + dist_Km_boundary + adopte_BL +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments


# 2) Nest by crop and fit

fits_adopte_6Y <- adopte_crop %>%
  group_by(crop) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_adopte_6Y, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

fits_adopte_1Y <- adopte_crop %>%
  group_by(crop) %>%nest() %>%
  mutate(
    model = map(data, ~ lm(fml_adopte_1Y, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
coef_tbl_6  <- fits_adopte_6Y %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(crop, estimate, std.error, p.value) %>% ungroup()

coef_tbl_1  <- fits_adopte_1Y %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(crop, estimate, std.error, p.value) %>% ungroup()
##
fit_stats_6 <- fits_adopte_6Y %>% unnest(stats) %>%  select(crop,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

fit_stats_1 <- fits_adopte_1Y %>% unnest(stats) %>%  select(crop,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_6 <- coef_tbl_6 %>%
  left_join(fit_stats_6,by = "crop") %>% ungroup()

inproj_with_fit_1 <- coef_tbl_1 %>%
  left_join(fit_stats_1,by = "crop") %>% ungroup()

# 5) Bild df for control group
control_mean <- 
  adopte_crop %>% group_by(in_project,crop) %>% 
  summarise(
    `Control_mean_6`=mean(adopte_6Y),
    `Control_mean_1`=mean(adopte_1Y)
    ) %>% ungroup() %>% 
  filter(in_project == 0) %>% 
  select(-in_project)

#     creat rows YES
twoRows <- 
  tribble( ~metric,  ~Sunflower, ~Sugarcane, ~Onions, ~Oilseeds	,~Chillies,
           "Control vars", "Yes", "Yes", "Yes", "Yes","Yes",
           "Dist. boundary", "Yes", "Yes", "Yes", "Yes","Yes")

# ML Outcomes Table ----
df_html_6 <- inproj_with_fit_6 %>%
  left_join(control_mean %>% select(crop,`Control_mean_6`)) %>% 
  pivot_longer(-crop, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = crop, values_from = value) %>% 
  select(metric,Sunflower,Sugarcane,Onions, Oilseeds, Chillies)%>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)

df_html_1 <- inproj_with_fit_1 %>%
  left_join(control_mean %>% select(crop,`Control_mean_1`)) %>% 
  pivot_longer(-crop, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = crop, values_from = value) %>% 
  select(metric,Sunflower,Sugarcane,Onions, Oilseeds, Chillies) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)


library(kableExtra)
df_html_6[c(1:3,7:8,4:6),] %>% 
  kable("html", caption = "Adoption of high-value crops | HH (%)",align = "c") %>%
  kable_classic( full_width = F) %>%
  add_header_above(c("", "After 6 years"= 5))%>%
  collapse_rows(columns = 1, valign = "top")

df_html_1[c(1:3,7:8,4:6),] %>% 
  kable("html", caption = "Adoption of high-value crops | HH (%)",align = "c") %>%
  kable_classic( full_width = F) %>%
  add_header_above(c("", "After 1 year"= 5))%>%
  collapse_rows(columns = 1, valign = "top")












# ___________________________ YIELD Sold Kept Lost__________________________----					

# NO BASLINE "YIELD %" VALUES IN THIS REG


#🟠 D60
d60a <- rmtl_midline2018 %>% select(2,starts_with("d60a")) %>% 
  pivot_longer(-hh_id,names_to = "d60",values_to = "Sold") %>% 
  mutate(d60 = str_remove(d60, "^d60a_"))

  
d60b <- rmtl_midline2018 %>% select(2,starts_with("d60b"))%>% 
  pivot_longer(-hh_id,names_to = "d60",values_to = "Self_consume") %>% 
  mutate(d60 = str_remove(d60, "^d60b_"))

d60c <- rmtl_midline2018 %>% select(2,starts_with("d60c"))%>% 
  pivot_longer(-hh_id,names_to = "d60",values_to = "Lost_in_harvest") %>% 
  mutate(d60 = str_remove(d60, "^d60c_"))

d60d <- rmtl_midline2018 %>% select(2,starts_with("d60d"), -contains("os"))%>% 
  pivot_longer(-hh_id,names_to = "d60",values_to = "other") %>% 
  mutate(d60 = str_remove(d60, "^d60d_"))


D60 <- d60a %>% 
  inner_join(d60b) %>% inner_join(d60c) %>%inner_join(d60d) %>%
  mutate(
    total_amount = Sold + Self_consume + Lost_in_harvest + other,
    d60 = case_when(
      grepl("^s1_", d60) ~ "rabi_201718",
      grepl("^s2_", d60) ~ "kharif_2017",
      TRUE ~ d60
    )) %>%
  filter(total_amount > 0, d60 %in% c("rabi_201718", "kharif_2017")) %>%
  group_by(hh_id, d60) %>%
  summarise(across(Sold:total_amount, \(x) sum(x, na.rm = TRUE)),.groups = "drop") %>%
  mutate(
    Sold = (Sold / total_amount) * 100,
    Self_consume = (Self_consume / total_amount) * 100,
    Lost_in_harvest = (Lost_in_harvest / total_amount) * 100
  )

yield_prt_MID <- D60 %>% 
  pivot_longer(-c(hh_id,d60), names_to = "status",values_to = "yield_pct_MID") %>% 
  filter(! status %in% c("total_amount", "other") )



# How much of the yield was [%]	# [percentage at Season-Crop]
# [L52] Sold # [L53] Kept for HH consumption # [L54] Lost in post-harves

yield_prt_season <- 
  yield_prt %>%  # in DF_22.R
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(hh_id,season) %>% 
  summarise(Sold=mean(prt_sold ,na.rm=T)/100,
            Stored=mean(prt_stored,na.rm=T)/100,
            Self_consumption=mean(prt_consum,na.rm=T)/100,
            Lost_in_harvest=mean(prt_lost,na.rm=T)/100
            ) %>% 
  ungroup() %>%
  pivot_longer(-c(hh_id ,season),
               names_to = "status",
               values_to = "pct") %>% 
  left_join(rmtl_con_vars) 

# PLOT desc ----

plot_yield_prt <- yield_prt_season %>% 
  group_by(season,in_project,status) %>%
  summarise(pct=mean(pct,na.rm=T)) %>% ungroup() %>% 
  mutate(status = recode(status,
                         "Self_consumption" = "Self\nConsume",
                         "Lost_in_harvest"  = "Lost in\nHarvest"))
# 600X300
plot_yield_prt %>% filter(status != "Stored" )%>% 
  ggplot(aes(x = status, y = pct, fill = factor(in_project))) +
  geom_col(position = "dodge", color = "white") +
  geom_text(aes( label = if_else(
    status %in% c("Lost in\nHarvest", "Self\nConsume"),
    sprintf("%.1f%%", pct * 100),            # 2 decimals
    scales::percent(pct, accuracy = 1)  )),  # whole %
    position = position_dodge(width = 0.9),vjust = -0.3, size = 3) +
  facet_wrap(~ season) +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "steelblue"),
                    labels = c("0" = "Out of Project", "1" = "In Project")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "% Yield",fill = NULL,
       title = "Yield uses as share of the total seasonal yield 2022") +
  theme_minimal(base_family = "serif")








#  Reg ----
# NO BASLINE "YIELD %" VALUES IN THIS REG
# TEST Reg
ml <- lm( pct ~ in_project + dist_Km_boundary  +
           hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
           housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
           total_livestock + total_farm_equipments,
         data = yield_prt_season  %>% filter(season =="Kharif",status=="prt_sold") 
         )
summary(ml)
sjPlot::tab_model(ml ,  show.se = T,digits = 5, show.stat  = F )


library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# 1) model formula 
# NO BASLINE "YIELD %" VALUES IN THIS REG

fml_yield_prt <- pct ~ in_project + dist_Km_boundary  +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 2) Nest by status and fit
fits_yield_prt_kharif <- yield_prt_season %>%
  filter(season =="Kharif") %>% 
  group_by(status) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_yield_prt, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

fits_yield_prt_rabi <- 
  yield_prt_season %>% filter(season =="Rabi") %>% 
  group_by(status) %>%nest() %>%
  mutate( model = map(data, ~ lm(fml_yield_prt, data = .x)),
          coefs = map(model, tidy),stats = map(model, glance))

# 3) Stacked outputs
coef_tbl_kharif  <- fits_yield_prt_kharif %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(status, estimate, std.error, p.value) %>% ungroup()
##
fit_stats_kharif <- fits_yield_prt_kharif %>% unnest(stats) %>%  
  select(status,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# RABI
coef_tbl_rabi  <- fits_yield_prt_rabi %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(status, estimate, std.error, p.value) %>% ungroup()
##
fit_stats_rabi <- fits_yield_prt_rabi %>% unnest(stats) %>%  
  select(status,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_Kharif <- coef_tbl_kharif %>%
  left_join(fit_stats_kharif,by = "status") %>% ungroup()

inproj_with_fit_rabi <- 
  coef_tbl_rabi %>% left_join(fit_stats_rabi, by = "status") %>%
  ungroup()

# 5) Bild df for control group
control_mean <- 
  yield_prt_season %>% 
  group_by(season,in_project,status) %>%
  summarise(pct=mean(pct,na.rm=T)) %>% ungroup() %>% 
  filter(in_project == 0) %>% 
  select(-in_project) %>% rename(control_mean=pct)


# 6) Create YES rows
twoRows <- 
  tribble(~season, ~metric,  ~Sold, ~Stored, ~Self_consumption, ~Lost_in_harvest	,
          "","Control vars", "Yes", "Yes", "Yes", "Yes",
          "","Dist. boundary", "Yes", "Yes", "Yes","Yes")

# ML Outcomes Table ...................................................

# KHARIF ML Outcomes Table
df_html_kharif <- inproj_with_fit_Kharif %>%
  left_join(control_mean %>% filter(season=="Kharif") ) %>% 
  pivot_longer(-c(season,status), 
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = status, values_from = value
              ) %>% 
  mutate(across(-c(season,metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)

# RABI ML Outcomes Table 
df_html_rabi <- inproj_with_fit_rabi %>%
  left_join(control_mean %>% filter(season=="Rabi") ) %>%
  pivot_longer(-c(season,status), names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = status, values_from = value
  ) %>% 
  mutate(across(-c(season,metric), ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE~ as.character(round(.x,3))
  ))) %>% rbind(twoRows)


library(kableExtra)
df_html_kharif[c(1:3,7:8,4:6),] %>% 
  rbind(
    df_html_rabi[c(1:3,7:8,4:6),] 
  ) %>% 
  kable("html", caption = "Percentage of yield sold/stored/self-consum/lost in harvest",align = "c") %>%
  kable_classic( full_width = F) 


# PLOT ----
library(jtools)
models_list_k <- fits_yield_prt_kharif %>% 
  filter(status == "Lost_in_harvest") %>% 
  { setNames(.$model, .$status) }    # names become model names in the legend

models_list_r <- fits_yield_prt_rabi %>% 
  filter(status == "Lost_in_harvest") %>% 
  { setNames(.$model, .$status) }    # names become model names in the legend

m1_plot <- 
  plot_summs(models_list_k ,coefs = c("In Project\nKharif" = "in_project"),
             model.names = names(models_list_k),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "% of Lost in Harvest yield", y = NULL) +
  xlim(-0.02, 0.01) 

m2_plot <- 
  plot_summs(models_list_r ,coefs = c("In Project\nRabi" = "in_project"),
             model.names = names(models_list_r),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "% of Lost in Harvest yield", y = NULL) +
  xlim(-0.02, 0.01) 

m1_plot + mp_theme
m2_plot + mp_theme



#__________________________   SEEDS   ______________________________________----

#🟣L57 Name the seed [  ]
#🟣L58 Is it normal or improved seeds? [1=normal | 2=improved]

library(stringr)
library(haven)

L57=rmtl_srvy22 %>% 
  select(hh_id, farmers_hh, starts_with("l57")  )%>% 
  mutate_at(vars(-hh_id), as.character) %>% 
  pivot_longer(-c(hh_id, farmers_hh), names_to = "season_crop", values_to = "seed_name") %>% 
  separate(season_crop, into = c("L" ,"season_crop"), sep = "7_") %>% select(-L)

L58=rmtl_srvy22 %>% 
  select(hh_id, farmers_hh, starts_with("l58")  ) %>% 
  pivot_longer(-c(hh_id, farmers_hh), names_to = "season_crop", values_to = "improved_is2") %>% 
  separate(season_crop, into = c("L" ,"season_crop"), sep = "8_") %>% select(-L)

L578=L57 %>%  left_join(L58) %>% 
  filter(!is.na(improved_is2),seed_name != -999) %>% 
  mutate(seed= ifelse(improved_is2==2,"Improved","Traditional"))

L578 %>% count(seed_name) %>% arrange(desc(n))

### [improved_seeds_22]
improved_seeds_22 <- L578 %>%   
  mutate(season_crop = str_replace(season_crop, "KHA22_", "kha_")) %>% 
  distinct() %>% 
  count(hh_id,seed) %>% 
  pivot_wider(names_from = seed,values_from = n,values_fill = list(n = 0) ) %>% 
  mutate(total_seeds= Improved + Traditional,
         pct_per_hh= Improved/total_seeds,
         pct_per_group = ifelse(Improved==0,0,1)
         ) %>% 
  select(hh_id,pct_per_group, pct_per_hh  ) %>% 
  pivot_longer(-hh_id,
               names_to = "status",
               values_to = "pct") %>% 
  left_join(rmtl_con_vars) 
 
# DESC STAT  ----
improved_seeds_22 %>% group_by(status, in_project) %>% 
  summarise(
    pct =  mean(pct),
    n=n()) %>% 
  mutate(status = recode(status,
                         "pct_per_group" = "% of farms using \nimproved seeds",
                         "pct_per_hh"  = "% of improved seeds \nused by individual farmer")
         ) %>% 
  ggplot(aes(x = status, y = pct, fill = factor(in_project))) +
  geom_col(position = "dodge", color = "white") +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "steelblue"),
                    labels = c("0" = "Out of Project", "1" = "In Project")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "% of ....",fill = NULL,
       title = "% of improved seeds 2022") +
  theme_minimal(base_family = "serif")



# REG  ----
library(tidyr)
library(purrr)
library(broom)

# 1) model formula 
# NO BASLINE "YIELD %" VALUES IN THIS REG

fml_seeds <- pct ~ in_project + dist_Km_boundary  +
  hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +
  housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property +
  total_livestock + total_farm_equipments

# 2) Nest by status and fit
fits_improved_seeds_22 <- improved_seeds_22 %>%
  group_by(status) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(fml_seeds, data = .x)),
    coefs = map(model, tidy),
    stats = map(model, glance))

# 3) Stacked outputs
coef_seeds <- fits_improved_seeds_22 %>% unnest(coefs) %>% 
  filter(term == "in_project") %>%
  select(status, estimate, std.error, p.value) %>% ungroup()
##
stats_seeds <- fits_improved_seeds_22 %>% unnest(stats) %>%  
  select(status,nobs, r.squared) %>% 
  rename(Num.Obs.=nobs,R2=r.squared ) %>% ungroup()

# 4) Join with model summary stats
inproj_with_fit_seeds <- coef_seeds %>%
  left_join(stats_seeds,by = "status") %>% ungroup()


twoRows <- 
  tribble(~metric,  ~pct_per_group, ~pct_per_hh,
          "Control vars", "Yes", "Yes",
          "Dist. boundary", "Yes", "Yes",
          "Baseline value", "No","No")

control_mean <- 
  improved_seeds_22 %>% group_by(in_project) %>% 
  summarise(
    pct_per_group =  mean(pct_per_group),
    pct_per_hh =  mean(pct_per_hh)
  ) %>% 
  filter(in_project == 0) %>% rename(metric=in_project)
control_mean$metric[control_mean$metric==0] <- "control_mean"


df_html_seeds <- inproj_with_fit_seeds %>%
  pivot_longer(-status, 
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = status, values_from = value
  ) %>% 
  rbind(control_mean) %>% 
  mutate(across(-metric, ~ case_when(
    metric == "std.error" ~ paste0("(", round(.x, 3), ")"),
    metric == "p.value"   ~ paste0("[", round(.x, 3), "]"),
    TRUE                  ~ as.character(round(.x,3))
  ))) %>% 
  rbind(twoRows)

# reg table
library(kableExtra)
df_html_seeds[c(1:3,7:9,4:6),] %>% 
  kable("html", caption = "Percentage of HH use Improved seeds",align = "c") %>%
  kable_classic( full_width = F) 


# PLOT ----
library(jtools)
models_list <- fits_improved_seeds_22 %>% 
  { setNames(.$model, .$status) }    # names become model names in the legend


m1_plot <- 
  plot_summs(models_list ,coefs = c("In Project" = "in_project"),
             model.names = c(
               "% of farms using improved seeds",
               "% of improved seeds used by individual farmer"),
             inner_ci_level = NULL, point.shape = F) + 
  labs(x = "Percentage of Improved seeds", y = NULL) +
  xlim(-0.05, 0.15) 

m1_plot + mp_theme



