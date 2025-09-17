# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(summarytools)
library(ggplot2)

## DFs  ________________________________________________________________ _ ----
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



control_vars <- #  1,612
  demographic_vars_2016 %>% #former demog_vars
  mutate(hh_haed_edu_level=
           ifelse(is.na(hh_haed_edu_level),0,
                  hh_haed_edu_level ) ) 

dist_elev_var  <-  # former [dist_var]
  Ramthal_dist_elev  %>% left_join(hh_2022) %>% 
  select(in1_out0,hh_id, dist_to_boundary_m, elevation_m) %>% # former var dist_to_south_m
  mutate(dist_Km_boundary =  dist_to_boundary_m*0.001,
         dist_Km_boundary = ifelse(in1_out0==0, dist_Km_boundary*-1,dist_Km_boundary)
         ) %>% 
  mutate(elevation = elevation_m-506,
         elevation= ifelse(in1_out0==0, elevation*-1 ,elevation))%>% 
  select(-in1_out0)

geo_var <- #  1,612
  rmtl_InOut %>% 
  select(hh_id, elevation , south1_north0, a5) %>% 
  rename(elev_cat = elevation,village=a5) %>% 
  mutate(elev_cat=ifelse(elev_cat=="7+",7,elev_cat),
         elev_cat=as.numeric(elev_cat))

Qmm2=rmtl_srvy22 %>% select(hh_id,mm2) %>% 
  mutate(im_in_project = ifelse(mm2==1,1,0)) %>% select(hh_id,im_in_project)

rmtl_con_vars <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_con_vars.xlsx")

rmtl_con_vars <- 
  as_tibble(control_vars) %>% rename(in_project= in1_out0) %>% 
  left_join(dist_elev_var) %>% 
  left_join(geo_var) %>% 
  left_join(Qmm2)

library(writexl)
write_xlsx(rmtl_con_vars, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_con_vars.xlsx")


#### IRRIGATION   _________________________________________________________ ----

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

display.brewer.pal(n = 11, name = 'Paired')
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




#                    ###  CULTIVATED LAND ################             ###-----

#### DS {season} {acre cult drip ir} --- --- ----
plots_crop_2022 %>%  
  select(hh_id,season,plotID) %>% distinct() %>% 
  left_join(a_plots_size %>% select(hh_id,plotID,acres)) %>% 
  left_join(a_irri_rain_method %>% select(plotID,season,hh_id,irri_method) %>% distinct()) %>% 
  mutate(irri_method=ifelse(irri_method=="drip","drip",ifelse(irri_method=="rain","rain","ir"))
         ) %>% 
  group_by(hh_id,season,plotID) %>% 
  mutate(n=n()) %>% ungroup() %>% mutate(acres=acres/n
                                         ) %>% 
  group_by(hh_id,season,irri_method) %>% 
  summarise(acre_cult=sum(acres),.groups="drop") %>% 
  mutate(acre_drip=ifelse(irri_method=="drip",acre_cult,0)) %>% 
  mutate(acre_ir=ifelse(irri_method=="ir",acre_cult,0))%>% 
  left_join(rmtl_InOut %>% select(hh_id, farmers_hh)
  ) %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(season, farmers_hh) %>% 
  summarise(acre_cult=mean(acre_cult),
            acre_drip=mean(acre_drip),
            acre_ir=mean(acre_ir),.groups="drop"
            )

#    DF for reg CROP  ----

### [crop_acre_22]
###
crop_acre_22 <- 
  plots_crop_2022 %>% # in DF.22.R
  select(hh_id,season, common_n2_family,plot_crop,plotID) %>%
  left_join(a_plots_size %>% select(hh_id,plotID,acres)) %>% 
  left_join(a_irri_rain_method %>% select(plot_crop,season,hh_id,irri_method)
  ) %>% mutate(irri_method=ifelse(irri_method=="drip","drip",ifelse(irri_method=="rain","rain","ir"))
  ) %>% rename(crop= common_n2_family) %>% 
  group_by(hh_id,season,plotID) %>% mutate(n=n()) %>% ungroup() %>% 
  mutate(acres=acres/n
  ) %>% 
  group_by(hh_id,season,crop,irri_method) %>% 
  summarise(acre_cult=sum(acres),.groups="drop") %>% 
  mutate(acre_drip=ifelse(irri_method=="drip",acre_cult,0)) %>% 
  mutate(acre_ir=ifelse(irri_method=="ir",acre_cult,0)
  ) %>% 
  group_by(hh_id,season,crop) %>% summarise(
    acre_cult=sum(acre_cult),acre_drip=sum(acre_drip), acre_ir=sum(acre_ir),.groups="drop"
  ) %>% 
  # Until now - seasons were calculated separately
  # now seasons can be REMOVEed in order to calculate CROPS separately
  group_by(hh_id,crop) %>% summarise(
    acre_cult=mean(acre_cult),acre_drip=mean(acre_drip), acre_ir=mean(acre_ir),
    .groups="drop")
###
crop_acre_22$crop[crop_acre_22$crop=="Oil seeds"] <- "Oilseeds"
crop_acre_22$crop[crop_acre_22$crop=="Bengal gram"] <- "Bengal_gram"
crop_acre_22$crop[crop_acre_22$crop=="Sorghum/jowar"] <- "Sorghum_jowar"

library(writexl)
write_xlsx(crop_acre_22, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")



###
### [crop_acre_BL]
###
crop_acre_16 <- 
  BL_2015_16_crop_IRsource_IRmethod %>% 
  select(hh_id, season, crop_common, plot_num ,crop_num, plot_acre, irri_method) %>% 
  # filter(season != "rabi_2015_16") %>% 
  mutate(irri_method=ifelse(irri_method=="Drip","drip",ifelse(irri_method=="Rain","rain","ir"))
  ) %>% rename(crop= crop_common) %>% 
  filter(!is.na(plot_acre)) %>% 
  group_by(hh_id,season,plot_num) %>% mutate(n=n())%>% ungroup() %>% 
  mutate(acres=plot_acre/n) %>% 
  group_by(hh_id,season,crop,irri_method) %>% 
  summarise(acre_cult_BL=sum(acres),.groups="drop") %>% 
  
  mutate(acre_drip_BL=ifelse(irri_method=="drip",acre_cult_BL,0)) %>% 
  mutate(acre_ir_BL=ifelse(irri_method=="ir",acre_cult_BL,0)
  ) %>% 
  group_by(hh_id,season,crop) %>% summarise(
    acre_cult_BL=sum(acre_cult_BL),acre_drip_BL=sum(acre_drip_BL), acre_ir_BL=sum(acre_ir_BL),.groups="drop"
  ) %>% 
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(hh_id,season,crop) %>% summarise(
    acre_cult_BL=mean(acre_cult_BL),acre_drip_BL=mean(acre_drip_BL), acre_ir_BL=mean(acre_ir_BL),
    .groups="drop")
###
crop_acre_BL <- crop_acre_16 %>% 
  select(hh_id, crop, acre_cult_BL, acre_drip_BL, acre_ir_BL)

library(writexl)
write_xlsx(crop_acre_BL, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_BL.xlsx")



### reg              ####

library(readxl)
crop_acre_22 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")
crop_acre_BL <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_BL.xlsx")

### DF to acre_cult [  ] Sunflower VegetablesANDFruits Oilseeds Sugarcane Toor Bengal_gram Sorghum_jowar Greengram
##
df1_Sunf<-
  crop_acre_22 %>% filter(crop=="Sunflower") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0))) 
mn_Sunf <- df1_Sunf %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult)) %>% filter(in_project == 0) %>% pull(Mean)

##
df1_Veg<-
  crop_acre_22 %>% filter(crop=="VegetablesANDFruits") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0)))
mn_Veg <- df1_Veg %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult)) %>% filter(in_project == 0) %>% pull(Mean)

##
df1_OilS<-
  crop_acre_22 %>% filter(crop=="Oilseeds") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0))) 
mn_OilS <- df1_OilS %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult)) %>% filter(in_project == 0) %>% pull(Mean)

##
df1_SuC<-
  crop_acre_22 %>% filter(crop=="Sugarcane") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0))) 
mn_SuC <- df1_SuC %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult)) %>% filter(in_project == 0) %>% pull(Mean)

###
### 
###
df1_toor<-
  crop_acre_22 %>% filter(crop=="Toor") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% select(acre_cult_BL,everything()) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0)))
mn_toor <- df1_toor %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult)) %>% filter(in_project == 0) %>% pull(Mean)
##
df1_Bg<-
  crop_acre_22 %>% filter(crop=="Bengal_gram") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% select(acre_cult_BL,everything()) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0)))
mn_Bg <- df1_Bg %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult))%>% filter(in_project == 0) %>% pull(Mean)
##
df1_sj<-
  crop_acre_22 %>% filter(crop=="Sorghum_jowar") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% select(acre_cult_BL,everything()) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0)))
mn_sj <- df1_sj %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult))%>% filter(in_project == 0) %>% pull(Mean)
##
df1_Gg<-
  crop_acre_22 %>% filter(crop=="Greengram") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% select(acre_cult_BL,everything()) %>% 
  mutate(across(c(acre_cult, acre_cult_BL), ~replace_na(., 0)))
mn_Gg <- df1_Gg %>% group_by(in_project) %>% summarise(Mean=mean(acre_cult))%>% filter(in_project == 0) %>% pull(Mean)
###
###
df_acre_cult = df1_Sunf
df_acre_cult = df1_Veg
df_acre_cult = df1_OilS
df_acre_cult = df1_SuC
#
df_acre_cult = df1_toor
df_acre_cult = df1_Bg
df_acre_cult = df1_sj
df_acre_cult = df1_Gg

m1_Sunf <-
m1_Veg<-
m1_OilS <-
m1_SuC<- 
m1_toor<-
m1_Bg <- 
m1_sj <- 
m1_Gg <- 
  lm(acre_cult  ~ in_project + elevation_0m +  acre_cult_BL  +
       hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 +housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +total_livestock + total_farm_equipments,
     data = df_acre_cult)
summary(m1_Gg)
sjPlot::tab_model(m1_Gg ,  show.se = T,digits = 5, show.stat  = F )
 ### 
 ###
 # reg TABLE         ----
 library(modelsummary)
 # "MODEL 1" elevation_0m TO impact_ir
 # modelsummary(
 #   list("Sunflower"= m1_Sunf, "Vegetables & Fruits"= m1_Veg,
 #        "Oilseeds"= m1_OilS, "Sugarcane"= m1_SuC),
 #   coef_map = c( in_project = "In Project",elevation_0m = "Elevation",
 #                 acre_cult_BL= "Baseline Value",`(Intercept)`="Constant"), 
 #   estimate  = "{estimate}",statistic = c("{std.error}", "{p.value}"),
 #   title= "Cultivated acre land for high-value crops| Elevation")
 # 
 # 
 # modelsummary(
 #   list("Toor"= m1_toor, "Chickpea"= m1_Bg,"Sorghum"= m1_sj, "Mung"= m1_Gg),
 #   coef_map = c( in_project = "In Project"), 
 #   estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
 #   gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",title= "Cultivated acre by crops")
 
 
 ms_tbl <- 
   modelsummary(
   list( "Vegetables"= m1_Veg,"Sunflower"= m1_Sunf, "Oilseeds"= m1_OilS, "Sugarcane"= m1_SuC,
     "Toor"= m1_toor, "Chickpea"= m1_Bg,"Sorghum"= m1_sj, "Mung"= m1_Gg),
   coef_map = c( in_project = "In Project"), 
   estimate  = "{estimate}",statistic = c("({std.error})", "[{p.value}]"),
   gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|RMSE|F",
   output = "data.frame"
   )
 
 control_mean <- data.frame(
   term = "Control mean", 
   Vegetables = round(mn_Veg, 3),
   Sunflower = round(mn_Sunf, 3),
   Oilseeds = round(mn_OilS, 3),
   Sugarcane = round(mn_SuC, 3),
   
   Toor = round(mn_toor, 3),
   Chickpea = round(mn_Bg, 3),
   Sorghum = round(mn_sj, 3),
   Mung = round(mn_Gg, 3)
 )
 control_mean[] <- lapply(control_mean, as.character)
 
 ms_tbl %>% select( term,Vegetables, Sunflower, Oilseeds, Sugarcane,Toor, Chickpea, Sorghum, Mung) %>% 
   bind_rows(control_mean
             ) %>% 
   kable("html", caption = "My Table Title",align = "c") %>%
   kable_classic( full_width = F) %>%
   add_header_above(c(" ", "(1)" = 1, "(2)" = 1, "(3)" = 1, "(4)" = 1,
                      "(5)" = 1, "(6)" = 1, "(7)" = 1, "(8)" = 1))%>%   
   
   add_header_above(c("Crop type", "High-value" = 4, "Traditional" = 4))%>%
   collapse_rows(columns = 1, valign = "top")
 
 
 #|=============================================================================
 
 
 # PLOT reg          ----
 
 library(jtools)
 library(ggstance)
 library(RColorBrewer)
 my_colors <- brewer.pal(8,"Paired")
 
 m1_plot <- 
   plot_summs(m1_Veg, m1_Sunf, m1_OilS, m1_SuC, m1_toor, m1_Bg,m1_sj,m1_Gg ,
              coefs = c("In Project" = "in_project"),
              model.names = c("Vegetables","Sunflower","Oilseeds","Sugarcane",
                              "Toor","Chickpea","Sorghum", "Mung"),
              inner_ci_level = NULL, point.shape = FALSE, colors = my_colors) + 
   labs(x = "Acre Land", y = NULL) 

 
 # PLOT THE REG COFF  #custom theme in IRRIGATIN in this  R script
m1_plot + mp_theme + ggtitle("`Elevation` as an Exogenous Variable [Entire sample]")




#                    ### 	YIELD 	########################	           ----					

BL_plot.Crop.Yield %>% count(crop_common)


#   [df_kg_acre]            ####

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







### ###  kg per crop [df_kg_acre] ### ### ----

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
summary(ml)
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




# _________________________________________CROPPING PATTERN___________________________________----

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





rmtl_srvy22 %>% select(hh_id,farmers_hh,mm5,mw1a) %>% count(mw1a)



