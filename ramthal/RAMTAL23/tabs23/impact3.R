# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(summarytools)


## DFs  ________________________________________________________________ _ ----
library(readr)
BL_2015_16_crop_IRsource_IRmethod <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL_2015_16_crop_IRsource_IRmethod.csv")
rmtl_16_18_22_sample <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_16_18_22_sample.csv")
demographic_vars_2016 <- # baseline vars
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/demographic_vars_2016.csv")

centroids_coords <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_coords.csv")

library(sf)
centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)

library(readxl)
elevation_points_tbl<- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_tbl.xlsx")


control_vars <- #  1,612
  demographic_vars_2016 %>% #former demog_vars
  mutate(hh_haed_edu_level=
           ifelse(is.na(hh_haed_edu_level),0,
                  hh_haed_edu_level ) ) 

dist_var <- #  1,409
  rd_water_with_coords %>% st_drop_geometry() %>% 
  select(in_project,hh_id,dist_to_south_m) %>% 
  mutate(dist_to_south_km= # transform `out` to negative & "m" to "km"
           ifelse(in_project==0,dist_to_south_m*(-0.001),
                  dist_to_south_m*0.001)) %>% select(-in_project)

geo_var <- #  1,612
  rmtl_InOut %>% 
  select(hh_id, elevation , south1_north0, a5)

elv_var <- 
  list_shape_code %>% left_join(elevation_points_tbl) %>% 
  right_join(
    rmtl_InOut %>% select(hh_id, elevation , south1_north0, a5)
  ) %>% select(hh_id , elevation_m, south1_north0 ,a5)

Qmm2=rmtl_srvy22 %>% select(hh_id,mm2) %>% 
  mutate(im_in_project = ifelse(mm2==1,1,0)) %>% select(hh_id,im_in_project)

rmtl_con_vars <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_con_vars.csv")
rmtl_con_vars <- 
  as_tibble(control_vars) %>% rename(in_project= in1_out0) %>% 
  left_join(elv_var) %>% 
  left_join(dist_var) 


library(readxl)
rmtl_cntrl_vars <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_cntrl_vars.xlsx")
rmtl_cntrl_vars <- rmtl_con_vars %>% 
  left_join(elv_var) %>% rename(village=a5) %>% 
  left_join(Qmm2) %>% 
  mutate(dist_to_south_km=
           ifelse(in_project==0,dist_to_south_km*(-1),dist_to_south_km)) %>% 
  mutate(elevation_0m = ifelse(in_project==0,(elevation_m-511)*(-1),elevation_m-511)) %>% 
  mutate(elevation_km = elevation_0m * 0.001) %>% 
  mutate(elevation_sqrt = ifelse(in_project==0,(elevation_km^2)*(-1),(elevation_km^2)))  %>% 
  select(-elevation_m)


library(writexl)
write_xlsx(rmtl_cntrl_vars, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_cntrl_vars.xlsx")
rmtl_cntrl_vars <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_cntrl_vars.xlsx")


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
  right_join(rmtl_cntrl_vars)

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
    "in_project","elevation_0m","drip_use_BL",
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
    "in_project","dist_to_south_km","drip_use_BL",
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
  covariates <- c(
    "in_project","drip_use_BL",
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

# reg for drip       ----

#| "MODEL 1" elevation_0m     TO impact_ir
#| "MODEL 2" dist_to_south_km TO impact_ir
#| "MODEL 3" dist_to_south_km TO impact_ir_south


# df <- irrigation_dist %>% filter(dist_to_south_m <1500)
# df <- irrigation_dist %>% filter(south1_north0 == 1)

impact_ir_south <- impact_ir %>% filter(dist_to_south_m <1500)
impact_ir_south <- impact_ir %>% filter(south1_north0 == 1)


mod_10 <-  # acre_drip acre_ir acre_cult
  lm(drip_use_6y  ~ in_project + elevation_0m + drip_use_BL + 
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





# reg TABLE          ----

# "MODEL 1" elevation_0m TO impact_ir
modelsummary(
  list("I'm in Project"= m1_iam, "DI installed"= m1_ins,
       "DI usage 2021-22"= m1_22, "DI usage 2020"= m1_20,
       "DI usage 2019"= m1_19, "DI usage 2018"= m1_18,
       "DI usage 2017"= m1_17, "DI usage 6 years"= m1_6y),
  coef_map = c( in_project = "In Project",elevation_0m = "Elevation",
                drip_use_BL= "Baseline Value",`(Intercept)`="Constant"), 
  estimate  = "{estimate}",statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | Elevation")

# Regression Results | South 1.5 Km cutoff
modelsummary(
  list("I'm in Project"= m2_iam, "DI installed"= m2_ins,
       "DI usage 2021-22"= m2_22, "DI usage 2020"= m2_20,
       "DI usage 2019"= m2_19, "DI usage 2018"= m2_18,
       "DI usage 2017"= m2_17, "DI usage 6 years"= m2_6y),
  coef_map = c(
    in_project = "In Project",dist_to_south_km = "Distance to Boundary (Km)",
    drip_use_BL= "Baseline Value",`(Intercept)`="Constant"), 
  estimate  = "{estimate}", statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | Distance Boundary")


# Regression Results | Elevation | South 1.5 Km cutoff
modelsummary(
  list("I'm in Project"= m3_iam, "DI installed"= m3_ins,
       "DI usage 2021-22"= m3_22, "DI usage 2020"= m3_20,
       "DI usage 2019"= m3_19, "DI usage 2018"= m3_18,
       "DI usage 2017"= m3_17, "DI usage 6 years"= m3_6y),
  coef_map = c(
    in_project = "In Project",dist_to_south_km = "Distance to Boundary (Km)",
    drip_use_BL= "Baseline Value",`(Intercept)`="Constant"), 
  estimate  = "{estimate}", statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | 1.5 Km Southern Boundary")

# summary(model_drip_6y)
# summ(model_drip_6y)
# tidy(model_drip_22, conf.int = TRUE) 
# modelsummary(list("Simple" = model_small, "Full" = model_big))
sjPlot::tab_model(model_drip_6y ,show.se = T,digits = 5, show.stat=T )

#|=============================================================================


# PLOT reg           ----

library(jtools)
library(ggstance)
library(RColorBrewer)
my_colors <- brewer.pal(8, "Paired")

m1_plot <- 
  plot_summs(m1_iam,m1_ins,m1_22, m1_20, m1_19,m1_18, m1_17, m1_6y,
             coefs = c("In Project" = "in_project"),
             model.names = c("Im in Project","DI installed","DI usage 2021-22", "DI usage 2020",
                             "DI usage 2019","DI usage 2018","DI usage 2017","DI usage 6 years"),
             inner_ci_level = NULL,
             point.shape = FALSE,
             colors = my_colors
  ) + labs(
    # subtitle = "Elevation as an Exogenous Variable [N=1,612]",
    # caption = "Note: Estimates reflect standardized coefficients with 95% CIs.",
    x = "Beta Estimate", y = NULL)

m2_plot <- 
  plot_summs(
    m2_iam, m2_ins, m2_22, m2_20, m2_19, m2_18, m2_17, m2_6y,
    coefs = c("In Project" = "in_project"),
    model.names = c("Im in Project","DI installed","DI usage 2021-22", "DI usage 2020",
                    "DI usage 2019","DI usage 2018","DI usage 2017","DI usage 6 years"),
    inner_ci_level = NULL,
    point.shape = FALSE,
    colors = my_colors) +
  labs(x = "Beta Estimate",y = NULL)


m3_plot <- 
  plot_summs(m3_iam,m3_ins, m3_22, m3_20, m3_19, m3_18, m3_17, m3_6y,
             coefs = c("In Project" = "in_project"),
             model.names = c("Im in Project","DI installed","DI usage 2021-22", "DI usage 2020",
                             "DI usage 2019","DI usage 2018","DI usage 2017","DI usage 6 years"),
             inner_ci_level = NULL,
             point.shape = FALSE,
             colors = my_colors
  ) + labs(x = "Beta Estimate", y = NULL)




#custom theme to format 
mp_theme=theme_bw()+
  theme(
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))


# PLOT THE REG COFF
m1_plot + mp_theme + ggtitle("`Elevation` as an Exogenous Variable [Entire sample]")
m2_plot + mp_theme + ggtitle("`Distance to Boundary` as an Exogenous Variable [Entire sample]")
m3_plot + mp_theme+  ggtitle("`Distance to Boundary` as an Exogenous Variable [Southern sample]")
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

### DF to acre_cult [  ]
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
plots_crop_2022 %>% count(common_n2_family)


#######  DFs [df100]  [df300]            ####

df100 <- 
  BL_plot.Crop.Yield %>% 
  select(hh_id,season,plotID,crop_common, yield) %>% 
  filter (yield >0) %>% 
  left_join(BL_plotAcre) %>%  filter(plot_acre>0) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n()) %>% ungroup() %>% 
  mutate(crop_acre = plot_acre/n ) %>% select(-n,-X) %>% 
  mutate(yield =ifelse(yield<50,yield*100,yield) ) %>% 
  rename(crop=crop_common, kg_crop =yield) %>% mutate(survey="bl")
##
##
##
df300 <- 
  plots_crop_2022 %>% 
  select(hh_id,season,plotID,crop_number ,common_n_family) %>% 
  left_join(
    a_total_yield %>% select(-farmers_hh ) ) %>% 
  left_join(total_acre_22) %>% 
  filter(kg_crop>0,acres>0)%>% select(-crop_number ) %>% 
  group_by(hh_id, season, plotID) %>% mutate(n=n()) %>% ungroup() %>% 
  mutate(crop_acre = acres/n)%>% select(-n) %>% 
  rename(crop=common_n_family, plot_acre=acres)%>% mutate(survey="mid2") 
df300$crop[df300$crop=="Bengal gram"] <- "Bengal_gram"
df300$crop[df300$crop=="Sorghum/jowar"] <- "Sorghum_jowar"


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


### ###  kg per plot ### ### ----
df400 <- rbind(df100,df300
               ) %>% 
  group_by(survey,hh_id, season, plotID) %>% # SUM plot kg
  summarise(plot_kg=sum(kg_crop,na.rm = T),
            plot_acre=sum(plot_acre,na.rm = T),.groups = "drop" 
            ) %>% 
  group_by(survey,hh_id, season) %>% 
  summarise(hh_kg=sum(plot_kg,na.rm = T), # SUM HH kg 
            hh_acre=sum(plot_acre,na.rm = T),.groups = "drop" 
            ) %>% 
  mutate(kg_per_acre= hh_kg/hh_acre) %>% 
  mutate(Season = sub("_.*", "", season)) 


df400 %>% left_join(hh_2022) %>% 
  group_by(survey, Season,farmers_hh) %>% summarise(mean(kg_per_acre))


### ###  kg per crop [kg_per_acre_15_22] ### ### ----

# Too few observations  for "Vegetables","Fruits", "VegetablesANDFruits"  "Sugarcane" "Onions","Chillies",, "Oilseeds"

df401 <- 
  rbind(df100,df300) %>% 
  filter(crop %in% c ( "Sunflower","Maize",  "Toor", "Bengal_gram","Sorghum", "Greengram") 
         )%>% 
  group_by(survey,hh_id, season, crop) %>% # SUM plot kg
  summarise(kg_crop=sum(kg_crop,na.rm = T),
            crop_acre=sum(crop_acre,na.rm = T),.groups = "drop" 
  ) %>% 
  group_by(survey,hh_id, crop) %>% 
  summarise(hh_kg=sum(kg_crop,na.rm = T), # SUM HH kg 
            hh_acre=sum(crop_acre,na.rm = T),.groups = "drop" 
  ) %>% 
  mutate(kg_per_acre= hh_kg/hh_acre)

kg_per_acre_15_22 <- 
  df401 %>% 
  mutate(crop_survey = paste(crop, survey, sep = "_")) %>% 
  select(hh_id, crop_survey, kg_per_acre) %>% 
  pivot_wider(names_from = crop_survey,values_from =kg_per_acre )%>% 
  mutate(across(everything(), ~replace_na(.x, 0))
         ) %>% 
  right_join(rmtl_cntrl_vars)


# REG ----

# mod_10 <- lm (Sunflower_mid2 ~ in_project+Sunflower_bl+elevation_0m+
#                 hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +total_livestock + total_farm_equipments,
#                 data = kg_per_acre_15_22)
# summary(mod_10)
# sjPlot::tab_model(mod_10 ,  show.se = T,digits = 5, show.stat  = F )

library(broom)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# get crop names from the column names
outcomes <- grep("_mid2$", names(kg_per_acre_15_22), value = TRUE)
baselines <- grep("_bl$", names(kg_per_acre_15_22), value = TRUE)
crop_names <- gsub("(_mid2|_bl)$", "", outcomes)
controls <- "in_project + elevation_0m + hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + housing_str321 + job_income_sourceS + govPnsin_scheme + rent_property + total_livestock + total_farm_equipments"

# library(purrr)
models <- map(crop_names, function(crop) {
  outcome <- paste0(crop, "_mid2")
  baseline <- paste0(crop, "_bl")
  formula <- as.formula(
    paste(outcome, "~ in_project +", baseline, "+", controls)
  )
  lm(formula, data = kg_per_acre_15_22)
})

# library(broom)
results <- map(models, tidy)
summaries <- map(models, summary)

# reg table ----
# Extract "in_project" info and summary stats for each model
summ_table <- map2_dfr(models, crop_names, function(model, crop) {
  if (is.null(model)) return(tibble(crop = crop, coef = NA, sd = NA, p = NA, N = NA, R2 = NA))
  tidy_mod <- tidy(model)
  in_proj <- tidy_mod %>% filter(term == "in_project")
  tibble(
    crop = crop,
    coef = in_proj$estimate,
    sd = in_proj$std.error,
    p = in_proj$p.value,
    N = nobs(model),
    R2 = summary(model)$r.squared
  )
})


# df 1
summ_table_fmt <- summ_table %>%
  mutate_at(c(2:4,6), round, 3) %>%
  pivot_longer(-crop, names_to = "stat", values_to = "value") 

# control_mean
df_control_mean <- df401 %>%
  right_join(rmtl_cntrl_vars) %>%
  filter(survey == "mid2", in_project == 0) %>%
  group_by(crop) %>%
  summarise(value = round(mean(kg_per_acre), 2)) %>% 
  mutate(stat="Control mean") %>% select(crop,stat,value)

# Bind this row to your existing summary table
df_table_fmt <- 
  bind_rows(summ_table_fmt,df_control_mean) %>% 
  mutate(
    value = ifelse(stat == "sd", paste0("(", value, ")"), as.character(value)),
    value = ifelse(stat == "p", paste0("[", value, "]"), as.character(value))
  ) %>%
  pivot_wider(names_from = crop, values_from = value) %>% 
  mutate(stat=ifelse(stat %in% c("sd","p"),"",
              ifelse(stat=="coef","In project", stat)) ) %>% 
  rbind(twoRows ) %>% rename(`_`= stat)

df_html <- df_table_fmt[c(1:3,6:8,4:5) ,]

df_html %>%  
  kable("html", caption = "Yield | Kg per acre",align = "c") %>% 
  kable_classic_2()  %>%
  add_header_above(c(" ", "(1)" = 1, "(2)" = 1, "(3)" = 1, "(4)" = 1,"(5)" = 1 ))

twoRows <- tribble( ~stat, ~Maize,  ~Toor, ~Sunflower, ~Bengal_gram	, ~Greengram,
  "Control vars", "X", "X", "X", "X", "X", "Dist. boundary", "X", "X", "X", "X", "X"
)
twoRows <- tribble( ~stat, ~Maize,  ~Toor, ~Sunflower, ~Bengal_gram	, ~Greengram,
                    "Control vars", "Yes", "Yes", "Yes", "Yes", "Yes", "Dist. boundary", "Yes", "Yes", "Yes", "Yes", "Yes"
)





