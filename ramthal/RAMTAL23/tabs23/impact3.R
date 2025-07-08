# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(summarytools)

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

# reg for drip ----

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





# reg TABLE ----

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


# PLOT reg ----

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



# PLOT dist_rd ----

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




# CULTIVATED LAND -----


library(writexl)
write_xlsx(crop_acre_22, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_22.xlsx")
write_xlsx(crop_acre_BL, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/crop_acre_BL.xlsx")

rmtl_cntrl_vars <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_cntrl_vars.xlsx")



#### DS | {season} {farmers_hh} {acre_cult} {acre_drip} {acre_ir} --- --- ----
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

### DF for reg CROP  ----

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
  
  mutate(season=ifelse(season=="rabi_2021_22","Rabi","Kharif")) %>% 
  group_by(hh_id,season,crop) %>% summarise(
    acre_cult=mean(acre_cult),acre_drip=mean(acre_drip), acre_ir=mean(acre_ir),
    .groups="drop") %>% 
  select(-season)


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


crop_acre_BL <- crop_acre_16 %>% 
  select(hh_id, crop, acre_cult_BL, acre_drip_BL, acre_ir_BL)




### reg ###
crop_Sunf <-crop_acre_22 %>% filter(crop=="Sunflower") %>% 
  right_join(rmtl_cntrl_vars) %>% left_join(crop_acre_BL) %>% 
  mutate(across(c(acre_cult, acre_drip, acre_ir,acre_cult_BL, acre_drip_BL, acre_ir_BL), ~replace_na(., 0)))


mod_10 <-  # acre_drip acre_ir acre_cult
  lm(acre_cult  ~ in_project + elevation_0m +  acre_cult_BL  +
       hh_haed_age + hh_haed_gendar + hh_haed_edu_level + total_acre16 + 
       housing_str321 + job_income_sourceS +govPnsin_scheme +rent_property +
       total_livestock + total_farm_equipments,
     data = crop_Sunf)
# summary(mod_10)
 sjPlot::tab_model(mod_10 ,  show.se = T,digits = 5, show.stat  = F )


#### ## |







