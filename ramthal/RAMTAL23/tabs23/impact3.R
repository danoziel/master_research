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

control_vars <- demographic_vars_2016 %>% #former demog_vars
  mutate(hh_haed_edu_level=
           ifelse(is.na(hh_haed_edu_level),0,
                  hh_haed_edu_level ) ) 

treatment <- rmtl_16_18_22_sample %>% select(hh_id, sample) %>%
  rename(in_project=sample)

# The Model                     ----
#
#| 𝑌𝑖𝑡 =𝛽0 +𝛽1 in_project𝑖 +𝛽2 varible2016 t +γX 𝑖𝑡 + ϵ 𝑖𝑡


#### IRRIGATION   _________________________________________________________ ----

# df to drip use [irrigation_dist]       ----        
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
irrigation_BL_to_22 <- 
  full_join(irrigation_2022, irrigation_2018_2020) %>% 
  full_join(irrigation_2017) %>% 
  full_join(irrigation_BL)%>% 
  full_join(irrigation_6y) %>% 
  full_join(treatment) %>% 
  full_join(control_vars) 

### df 
irrigation_dist <- 
  rd_water_with_coords %>% st_drop_geometry() %>% 
  select(hh_id, dist_to_south_m)  %>% 
  left_join(irrigation_BL_to_22
            ) %>% 
  mutate(dist_to_south_km= # transform `out` to negative & "m" to "km"
           ifelse(in_project==0,dist_to_south_m*(-0.001),
                  dist_to_south_m*0.001))

# table stat 
irrigation_dist %>% filter(!is.na(drip_use_2022)) %>% 
  count(in_project,drip_use_2022) %>% 
  group_by(in_project) %>% mutate(N=sum(n),n/N)

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
 





# function [func_drip_dist] [func_drip]  ----

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

# cutoff 1.5 Km
model_drip_installed_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("DI_installed")
model_drip_18_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("drip_use_2018")
model_drip_19_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("drip_use_2019")
model_drip_20_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("drip_use_2020")
model_drip_22_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("drip_use_2022")
model_drip_6y_1.5 <- irrigation_dist %>% filter(dist_to_south_km<1.5) %>% func_drip_dist("drip_use_6y")

# NO cutoff
model_drip_installed <- irrigation_dist %>% func_drip_dist("DI_installed")
model_drip_17 <- irrigation_dist %>% func_drip_dist("drip_use_2017")
model_drip_18 <- irrigation_dist %>% func_drip_dist("drip_use_2018")
model_drip_19 <- irrigation_dist %>% func_drip_dist("drip_use_2019")
model_drip_20 <- irrigation_dist %>% func_drip_dist("drip_use_2020")
model_drip_22 <- irrigation_dist %>% func_drip_dist("drip_use_2022")
model_drip_6y <- irrigation_dist %>% func_drip_dist("drip_use_6y")

# NO dist
model_drip_installed_NOdist <- irrigation_dist %>% func_drip("DI_installed")
model_drip_17_NOdist <- irrigation_dist %>% func_drip("drip_use_2017")
model_drip_18_NOdist <- irrigation_dist %>% func_drip("drip_use_2018")
model_drip_19_NOdist <- irrigation_dist %>% func_drip("drip_use_2019")
model_drip_20_NOdist <- irrigation_dist %>% func_drip("drip_use_2020")
model_drip_22_NOdist <- irrigation_dist %>% func_drip("drip_use_2022")
model_drip_6y_NOdist <- irrigation_dist %>% func_drip("drip_use_6y")

# reg TABLE ----

# Regression Results | 1.5 Km cutoff
modelsummary(
  list("DI installed"= model_drip_installed_1.5,
    "DI usage 2023"= model_drip_17_1.5, "DI usage 2020"= model_drip_20_1.5,
    "DI usage 2019"= model_drip_19_1.5, "DI usage 2018"= model_drip_18_1.5,
    "DI usage 2017"= model_drip_17_1.5, "DI usage 6 years"= model_drip_6y_1.5),
  coef_map = c(
    in_project = "In Project",dist_to_south_km = "Distance to Boundary (Km)",
    drip_use_BL= "Baseline | Lagged Outcome"),
  estimate  = "{estimate}", statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | 1.5 Km cutoff")


# Regression Results | 10 Km cutoff
modelsummary(
  list("DI installed"= model_drip_installed,
    "DI usage 2023"= model_drip_22, "DI usage 2020"= model_drip_20,
    "DI usage 2019"= model_drip_19,"DI usage 2018"= model_drip_18,
    "DI usage 2017"= model_drip_17, "DI usage 6 years"= model_drip_6y),
  coef_map = c(in_project= "In Project",
               dist_to_south_km= "Distance to Boundary (Km)",
               drip_use_BL= "Baseline | Lagged Outcome"),
  estimate  = "{estimate}",  # print estimate alone (no parentheses)
  statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | 10 Km cutoff")



# Regression Results | NO Distance
modelsummary(
  list("DI installed"= model_drip_installed_NOdist,
    "DI usage 2023"= model_drip_17_NOdist, "DI usage 2020"= model_drip_20_NOdist,
    "DI usage 2019"= model_drip_19_NOdist, "DI usage 2018"= model_drip_18_NOdist,
    "DI usage 2017"= model_drip_17_NOdist, "DI usage 6 years"= model_drip_6y_NOdist),
  coef_map = c(in_project = "In Project",
               drip_use_BL= "Baseline | Lagged Outcome"),
  estimate  = "{estimate}",statistic = c("{std.error}", "{p.value}"),
  title= "Drip‐Use Regression Results | NO Distance Var")


# summary(model_drip_6y)
# summ(model_drip_6y)
# tidy(model_drip_22, conf.int = TRUE) 
# modelsummary(list("Simple" = model_small, "Full" = model_big))
sjPlot::tab_model(model_drip_6y ,show.se = T,digits = 5, show.stat=T )


# PLOT reg ----

library(jtools)
library(ggstance)

models_1.5 <- 
  plot_summs(model_drip_installed_1.5,
    model_drip_22_1.5, model_drip_20_1.5, model_drip_19_1.5,
    model_drip_18_1.5, model_drip_17_1.5, model_drip_6y_1.5,
    coefs = c("In Project"= "in_project",
            "Distance to \nSouthen Boundary"= "dist_to_south_km",
            "Baseline \nLagged Outcome" = "drip_use_BL"),
  model.names = c("DI installed",
                  "DI usage 2023", "DI usage 2020","DI usage 2019",
                  "DI usage 2018","DI usage 2017","DI usage 6 years"),
  inner_ci_level = NULL, point.shape = FALSE)+
  labs(x = "Beta Estimate", y = NULL)

models <- 
  plot_summs(model_drip_installed,
    model_drip_22, model_drip_20, model_drip_19,
    model_drip_18, model_drip_17, model_drip_6y,
    coefs = c("In Project"= "in_project",
              "Distance to \nSouthen Boundary"= "dist_to_south_km",
              "Baseline \nLagged Outcome" = "drip_use_BL"),
    model.names = c("DI installed",
                    "DI usage 2023", "DI usage 2020","DI usage 2019",
                    "DI usage 2018","DI usage 2017","DI usage 6 years"),
    inner_ci_level = NULL, point.shape = FALSE)+
  labs(x = "Beta Estimate", y = NULL)

models_NOdist <- 
  plot_summs(model_drip_installed_NOdist,
    model_drip_22_NOdist, model_drip_20_NOdist, model_drip_19_NOdist,
    model_drip_18_NOdist, model_drip_17_NOdist, model_drip_6y_NOdist,
    coefs = c("In Project"= "in_project",
              "Baseline \nLagged Outcome" = "drip_use_BL"),
    model.names = c("DI installed",
                    "DI usage 2023", "DI usage 2020","DI usage 2019",
                    "DI usage 2018","DI usage 2017","DI usage 6 years"),
    inner_ci_level = NULL, point.shape = FALSE)+
  labs(x = "Beta Estimate", y = NULL)


#custom theme to format 
apatheme=theme_bw()+
  theme(
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))


# PLOT THE REG COFF
models_1.5 + apatheme + ggtitle("Key Coefficients | 1.5 Km Distance")
models + apatheme + ggtitle("Key Coefficients | 10 Km Distance")
models_NOdist + apatheme+  ggtitle("Key Coefficients | No Distance")
# scale_x_continuous(breaks = seq(-0.4,0.4,0.1) )



# PLOT dist_rd ----

dist_rd <- 
  irrigation_dist %>%
  filter(dist_to_south_km >= (-10)) %>% 
  mutate(dist_1.5Km = ifelse(dist_to_south_km <= 1.5, dist_to_south_km,NA ) 
         ) %>% 
  mutate(dist_1.5Km = ifelse(dist_1.5Km >= (-1.5), dist_1.5Km,NA ) 
  ) %>% 
  mutate(dist_1.5Km_sq= dist_1.5Km^2,
         dist_1.5Km_sq= ifelse(in_project==0,dist_1.5Km_sq*(-1),dist_1.5Km_sq ))




# rd_drip_dist <- rdrobust(y = dist_rd$drip_use_2022, x = dist_rd$dist_to_south_km)
# summary(rd_drip_dist)


## custom theme to RD plot format ----
themeRD=theme_minimal()+
  theme(
    panel.grid.minor=element_blank(),
    axis.line=element_line(),
    text=element_text(family="serif"),
    legend.title=element_blank(), 
    axis.text=element_text(size=14),
    axis.title=element_text(size=10),
    legend.text = element_text(size = 12))

custom_layers <- list(
  scale_color_manual(
    values = c("0" = "orange3", "1" = "dodgerblue3"),
    labels= c("Out project","In project")),
  labs(x= "Km Distance to Boundary",y= "Percent of Farm"),
  themeRD) # add custom theme 



# DI_installed  dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= DI_installed, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers +
  ggtitle("RD | DI installed")

# drip_use_2022  dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_2022, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+
  custom_layers +
  ggtitle("RD | DI usage 2023") +
  coord_cartesian(ylim = c(-0.05,, 0.15)) # Plot window to go from 0 to 0.5 without dropping any LOESS points

# drip_use_6y  dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_6y, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+ custom_layers + 
  ggtitle("RD | DI usage 6 years") +coord_cartesian(ylim = c(0, 0.5))

# drip_use_2017  dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_2017, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+ custom_layers + 
  ggtitle("RD | DI usage 2017") +coord_cartesian(ylim = c(-0.05, 0.2))

# drip_use_2018   dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_2018, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+ custom_layers + 
  ggtitle("RD | DI usage 2018") +coord_cartesian(ylim = c(-0.05, 0.3))

# drip_use_2019   dist_to_south_km
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_2019, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+ custom_layers + 
  ggtitle("RD | DI usage 2019") +coord_cartesian(ylim = c(-0.05, 0.2))

# drip_use_2020
ggplot(dist_rd,aes(x = dist_1.5Km,  y= drip_use_2020, col= factor(in_project))) +
  geom_point(color='white') +geom_smooth(method = "loess")+ custom_layers + 
  ggtitle("RD | DI usage 2020") +coord_cartesian(ylim = c(-0.05, 0.1))





rdplot(dist_rd$drip_use_6y, dist_rd$dist_1.5Km,
       title="Drip Installation | Distance to Boundary",
       x.label=NULL,y.label="Users", y.lim = c(0, 0.5))









# rd_land_with_coords ----
rd_land_with_coords %>% 
  mutate(dist_to_south_m=ifelse(in_project==0,dist_to_south_m*(-1),dist_to_south_m))












