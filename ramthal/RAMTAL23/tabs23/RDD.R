# 
#|     Spatial regression discontinuity
#|    ==================================

# df used in this script
rd_water_2022
rd_land_2022 # obs 1,578

library(dplyr)
library(modelsummary)

# DFs for regression  ----

rd_water_2022 <- 
  rmtl_InOut %>% select(
    hh_id,in1_out0, mm4 ,hh_6m_2021_22 , elevation, around_boundary,
    south1_north0,distance_km,a5,drip_use, ir_use) %>% 
  rename(in_project  = in1_out0, drip_installed=mm4) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) 

rd_land_2022 <- 
  cultivated_land_2022 %>% # df from impact2.R
  group_by(hh_id ) %>% 
  summarise(acre_drip=sum(acre_drip),
            acre_ir=sum(acre_ir),
            acre_cult=sum(acre_cult),# Sum
            land_holding=mean(land_holding), # Mean
            pct_cult_land=sum(pct_cult_land),
            pct_drip_land=sum(pct_drip_land),
            pct_ir_land=sum(pct_ir_land) ) %>% 
  left_join( 
    rmtl_InOut %>% 
      select(hh_id,in1_out0,drip_use,
             elevation,distance_km, around_boundary,south1_north0)
  ) %>% rename(in_project=in1_out0) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) 




# summary stats  ------------------------------

rd_water_2022 %>% count(south1_north0,in_project)
rd_water_2022 %>% count(south1_north0,distance_km)
rd_water_2022 %>% count(distance_km)
rd_water_2022 %>% filter(distance_km==1) %>% count(in_project)

 # n=805 | "1" IN south1_north0 SAME "1" + "1.5" in distance_km

# Tables  .........................................................

# WATER
rd_water_2022 %>% filter(south1_north0==1) %>% 
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup() %>% 
  kable() %>% kable_minimal() 

# LAND
rd_land_2022 %>% filter(south1_north0==1) %>%
  group_by(in_project) %>%
  summarise(
    n = n(),
    acre_drip  = mean(acre_drip , na.rm = TRUE),
    pct_drip_land = mean(pct_drip_land , na.rm = TRUE),
    acre_ir = mean(acre_ir , na.rm = TRUE),
    acre_cult = mean(acre_cult , na.rm = TRUE),
    land_holding = mean(land_holding, na.rm = TRUE)
  ) %>% 
  kable() %>% kable_minimal()


# Plots  .........................................................

# Plot | drip_use ................................................
pct_elevation_DI <- 
  rd_water_2022 %>% 
  filter(south1_north0==1) %>% 
  count(in_project,elevation,drip_use) %>% 
  group_by(in_project,elevation) %>% mutate(N=sum(n)) %>% 
  filter(drip_use==1) %>% rename(n_DI=n) %>% 
  mutate(pct_DI=n_DI/N) %>% ungroup() %>% 
  select( in_project, elevation, pct_DI )
  
pct_elevation_DI <- bind_rows( # add data of the entire project
  pct_elevation_DI,
    tibble(
      in_project = c(1, 0,1,0),
      elevation = c(8,8,9,9), # Garbage Vars for plot display
      pct_DI = c(0,0,0.31, 0.1)) )

pct_elevation_DI %>% # 700 X 250
  ggplot(aes(x = elevation, y = pct_DI, fill = factor(in_project))) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "", 
    values = c("0" = "gray70", "1" = "dodgerblue4"),
    labels = c("Out Project", "In Project") ) +
  scale_x_continuous( breaks = 0:9,
                      labels = c("519m", "< 522m", "< 525m", "< 528m", 
                                 "< 531m", "< 534m", "< 537m", "< 540m",
                                 "" ,"Project") ) +
  theme_minimal(base_family = "serif")+
  labs(title =  "Share of farmers in Elevation Bin", # | Southern Boundary" 
       x = "Elevation",y = "% of Households"
  ) +theme_minimal(base_family = "serif")


# Plot | drip_use ................................................

rd_land_2022 %>% 
  # filter(south1_north0==1) %>%
  group_by(in_project) %>%
  summarise(acre_drip=mean(acre_drip),pct_drip = mean(pct_drip_land)*100) 

elv_land <- rd_land_2022 %>% 
  # filter(south1_north0==1) %>%
  group_by(in_project,elevation) %>%
  summarise(acre_drip=mean(acre_drip),pct_drip = mean(pct_drip_land)) %>% ungroup()

# acre_drip
elv_land %>% # 500 X 200
  ggplot(aes(x = elevation, y = acre_drip, fill = factor(in_project))) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "", values = c("0" = "gray70", "1" = "dodgerblue4"),labels = c("Out Project", "In Project") ) +
  scale_x_continuous( breaks = 0:9,labels = c("519m", "< 522m", "< 525m", "< 528m", "< 531m", "< 534m", "< 537m", "< 540m","" ,"Project") ) +
  theme_minimal(base_family = "serif")+theme(legend.position="none")+
  labs(title =  "Land Under Drip-Irrigatin in Elevation Bin", # | Southern Boundary", 
       x = "Elevation",y = "Acre")+
  expand_limits(y=c(0, 0.72))


# pct_drip
elv_land %>% # 500 X 200
  ggplot(aes(x = elevation, y = pct_drip, fill = factor(in_project))) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "", values = c("0" = "gray70", "1" = "dodgerblue4"),labels = c("Out Project", "In Project") ) +
  scale_x_continuous( breaks = 0:9,labels = c("519m", "< 522m", "< 525m", "< 528m", "< 531m", "< 534m", "< 537m", "< 540m","" ,"Project") ) +
  theme_minimal(base_family = "serif")+theme(legend.position="none")+
  labs(title = "Percent of Drip-Irrigated Land in Elevation Bin", # | Southern Boundary", 
       x = "Elevation",y = "")+
  scale_y_continuous(limits = c(0, 0.07),
                     labels = scales::percent_format(accuracy = 1) )






lm_rd <- lm(acre_drip ~ in_project + elevation, 
            data = rd_land_2022 # %>% filter(abs(elevation) <= 2)
)

model_di <- 
  lm(drip_use ~ in_project + elevation, 
     data = rd_land_with_coords)
summary(model_di)

sjPlot::tab_model(model_di ,  show.se = T,digits = 3,     show.stat  = TRUE )





# summary stats for obs with coords ------------------------------




# plot ----
library(dplyr)
library(ggplot2)

rd_water_with_coords %>% 
  st_drop_geometry() %>%
  filter(rd_distance >= -1500, rd_distance < 1500) %>%
  mutate(
    distance_bin = cut(rd_distance, 
                       breaks = seq(-1500, 1500, by = 100), 
                       include.lowest = TRUE, 
                       right = FALSE),
    distance_label = as.numeric(gsub(".*,", "", gsub("\\)", "", distance_bin)))  # get upper bin value
  ) %>%
  select(hh_id, in_project, distance_label) %>%
  count(in_project, distance_label) %>%
  group_by(in_project) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = distance_label, y = percent, fill = factor(in_project))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    name = "Project Status",
    values = c("0" = "gray60", "1" = "dodgerblue4"),
    labels = c("Outside Project", "Inside Project")
  ) +
  scale_x_continuous(breaks = seq(-1500, 1500, by = 500)) +
  labs(
    title = "Farmer Distribution by Distance to Project Boundary",
    subtitle = "Share of farmers within each 100m bin, by project status",
    x = "Distance to Boundary (meters)",
    y = "Share of Farmers (%)"
  ) +
  theme_minimal(base_family = "serif")


# 1. BAR PLOT: DI acres per farmer by distance to boundary  ----

quantile(rd_data$DI, probs = c(0.01, 0.99), na.rm = TRUE)

rd_land_with_coords %>%
  # filter(rd_distance<1500,rd_distance>(-1500)) %>%   filter(acre_drip < 5.97) %>%  # Keep zeros, drop top 1% outliers
  ggplot(aes(x = rd_distance, y = acre_drip)) +
  geom_col(fill = "lightblue3", width = 180, position = position_dodge(width = 200)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red3") +
  labs(
    title = "Drip Irrigation Area by Distance to Project Boundary",
    subtitle = "A majority of drip users (2016–2023) reported zero drip-irrigated acreage in 2023.",
    x = "Distance to Boundary (meters)",
    y = "DI Area (acres)"
  ) +
  theme_minimal(base_family = "serif")

# 2. HISTOGRAM: Count of farmers by distance to boundary ----
rd_water_with_coords %>% 
  filter(rd_distance<1500,rd_distance>(-1500)) %>% 
  ggplot(aes(x = rd_distance)) +
  geom_histogram(fill = "dodgerblue4", bins = 50) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red3") +
  labs(
    title = "Histogram of Distance to Project Boundary",
    x = "Distance to Boundary (meters)",
    y = "Number of Farmers"
  ) + theme_minimal(base_family = "serif")






# IRELEVAT RD library(rdrobust)----
names(rd_water_with_coords)
names(rd_land_with_coords)

### rd_water_with_coords ###
#   Sample: all HH | drip_use, ir_use, drip_installed
#
### rd_land_with_coords ###
# acre_drip, acre_ir, acre_cult(cultivated land) , land_holding (in acre), pct_drip_land(% of land uder drip)
#   Sample: drip_use == 1 # acre_drip, land_holding, pct_drip_land
#   Sample: ir_use  ==  1 # acre_ir, acre_cult

# Create a function to extract relevant values from rdstats object
extract_rd_results <- function(obj, variable_name) {
  data.frame(
    Variable      = variable_name,
    # N_Left        = obj$N[1],
    # N_Right       = obj$N[2],
    # Eff_N_Left    = obj$N_h[1],
    # Eff_N_Right   = obj$N_h[2],
    # Bandwidth     = round(obj$bws["h", "left"], 1),
    
    Coef_Conv     = round(obj$coef["Conventional", "Coeff"], 3),
    SE_Conv       = round(obj$se["Conventional", "Std. Err."], 3),
    Z_Conv        = round(obj$z["Conventional", "z"], 3),
    P_Conv        = round(obj$pv["Conventional", "P>|z|"], 3),
    CI_Low_Conv   = round(obj$ci["Conventional", "CI Lower"], 3),
    CI_High_Conv  = round(obj$ci["Conventional", "CI Upper"], 3),
    
    Z_Robust      = round(obj$z["Robust", "z"], 3),
    P_Robust      = round(obj$pv["Robust", "P>|z|"], 3),
    CI_Low_Rob    = round(obj$ci["Robust", "CI Lower"], 3),
    CI_High_Rob   = round(obj$ci["Robust", "CI Upper"], 3)
  )
}

### rd_water_with_coords ###
# Distance meter as running var  ................................
# Run RD with distance as running variable
rd_drip_dist <- rdrobust(y = rd_water_with_coords$drip_use,
                         x = rd_water_with_coords$rd_distance)

rd_ir_dist <- rdrobust(y = rd_water_with_coords$ir_use,
                       x = rd_water_with_coords$rd_distance)

rd_installed_dist <- rdrobust(y = rd_water_with_coords$drip_installed,
                              x = rd_water_with_coords$rd_distance)
summary(rd_drip_dist)
summary(rd_ir_dist)
summary(rd_installed_dist)

# Create a combined summary table
rd_table <- bind_rows(
  extract_rd_results(rd_drip_dist, "drip_use"),
  extract_rd_results(rd_ir_dist, "ir_use"),
  extract_rd_results(rd_installed_dist, "drip_installed")
)
library(knitr)
library(kableExtra)
rd_table %>% kbl(digits = 3) %>%kable_styling()

# PlotS hight on the line - wide -open 20 line in console,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
rdplot(rd_water_with_coords$drip_use, rd_water_with_coords$rd_distance,
       title = "Drip Usgers | Distance to Boundary",x.label="",y.label="Users",
       # x.lim = c(-1500, 1500)
       )

rdplot(rd_water_with_coords$ir_use, rd_water_with_coords$rd_distance,
       title="Irrigation Usgers | Distance to Boundary",x.label="",y.label="Users",
       # x.lim = c(-1500, 1500)
       )

rdplot(rd_water_with_coords$drip_installed, rd_water_with_coords$rd_distance,
       title="Drip Installation | Distance to Boundary",x.label="",y.label="Users",
       # x.lim = c(-1500, 1500)
       )

# ELEVATION as running var .................................
# Run RD with elevation as running variable
rd_drip_elev       <- rdrobust(rd_water_with_coords$drip_use,        rd_water_with_coords$elevation)
summary(rd_drip_elev)
rd_ir_elev         <- rdrobust(rd_water_with_coords$ir_use,          rd_water_with_coords$elevation)
rd_installed_elev  <- rdrobust(rd_water_with_coords$drip_installed,  rd_water_with_coords$elevation)

# Combined summary table
rd_table_elevation <- bind_rows(
  extract_rd_results(rd_drip_elev, "drip_use"),
  extract_rd_results(rd_ir_elev, "ir_use"),
  extract_rd_results(rd_installed_elev, "drip_installed")
)
library(knitr)
library(kableExtra)
rd_table_elevation %>% kbl(digits = 3) %>%kable_styling()

# PlotS ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
rdplot(rd_water_with_coords$drip_use, rd_water_with_coords$elevation,
       title = "Drip Usgers | Elevation",x.label="",y.label="Users")

rdplot(rd_water_with_coords$ir_use, rd_water_with_coords$elevation,
       title="Irrigation Usgers | Elevation",x.label="",y.label="Users")

rdplot(rd_water_with_coords$drip_installed, rd_water_with_coords$elevation,
       title="Drip Installation | Elevation",x.label="",y.label="Users")


### rd_land_with_coords ###
# acre_drip, acre_ir, acre_cult(cultivated land) , land_holding (in acre), pct_drip_land(% of land uder drip)
#   Sample: drip_use == 1 # acre_drip, land_holding, pct_drip_land
#   Sample: ir_use  ==  1 # acre_ir, acre_cult

# Distance meter as running var................................
rd_land_coords <- rd_land_with_coords %>% 
  filter(drip_use==1)
rd_acre_drip_dist <- rdrobust(y=rd_land_coords$acre_drip,x=rd_land_coords$rd_distance)
summary(rd_acre_drip_dist)
rd_acre_ir_dist <- rdrobust(y=rd_land_coords$acre_ir,x=rd_land_coords$rd_distance)
rd_acre_cult_dist <- rdrobust(y=rd_land_coords$acre_cult,x=rd_land_coords$rd_distance)
rd_land_holding_dist <- rdrobust(y=rd_land_coords$land_holding,x=rd_land_coords$rd_distance)
rd_pct_drip_land_dist <- rdrobust(y=rd_land_coords$pct_drip_land,x=rd_land_coords$rd_distance)

# Combine RD results for land outcomes
rd_table_land_dist <- bind_rows(
  extract_rd_results(rd_acre_drip_dist, "acre_drip"),
  extract_rd_results(rd_acre_ir_dist, "acre_ir"),
  extract_rd_results(rd_acre_cult_dist, "acre_cult"),
  extract_rd_results(rd_land_holding_dist, "land_holding"),
  extract_rd_results(rd_pct_drip_land_dist, "pct_drip_land")
)
library(knitr)
library(kableExtra)
rd_table_land_dist %>% kbl(digits = 3,) %>%kable_styling()

# ELEVATION as running var..............................
rd_land_coords <- rd_land_with_coords %>% 
  filter(drip_use==1)
rd_acre_drip_elev <- rdrobust(y=rd_land_coords$acre_drip,x=rd_land_coords$elevation)
summary(rd_acre_drip_elev)
rd_acre_ir_elev <- rdrobust(y=rd_land_coords$acre_ir,x=rd_land_coords$elevation)
rd_acre_cult_elev <- rdrobust(y=rd_land_coords$acre_cult,x=rd_land_coords$elevation)
rd_land_holding_elev <- rdrobust(y=rd_land_coords$land_holding,x=rd_land_coords$elevation)
rd_pct_drip_land_elev <- rdrobust(y=rd_land_coords$pct_drip_land,x=rd_land_coords$elevation)

# Combine RD results for land outcomes
rd_table_land_elev <- bind_rows(
  extract_rd_results(rd_acre_drip_elev, "acre_drip"),
  extract_rd_results(rd_acre_ir_elev, "acre_ir"),
  extract_rd_results(rd_acre_cult_elev, "acre_cult"),
  extract_rd_results(rd_land_holding_elev, "land_holding"),
  extract_rd_results(rd_pct_drip_land_elev, "pct_drip_land")
)
library(knitr)
library(kableExtra)
rd_table_land_elev %>% kbl(digits = 3,) %>%kable_styling()





