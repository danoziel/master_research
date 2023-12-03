
# ramthal_east: SpatialPolygonsDataFrame ----


list_plots= # list plot_survey_1-10
  a_rmtl_srvy22 %>% 
  select(id,plot_1,plot_survey_1, 19,22, 29,32 ,39,42 ,49,52, 59,62, 69,72, 79,82,
         plot_9,plot_survey_9, plot_10, plot_survey_10)



library(tidyverse)
library(rgdal)

ramthal_east <- 
  readOGR("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Ramthal_East")
rmtl_gis = ramthal_east

glimpse(rmtl_gis)

plot(rmtl_gis)

#rename "shp_code" = "id"
rmtl_gis@data <- rename(rmtl_gis@data, shp_code = id)
rmtl_gis_data=rmtl_gis@data

# add id 0 1 2 3 ...
rmtl_gis@data$id <- rownames(rmtl_gis@data)

# tidy / fortify 
rmtl_gis_fortify <- fortify(rmtl_gis, region = "id")

# merge the new id to the fortifydata
rmtl_gis_df <- merge(rmtl_gis_fortify, rmtl_gis@data )

names(rmtl_gis@data)
names(ramthal_east@data)

## add new datasets ----

# Percentage of water users per #survey ----
a_plots_geo%>% 
  mutate(water_usage=ifelse(mm5==1,"use water","didnt use water"))

### map A group vars ----
rmtl_gis_df2 <-left_join(a_plots_geo, rmtl_gis@data, by = "shp_code", relationship = "many-to-many")
rmtl_mapA <- left_join(rmtl_gis_df,rmtl_gis_df2, relationship = "many-to-many")
rm(ramthal_east)
# ggplot map
library(ggplot2)
library(dplyr)
# in1_out0  ----

rmtl_mapA %>% 
  mutate(in1_out0=as.character(in1_out0)) %>%
  mutate( in1_out0 = ifelse(is.na(in1_out0), "Not in sample",
                            ifelse(in1_out0 == 1, "In Ramthal", "Not in Ramthal"))) %>%
  
  ggplot(aes(x = long, y = lat, group = group, fill = in1_out0)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "Farmers in/out Ramthal 2016") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank())+
  scale_fill_manual(values = c( "#56B4E9", "black","white"),
                    labels = c("In Ramthal","Not in Ramthal", "Not in sample 2016"))

# map_water_usage ----

rmtl_mapA %>%
  mutate(water_usage = ifelse(is.na(mm5), "NA", ifelse(mm5 == 1, "use water", "didn't use water"))) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = water_usage)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "Water use by farmers 2016-2022") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank()
  ) +
  scale_fill_manual(
    values = c("use water" = "lightblue", "didn't use water" = "#FFA500", "NA" = "white"),
    breaks = c("use water", "didn't use water","NA"),
    labels = c("Use water", "Didnt use water","Not in sample 2016")
  )








#scale_fill_brewer("Utah Ecoregion")
  #scale_fill_viridis_d()


# villages  ----
library("viridis")
rmtl_mapA %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = village)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "villages Ramthal") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank(),
    legend.position = "non")+
  scale_fill_viridis(discrete = TRUE)










# villages layer GIS  ----
library("viridis")
rmtl_mapA %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = layer)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "villages layer") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank(),
    legend.position = "non")+
  scale_fill_viridis(discrete = TRUE)











# villages layer  ----
library("viridis")
rmtl_mapA %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = layer.x)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "villages layer") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank())+
  scale_fill_viridis(discrete = TRUE)












# newfarmer mm2_1  ----
library("viridis")
rmtl_mapA %>% 
  mutate(newfarmer= ifelse(in1_out0==0 & mm2_1 == 1, "ramtal 2022","else")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = newfarmer)) +
  geom_polygon(color = "#999999", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "Not ramthal farmers 2016 - said Ramhtal 2022") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank())+
  scale_fill_manual(values = c( "lightblue3",  "black"))

### map B:  WU=a_water_usage_key_var ----
water_usage_key_var_gis <- a_water_usage_key_var %>% 
  left_join(list_shape_code) %>% 
  rename(id_srvy=id,layer_elevation=layer) %>%
  mutate(id_srvy=as.character(id_srvy),
         shp_code=as.character(shp_code),
         survey = as.character (survey))

rmtl_gis_WU <-left_join(water_usage_key_var_gis, rmtl_gis_data, by = "shp_code", relationship = "many-to-many")
gis_WU_map <- left_join(rmtl_gis_df,rmtl_gis_WU, relationship = "many-to-many")

rm(ramthal_east)
rm(water_usage_key_var_gis,rmtl_gis_WU)
names(gis_WU_map)

# ggplot map
library(ggplot2)
library(dplyr)

# irri.rain ----
gis_WU_map %>% 
  mutate(water_source = ifelse(irri.rain == "rainfed","Rain only", "Rain & Irrigation")) %>% 
  mutate(water_source = ifelse(is.na(water_source), "Not in sample 2016",water_source)) %>% 
  
  ggplot(aes(x = long, y = lat, group = group, fill = water_source)) +
  geom_polygon(color = "gray70", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "water_source 2016-2022") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank()
  ) +
  scale_fill_manual(
    values = c("Rain only" = "lightblue", "Rain & Irrigation" = "royalblue1", "Not in sample 2016" = "white"),
    breaks = c("Rain only", "Rain & Irrigation","Not in sample 2016")  )






# used Kharif Rabi Both ---- TO FIX ----
what_season=
  a_water_usage_key_var %>% select(id,mw2) %>%
  left_join(list_shape_code) %>%   rename(id_srvy=id) %>% 
  mutate(shp_code= as.character(shp_code))

  

what_season_gis_WU <-left_join(what_season, rmtl_gis_data, by = "shp_code", relationship = "many-to-many")
what_season_gis_WU_map <- left_join(rmtl_gis_df,what_season_gis_WU, relationship = "many-to-many")


what_season_gis_WU_map %>% 
  mutate(
         season_used = ifelse(mw2 == 1,"Kharif",
                              ifelse(mw2 == 2,"Rabi","Both"))) %>% 


  ggplot(aes(x = long, y = lat, group = group, fill = season_used)) +
  geom_polygon(color = "gray70", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "in what season you still making use of the water?") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank()
  ) +
  scale_fill_manual(
    values = c("Kharif" = "red4","Both" = "black", "Rabi" = "royalblue4"),
    breaks = c("Kharif", "Rabi", "Both")  )



 #  mm9 ---- 
gis_WU_map %>% 
  mutate(
    lc_on_pipe = 
      case_when(mm9>=0 & mm9 < 6   ~ "0-5",
                mm9>= 6 & mm9 <11   ~ "5-10",
                mm9>= 11 & mm9 <21   ~ "10-20",
                mm9>= 21 & mm9 <31   ~ "20-30",
                mm9>= 31 & mm9 <51   ~ "30-50",
                mm9 == -999   ~ "no answer",
                TRUE ~ NA)) %>%
  mutate(lc_on_pipe = ifelse(is.na(lc_on_pipe), "Not in sample 2016",lc_on_pipe)) %>% 
  
  
  ggplot(aes(x = long, y = lat, group = group, fill = lc_on_pipe)) +
  geom_polygon(color = "gray70", linewidth = 0.1)+ #linewidth = size
  coord_equal() +
  theme_void() +
  labs(title = "Location on pipeline") +
  theme(
    plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
    legend.text = element_text(family = "serif",size =12),
    legend.title = element_blank()) +
  scale_fill_manual(
    values =c("0-5" = "gray10" ,"5-10" = "orchid4","10-20" = "orchid3","20-30" = "orchid2","30-50" = "orchid1",
                 "no answer" = "grey70", "Not in sample 2016" = "white"),
    breaks = c("0-5","5-10","10-20","20-30","30-50","no answer","Not in sample 2016"))




#  mw4 ----
gis_WU_map %>% 
    mutate(mw4=as.numeric(mw4),
           still_using = ifelse(mw4 == 1, "still using",
           ifelse(mw4 == 2, "Not using","Sometimes"))) %>%
  mutate(still_using = ifelse(is.na(still_using), "Not in sample 2016",still_using)) %>% 
  

        ggplot(aes(x = long, y = lat, group = group, fill = still_using)) +
    geom_polygon(color = "gray70", linewidth = 0.1)+ #linewidth = size
    coord_equal() +
    theme_void() +
    labs(title = "mw4") +
    theme(
      plot.title = element_text(family = "serif", margin = margin(t = 5, b = -5),size =15),
      legend.text = element_text(family = "serif",size =12),
      legend.title = element_blank()) +
    scale_fill_manual(
    values = c("still using" = "royalblue4" , "Sometimes" = "royalblue1","Not using"= "lightblue" , "Not in sample 2016" = "white"),
    breaks = c("still using", "Sometimes","Not using","Not in sample 2016")  )
# mw1c ----

mw1c=a_rmtl_srvy22 %>% select(id,mm5,mw2,starts_with("mw1c")) 

library(tidyr)
mw1c%>% summarise_at(c("mw1c_1","mw1c_2","mw1c_3","mw1c_4","mw1c_5",
                       "mw1c_6","mw1c_7","mw1c_8","mw1c_9","mw1c_10"
                       ), sum, na.rm = TRUE) %>% 
  pivot_longer(mw1c_1 :mw1c_10,names_to = "ans", values_to = "n") %>% 
  mutate(pct=n/sum(n)) %>%  arrange(n)




# _gis tutorial____________-----


ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))

ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))+

stream <- data.frame(
  x = cumsum(runif(50, max = 0.1)),
  y = cumsum(runif(50,max = 0.1))
)

ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))+
  geom_line(data = stream, colour = "red", size = 1)












#colorblind-friendly palette ----

# The palette with grey:
cbPalette <- 
  c("#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- 
  c("#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)