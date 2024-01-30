"C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp"

\





# Instruction ----

https://www.youtube.com/watch?v=h29g21z0a68

https://www.youtube.com/watch?v=NQZNpyEgVss

https://www.youtube.com/watch?v=f26U2kwAWkQ

https://rstudio-pubs-static.s3.amazonaws.com/245582_310956f40b364b1a95dff2d2041cae9b.html


# NYC ----
https://medium.com/@honggao/plot-polygons-with-ggplot2-b5a87e4fa602

library(ggplot2)
library(RColorBrewer)
library(rgdal)

# read in the shapefile and name it zipcode
zipcode <- readOGR("C:/Users/Dan/Documents/master_research/DATAs/ZIP_CODE_NYC", "ZIP_CODE_040114")

zipcode.data <- zipcode@data 

# generate a unique ID for each polygon
zipcode@data$seq_id <- seq(1:nrow(zipcode@data))

plot(zipcode)


# generate random numbers from a uniform distribution ----
zipcode@data$continuous_var <- runif(nrow(zipcode@data))
# create some missing data, NAs
# values below 0.1 are changed to NA
zipcode@data[zipcode@data$continuous_var<0.1,]$continuous_var <- NA

#We bin the continuous_var to three categories with its 33th and 66th percentile 
#and call it categorical_var. Missing data (NA) is preserved in the new column.

zipcode@data$categorical_var <- 
  .bincode(zipcode@data$continuous_var, 
           breaks = quantile(zipcode@data$continuous_var, seq(0,1,0.33), 
                             na.rm = T), include.lowest = T)

## Plot Polygons ----

# in order to plot polygons, first fortify the data
zipcode@data$id <- rownames(zipcode@data)

library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# create a data.frame from our spatial object
zipcodedata <- fortify(zipcode, region = "id")

# merge the "fortified" data with the data from our spatial object
zipcode.df <- merge(zipcodedata, zipcode@data,by = "id")

# plot ----
ggplot(data = zipcode.df, aes(x = long, y = lat, group = group, fill = continuous_var)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  scale_fill_gradient(breaks=c(0.33,0.66,0.99), labels=c("Low","Medium","High")) + 
  coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Attrition Rate by New York City Zip Code")

display.brewer.pal(6, "Blues")
brewer.pal(6, "Blues")

ggplot(data = zipcode.df, aes(x=long, y=lat, group = group, fill=addNA(factor(categorical_var)))) +
  geom_polygon()  +
  geom_path(color = "white", size = 0.2) +
  scale_fill_manual(
    values=c("#08306B", "#4292C6","#C6DBEF","#666666"), 
    breaks= c("3","2","1","NA"), 
    labels= c("High","Medium","Low","NA"))+
  coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), 
        axis.text = element_blank()) +
  labs(title = "Attrition Rate by New York City Zip Code", 
       fill = "Attrition Rate")

# Scotland ----
https://www.marmoe.xyz/2018/09/04/shapefiles-in-r-with-ggplot2-rgdal/
  
library(tidyverse)
library(rgdal)

NHSBoards <- readOGR(dsn = "C:/Users/Dan/Documents/master_research/DATAs/map_of_scotland", "SG_NHS_HealthBoards_2019")
glimpse(NHSBoards)

plot(NHSBoards)
NHSBoards.data <- NHSBoards@data

#A First Simple Map

library(broom)
NHSBoards_tidy <- tidy(NHSBoards)

#Adding Data Attributes
NHSBoards$id <- row.names(NHSBoards)
NHSBoards_tidy2 <- left_join(NHSBoards_tidy, NHSBoards@data)


hospitalsSco <- data.frame(HBName = sort(NHSBoards@data$HBName),
                           Hospitals = c(16,15,23,12,8,34,45,28,20,34,1,1,32,3))

NHSBoards_tidy3 <- left_join(NHSBoards_tidy2, hospitalsSco)

ggplot(NHSBoards_tidy3, aes(x = long, y = lat, group = group, fill = Hospitals)) +
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  labs(title = "Hospital Density in Scotland (2019)") +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))

#Adding Another Layer 
HBLabel <- NHSBoards_tidy %>%
  group_by(HBName) %>%
  summarise(label_long = mean(range(long)), label_lat = mean(range(lat)), Hospitals = mean(Hospitals))

map <- ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = Hospitals)) +
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  labs(title = "Hospital Density in Scotland (2019)") +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))

map +
  geom_text(data = HBLabel, mapping = aes(x = label_long, y = label_lat, label = Hospitals, group = NA)
            , cex = 4, col = "white")



HBLabel <- NHSBoards_tidy %>%
  group_by(HBName) %>%
  summarise(label_long = mean(range(long)), label_lat = mean(range(lat)), Hospitals = mean(Hospitals)) %>%
  mutate(LabelOutsideBoundaries = HBName %in% c("Orkney", "Shetland", "Western Isles"),
         label_long = replace(label_long, HBName %in% c("Ayrshire and Arran", "Fife", "Forth Valley", "Highland", "Orkney", "Shetland", "Western Isles"), 
                              c(245000, 340000, 260000, 250000, 375000, 400000, 75000)),
         label_lat = replace(label_lat, HBName %in% c("Fife", "Forth Valley", "Highland", "Orkney", "Shetland"), 
                             c(710000, 700000, 810000, 1000000, 1175000)))

map +
  geom_text(data = HBLabel, mapping = aes(x = label_long, y = label_lat, label = Hospitals, group = NA, col = LabelOutsideBoundaries)
            , cex = 4, show.legend = FALSE) +
  scale_color_manual(values = c("white", "black"))








"C:/Users/Dan/Documents/master_research/DATAs/Ramthal Data to Dan/Project Map/ramthal border.shp"
DATAs/ramthal_data/project_map/vlgs
# bind SHP - another way ----

file1 <-sp::spChFIDs(file1,"1") # set polygon name 
row.names(file1)
peninsula <- rbind(file1,file2,file3)
mapa_peninsula <- fortify(peninsula)

ggplot(ab,aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()
