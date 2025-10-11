
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(sp)

ramthal_border <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/Ramthal Data to Dan/Project Map/ramthal border.shp")

library(maptools)
Am <- readShapePoly("~/master_research/DATAs/Ramthal Data to Dan/Project Map/Shape Files/Amaravathi.shp")

# fortify() and tidy() are same argument

CMF <- CMF_RAMTHAL_IRRIGATION_18_Aug_2016_cleaned_labeled %>% dplyr::select(1,6,7,"A17",matches("C2_"),matches("D4_"))




# work with spatial data; sp package will load with rgdal.
library(rgdal)
library(rgeos)
# for metadata/attributes- vectors or rasters
library(raster)

sjer_plot_locations<-readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Banihatti")


summary(banihatti@data)

# import KML file----
library(sf)
ccg <- st_read("~/master_research/DATAs/ramthal_data/Jain_feb_2020/RAMTHAL_PROJECT_COMMAND_MAP.kml")
plot(ccg[1])

# Division into regions (north, south, etc.) ----

south12 <- readOGR("~/master_research/DATAs/Ramthal Data to Dan/Project Map/New folder", "1 south-in")
north22 <- readOGR("~/master_research/DATAs/Ramthal Data to Dan/Project Map/New folder", "2 north-in")

row.names(south12)
row.names(north22)

south12 <- spChFIDs(south12,"1") # set polygon name 
row.names(south12)
SNWE <- rbind(north22,south12)
SNWE <- fortify(SNWE)

ggplot(SNWE,aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()


tot <- CMF_RAMTHAL_IRRIGATION_18_Aug_2016_cleaned_labeled[c(200,201,203,220,221),]




