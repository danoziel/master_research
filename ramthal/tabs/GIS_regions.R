library(tidyverse)
library(sp)
library(raster)
library(sf)
library(rgdal)
library(readxl)
library(dplyr)

# DWG file ----
# Specify the file path to the DWG file
file_path <- "~/master_research/DATAs/ramthal_data/project_map/Ramthal Command Map- East SIde 12300 Ha.dwg"

# Read the DWG file
dwg <- read.dwg(file_path)


ramthal_border

# Division into regions (north, south, etc.) ----

south12 <- readOGR("~/master_research/DATAs/ramthal_data/project_map/New folder","1 south-in")
north22 <- readOGR("~/master_research/DATAs/ramthal_data/project_map/New folder", "2 north-in")

row.names(south12)
row.names(north22)

south12 <- spChFIDs(south12,"1") # set polygon name 
row.names(south12)
SNWE <- rbind(north22,south12)
SNWE <- fortify(SNWE)

ggplot(SNWE,aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()
