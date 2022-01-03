library(tidyverse)
library(raster) # + sp + terra
shp <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/Ramthal Data to Dan/Project Map/ramthal border.shp")

library(rgdal)
shp<-readORG(dsn="C:/Users/Dan/Documents/master_research/DATAs/Ramthal Data to Dan/Project Map/ramthal border.shp", layer = "ramthal border")

library(sf)

"C:/Users/Dan/Documents/master_research/DATAs/Ramthal Data to Dan/Project Map/ramthal border.shp"

plot(shp)
library(ggplot2)

df=fortify(shp)

ggplot()+
  geom_polygon(data=df,aes(x=long,y=lat,group=group),
               fill="green",color="black",lwd=0.25)+
  theme_light()


