"C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp"

# exsample ----
# https://mapping-in-r-workshop.ryanpeek.org/01_vector_shapefiles

# notice the simple structure, but results in dataframe
hucs_sf <- st_read("C:/Users/Dan/Downloads/h8_tahoe.shp")

# check crs
st_crs(hucs_sf)

library(urbnmapr)

# Pick a State
state_names <- c("CA")

# warning is ok
CA <- get_urbn_map(map = "states", sf = TRUE) %>% filter(state_abbv==state_names)
st_crs(CA)

library(purrr)

# Pick some CA counties
co_names <- c("Butte County", "Placer County", 
              "El Dorado County", "Nevada County", 
              "Yuba County", "Sierra County",
              "Plumas County")

# get counties
counties_spec <- get_urbn_map(map = "counties", sf=TRUE) %>% 
  filter(state_abbv==state_names, county_name %in% co_names)


# add centroid for each county using purrr::map_dbl
counties_spec <- counties_spec %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))


#Make a Basic Map
plot(st_geometry(hucs_sf), col="blue2", 
     main="Tahoe HUC8", axes=TRUE)

# this is same
# plot(hucs_sf$geometry), col="darkblue", main="Tahoe HUC8")


# Advanced Mapping
# note adding some alpha adjustment here and using "cex" to make larger
plot(counties_spec$geometry, col=adjustcolor("maroon", alpha=0.5), cex=1.5, axes=TRUE, main="Selected CA Counties")

# now add tahoe HUC8 to existing plot, with add=TRUE
plot(hucs_sf$geometry, col=adjustcolor("blue", alpha=0.7), add=TRUE)

# now add some labels using the centroid lat/long we added earlier
text(counties_spec$lon, counties_spec$lat, labels = counties_spec$name)

# get range of lat/longs from counties for mapping
mapRange1 <- st_bbox(counties_spec) # view bounding box

# add counties
plot(counties_spec$geometry, col=adjustcolor("maroon", alpha=0.5), cex=1.5,
     xlim=mapRange1[c(1,3)], ylim = mapRange1[c(2,4)], axes=TRUE, 
     main="Selected CA Counties and Tahoe HUC8")

# add HUC
plot(hucs_sf$geometry, col=adjustcolor("blue", alpha=0.7), add=TRUE)

# add labels for counties
text(counties_spec$lon, counties_spec$lat, labels = counties_spec$name,
     col=adjustcolor("white", alpha=0.8), font = 2)

# need to make sure CRS is the same!
st_crs(hucs_sf)==st_crs(counties_spec)

counties_spec <- st_transform(counties_spec, st_crs(hucs_sf))

# crop watershed by counties
hucs_sf_crop <- st_intersection(hucs_sf, counties_spec) # warning is ok

# plot
plot(hucs_sf_crop$geometry, col="blue") # a quick plot check to make sure it worked

# buffer a single county? Warning is ok as well...has to do with lat/lon
county_buff <- st_buffer(counties_spec[counties_spec$name=="El Dorado",], dist = .05) # note this is a buffer of decimal degrees

# now plot
plot(counties_spec$geometry, axes=TRUE)
plot(county_buff$geometry, border="maroon", col=adjustcolor("maroon", alpha=0.5), add=TRUE)
plot(hucs_sf_crop$geometry, col="blue", add=TRUE)

# try again, let's switch layer ordering
plot(counties_spec$geometry, col=adjustcolor("maroon", alpha=0.2), cex=1.5, axes=TRUE, main="Selected CA Counties")
plot(hucs_sf_crop$geometry, col=adjustcolor("blue", alpha=0.7),add=TRUE)
plot(CA$geometry, add=TRUE, lwd=2)
text(counties_spec$lon, counties_spec$lat, labels = counties_spec$name,
     col="maroon", font = 2)

library(ggplot2)
library(ggrepel)

# quick test of just CA
ggplot() + geom_sf(data = CA)

# not cropped
ggplot() + 
  geom_sf(data=CA, color = "gray30", lwd=2, fill=NA) + # California border
  geom_sf(data=counties_spec, fill = NA, show.legend = FALSE, color="gray50", lwd=0.4) + # counties
  geom_label_repel(data=counties_spec, aes(x=lon, y=lat, label=county_name)) +
  theme_bw()

# with cropped range (to only our selected counties)
ggplot() + 
  geom_sf(data=CA, color = "gray30", lwd=2, fill=NA) +
  geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4) +
  geom_sf(data=hucs_sf_crop, fill="blue", alpha=0.5, size=0.5)+
  geom_label_repel(data=counties_spec, aes(x=lon, y=lat, label=county_name)) +
  coord_sf(xlim = mapRange1[c(1,3)], ylim = mapRange1[c(2,4)]) +
  theme_bw(base_family = "Helvetica") + # change to "sans" if this font not available
  labs(title="Selected CA Counties and Tahoe HUC8")+
  theme(panel.grid.major = element_line(color = "gray80", linetype = 1)) + # change grid
  annotate("text", x=c(-120.1,-119.9), y=40.3, label=c("CA","NV"), color="gray40", size=3, fontface=2, angle = 90)













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
