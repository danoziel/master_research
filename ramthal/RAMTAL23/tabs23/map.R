
uninstall

# library(raster)
# shp <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

library(sf)
map_file <- read_sf("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages","Ramthal_East")
bd_data0  <- read_sf("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

mp_rmtl <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

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
plot(st_geometry(hucs_sf), col="darkblue", 
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

# THE END ----

#check the imports worked
head(map_rmtl)
class(rmtl_gis)
attr(bd_data0, "sf_column")

nc.no_sf <- as.data.frame(bd_data0)
class(nc.no_sf)

(nc_geom <- st_geometry(bd_data0)) # class = "sfc_POLYGON" "sfc" IS IT FORTIFY????????
nc_geom[[1]]



crop=a_plots_crop %>% filter(plotID=="plot_01", season=="kha") %>%dplyr:: select(hh_id,crop_common)
nc= bd_data0 %>% left_join(list_shape_code) %>%left_join (crop )

nc %>% ggplot() + geom_sf(aes(fill = crop_common ))




rmtl_gis_df <- st_as_sf(map_rmtl) %>% st_set_geometry(NULL) %>% as.data.frame()


list_shape_code=list_shape_code %>% rename(hh_id=id,id=shp_code)

rmtl_gis_df2 <-left_join(a_irri_rain_method, list_shape_code, by = "hh_id")

rmtl_mapA <- left_join(rmtl_gis,rmtl_gis_df2, relationship = "many-to-many")

mp=ggplot() + geom_sf(data = rmtl_mapA , aes(fill = irri_method ))








library(sf)
sf_use_s2()
fname <- system.file("shape/Ramthal_East.shp", package="sf")

nc = st_read(system.file("shape/Ramthal_East.shp", package="sf"))

nc <- st_read(rmtl_gis)
rmtl_gis


st_read(rmtl_gis, stringsAsFactors = FALSE)













