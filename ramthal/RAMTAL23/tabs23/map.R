
# DF use in this script
rmtl_InOut
cultivated_land_2022

Ramthal_East_shp # 7274 obs sf         # original file # 
Ramthal_clean    # 3548 obs sf         # cols: id,geometry=POLYGON [m]
centroids_coords # 3548 obs data.frame # cols: X, Y, id

multi_hh_df          # hh_id X Y       #  1,489 obs
rd_water_with_coords # sf  Water usage #  1,409 obs
rd_land_with_coords  # sf  Land usage  #  1,378 obs



# Clean  the Ramthal_East_shp shapefile [Ramthal_clean]    ----
library(sf)

sf::sf_use_s2(FALSE)  # Disable S2 to avoid geometry engine issues

# Adjust this path if your file is stored elsewhere
Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

dev.new()  # opens a new plotting window
plot(st_geometry(Ramthal_East_shp))

st_crs(Ramthal_East_shp)
# EPSG:4326 →  lat/lon
# EPSG:32643 →  UTM (meters)
# Ramthal_East_shp - mismach - EPSG:4326 AND  UTM (meters)
# Ramthal_East_shp has mismach of Bounding box UTM  and Geodetic CRS:WGS 84


# STEP 1: Fix geometries with st_buffer(0)
Ramthal_fixed <- Ramthal_East_shp %>%
  mutate(geometry = st_buffer(geometry, 0))
# IGNOR THE WARNING


# STEP 2: Remove broken, empty, or NA-boundary geometries
Ramthal_fixed_clean <- Ramthal_fixed %>%
  filter(!st_is_empty(geometry), st_is_valid(geometry)) %>%
  filter(!sapply(st_geometry(.), function(g) any(is.na(st_bbox(g)))))


# STEP 3: Group polygons, union them
Ramthal_clean <- Ramthal_fixed_clean %>%
  group_by(id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
# IGNOR THE WARNING AND THE ENDLESS LIST


# STEP 4: Reassign CRS to UTM
st_crs(Ramthal_clean) <- 32643  # UTM zone 43N — coordinates are in meters





# Convert POLYGON to POINT  [centroids_coords]             ----
# Create centroids for each polygon (in meters)

Ramthal_centroids_utm <- 
  Ramthal_clean %>% mutate(geometry = st_centroid(geometry))

# Extract X/Y coordinates from the centroids
coords <- st_coordinates(Ramthal_centroids_utm)

# Combine coordinates with polygon id
centroids_coords <- as.data.frame(coords)
centroids_coords$id <- Ramthal_centroids_utm$id


# Allocate more coordinates  [multi_hh_df]                 ----

# Adjust polygons with 2-3 farm
# Adjust farmer coordinates to offset multiple households per polygon 
# (25m step in X and Y)

multi_hh_df <- list_shape_code %>%
  select(hh_id, id) %>%
  left_join(centroids_coords, by = "id") %>%
  filter(!is.na(X)) %>%  # remove hh_id and id without coords
  group_by(id) %>%
  mutate(
    hh_count     = n(),                     # number of households per polygon
    offset_index = row_number(),            # row index within group
    offset_m     = (offset_index - 1) * 25, # 25 meters per additional farmer
    adj_X        = X + offset_m,            # shift east
    adj_Y        = Y + offset_m             # shift north
  ) %>%
  ungroup() %>% 
  select(hh_id ,id ,adj_X ,adj_Y) %>%  
  rename(X= adj_X ,Y= adj_Y)




# df to Sothe boundary   [south_edge_sf]                   =====================================
library(sf)

south1_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/1 south-in.shp")
north2_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/2 north-in.shp")

plot(st_geometry(south1_in ),main = "Project's South Boundary")
dev.new()
plot(st_geometry(south1_in))

#|==============================
#       Southern Border
#|==============================

st_crs(south1_in)
south1_utm <- st_transform(south1_in, crs = 32643)
st_crs(south1_utm)

# Step 1: Extract boundary coordinates from polygon
boundary_coords <- st_coordinates(south1_utm)

# Step 2: Visualize coordinates to identify problematic points
plot(boundary_coords[, "X"], boundary_coords[, "Y"],
     type = "l", col = "gray", main = "Polygon Coordinates")
points(boundary_coords[, "X"], boundary_coords[, "Y"], col = "yellow", pch = 19)
text(boundary_coords[, "X"], boundary_coords[, "Y"],
     labels = seq_len(nrow(boundary_coords)), cex = 0.5, pos = 3)


# Step 3: Remove bad points (retain only the clean southern segment)
#             Manual selection to remove points 2-75 from step 2 plot
boundary_coords_clean <- boundary_coords[76:nrow(boundary_coords), ]

# Step 4: Create a LINESTRING from cleaned coordinates
southern_edge_line <- st_linestring(as.matrix(boundary_coords_clean[, c("X", "Y")]))

# Step 5: Wrap into an sf object using the original CRS (UTM zone 43N)
southern_edge_sf <- st_sfc(southern_edge_line, crs = st_crs(south1_utm))

#️ Step 6: Plot the final cleaned southern edge
plot(st_geometry(southern_edge_sf), col = "red3", lwd = 2,
     main = "Southern Border")

# Step 7 😄:  Ensure centroids are in sf format
centroids_sf <- st_as_sf(centroids_coords, coords = c("X", "Y"), crs = 32643)
library(sf)

# Read the shapefile back into R
centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)




#|==============================
# Completion to Southern Border 
#|==============================

st_crs(north2_utm)
north2_utm <- st_transform(north2_in, crs = 32643)
st_crs(north2_utm)

boundary_north2_coords <- st_coordinates(north2_utm)
plot(boundary_north2_coords[, "X"], boundary_north2_coords[, "Y"],
     type = "l", col = "gray", main = "Polygon Coordinates")
points(boundary_north2_coords[, "X"], boundary_north2_coords[, "Y"], col = "orange", pch = 19)
text(boundary_coords[, "X"], boundary_north2_coords[, "Y"],
     labels = seq_len(nrow(boundary_north2_coords)), cex = 0.5, pos = 3)

boundary_north2_clean <- boundary_north2_coords[174:nrow(boundary_north2_coords), ]
north2_edge_line <- st_linestring(as.matrix(boundary_north2_clean[, c("X", "Y")]))
north2_edge_sf <- st_sfc(north2_edge_line, crs = st_crs(north2_utm))
plot(st_geometry(north2_edge_sf), col = "red3", lwd = 2)




# # Final - Calculate shortest distance to the boundary
# centroids_coords$dist_to_boundary <- st_distance(
#   centroids_sf,
#   southern_edge_sf
# )

#|======================================================================
#  combine [southern_edge_sf] & [north2_edge_sf] into a single LINESTRING
#|======================================================================

# Step 1: Extract coordinates as separate matrices
coords_south <- st_coordinates(southern_edge_sf)[, c("X", "Y")]
coords_north <- st_coordinates(north2_edge_sf)[, c("X", "Y")]

# Step 2: Wrap both in a list
edge_segments <- list(coords_south, coords_north)

# Step 3: Create MULTILINESTRING
combined_multiline <- st_multilinestring(edge_segments)
# south_edge <- 

# Step 4: Convert to sf object with same CRS
combined_edge_sf <- st_sfc(combined_multiline, crs = st_crs(southern_edge_sf))
# south_edge_sf <- 

# Plot result
plot(combined_edge_sf, col = "red3", lwd = 2, main = "Combined Project Edge (Unconnected)")

rm(south1_in,south1_utm,boundary_coords,boundary_coords_clean, 
   southern_edge_line,southern_edge_sf )
rm(north2_in,north2_utm, boundary_north2_coords, boundary_north2_clean,
   north2_edge_line,north2_edge_sf)
rm(coords_south, coords_north, edge_segments)

# df to Ramthal boundary [ramthal_border_utm]              =====================================
library(sf)

ramthal_border <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/ramthal_border.shp")

dev.new()
plot(st_geometry(ramthal_border ),main = "Project's Boundary")

st_crs(ramthal_border)

dev.new()  # opens a new plotting window
plot(st_geometry(ramthal_border))
st_crs(ramthal_border)

library(sf)

# 1. Fix the ramthal_border (project boundary lines)
# Drop Z dimension (from LINESTRING Z to LINESTRING)
ramthal_border_utm <- st_zm(ramthal_border, drop = TRUE)

# The original file had wrong CRS label (WGS84), but it's actually in UTM meters
# So: we assign (not transform) the correct UTM CRS — EPSG:32643
st_crs(ramthal_border_utm) <- 32643


# 2. Convert centroids_coords to spatial points
# Convert to sf object with correct CRS
centroids_sf <- st_as_sf(
  centroids_coords,
  coords = c("X", "Y"),
  crs = 32643)  # UTM zone 43N (meters)

ggplot() +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  geom_sf(data = centroids_sf, color = "firebrick4", size = 1.5, alpha = 0.6) +
  labs(
    title = "Ramthal Project Boundaries and Farmer Centroids",
    subtitle = "Black lines: Project zone boundaries | Blue dots: Farmer locations",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "serif")

#|=============================================================================

# extract elevation data from coordinates [elev_data_sf] [elev_data_tbl] ----
#|=============================================================================

library(elevatr)
library(sf)

## FOR SHAPE FILE centroids_sf
# Make sure the points are in WGS84 (EPSG:4326) # "centroids_sf" current CRS is UTM zone 43N
centroids_wgs <- st_transform(centroids_sf, crs = 4326)

# Extract elevation
elevation_points_sf <- get_elev_point(locations = centroids_wgs, src = "aws", units = "meters", z = 10)
st_write(elevation_points_sf, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_sf.shp")


## FOR tibble df "centroids_coords"
centroids_sf2 <- st_as_sf(centroids_coords, coords = c("X", "Y"), crs = 32643)  # UTM zone 43N
centroids_wgs2 <- st_transform(centroids_sf2, crs = 4326)
elev_data2 <- get_elev_point(locations = centroids_wgs2, src = "aws", units = "meters", z = 10)

elevation_points_tbl <- st_drop_geometry(elev_data2)
elevation_points_tbl <- elevation_points_tbl %>% 
  select(id ,elevation) %>% rename(elevation_m=elevation)

library(writexl)
write_xlsx(elevation_points_tbl, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_tbl.xlsx")

elev_df <- cbind(st_drop_geometry(elev_data2),st_coordinates(elev_data2))






# =============================================================================
# DF 2022 [df_for_Land_map] [df_for_water_map]----

# [df_for_water_map] 
df_for_water_map <- 
  rmtl_InOut %>% select(
    hh_id,in1_out0, mm4 ,hh_6m_2021_22 , elevation,distance_km,
    south1_north0,a5,drip_use, ir_use) %>% 
  rename(in_project  = in1_out0, drip_installed=mm4) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) #  %>% 
# mutate(Elevation=
#          case_when( elevation==0~ 519,elevation==1~ 522,
#                     elevation==2~ 525,elevation==3~ 528,
#                     elevation==4~ 531,elevation==5~ 534,
#                     elevation==6~ 537,elevation==7~ 540,
#                     TRUE ~ 1000))


# [df_for_Land_map]
df_for_Land_map <- 
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
    rmtl_InOut %>% select(hh_id,in1_out0,drip_use, ir_use,elevation,distance_km, south1_north0  )
  ) %>% rename(in_project=in1_out0) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) 

rd_water_with_coords
rd_land_with_coords


# ##      [rd_water_with_coords]       ----

# 1. Bind the df's
rd_water_with_coords <- 
  multi_hh_df %>% select(hh_id, X, Y) %>%
  inner_join(df_for_water_map, by = "hh_id")

# 2. Conver to sf format
rd_water_with_coords <- rd_water_with_coords %>%
  st_as_sf(coords = c("X", "Y"), crs = 32643) # UTM

# 3. Compute distance to the southern boundary line  (south_edge_sf) (combined_edge_sf)
rd_water_with_coords$dist_to_south_m <- st_distance(
  rd_water_with_coords,
  combined_edge_sf)
  # south_edge_sf)

# 3. Convert from units to numeric meters (optional but clearer)
rd_water_with_coords$dist_to_south_m <- as.numeric(rd_water_with_coords$dist_to_south_m)
rd_water_with_coords$elevation <- as.factor(rd_water_with_coords$elevation)


# ##      [rd_land_with_coords]        ----
# A tibble: 1,378

# 1. Bind the df's
rd_land_with_coords <- 
  multi_hh_df %>% select(hh_id, X, Y) %>%
  inner_join(df_for_Land_map, by = "hh_id") 

# 2. Conver to sf format
rd_land_with_coords <- 
  st_as_sf(rd_land_with_coords,
  coords = c("X", "Y"), crs = 32643)  # UTM

# 3. Compute distance to the southern boundary line
rd_land_with_coords$dist_to_south_m <- st_distance(
  rd_land_with_coords,
  combined_edge_sf)
  # south_edge_sf) # df south_edge_sf creating is below

# 4. Convert from units to numeric meters (optional but clearer)
rd_land_with_coords$dist_to_south_m <- as.numeric(rd_land_with_coords$dist_to_south_m)



# summary stats for obs with coords ------------------------------

rd_water_with_coords %>% # df below
  st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup() %>% 
  mutate(Sample=sum(N)/2 ) %>% 
  kable() %>% kable_minimal()

rd_land_with_coords %>% # df below
  st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup()%>% 
  mutate(Sample=sum(N)/2 )%>% 
  kable() %>% kable_minimal()


rd_land_with_coords %>% st_drop_geometry() %>%
  group_by(in_project) %>%
  summarise(
    n = n(),
    acre_drip  = mean(acre_drip , na.rm = TRUE),
    acre_ir = mean(acre_ir , na.rm = TRUE),
    acre_cult = mean(acre_cult , na.rm = TRUE),
    pct_drip_land = mean(pct_drip_land , na.rm = TRUE),
    land_holding = mean(land_holding, na.rm = TRUE)
  ) %>% arrange(desc(in_project))%>% 
  kable() %>% kable_minimal()





# =============================================================================
# MAPS ----
library(ggplot2)
library(sf)

# Map | elevation                        ----
rd_water_with_coords
ramthal_border_utm

# Create custom labels
elevation_labels <-
  c("0" = "~519 m", "1" = "~521 m", "2" = "~524 m", "3" = "~527 m",
    "4" = "~531 m", "5" = "~534 m", "6" = "~537 m", "7" = "~540 m" )

library(RColorBrewer)
dev.new()
ggplot() + # Export 650 X 490
  geom_sf(data = rd_water_with_coords, aes(fill = as.factor(elevation)),
          color = "gray70", shape = 21, size = 2, alpha = 0.7) +
  scale_fill_brewer(palette = "Reds", name = "Elevation",
                    labels = elevation_labels) +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  labs(title = "Farmer Locations by Elevation",x = NULL, y = NULL) +
  theme_minimal(base_family = "serif")

# Map | drip_use                         ----
rd_water_with_coords
ramthal_border_utm

ggplot() +  # Export 650 X 490
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  geom_sf(data = rd_water_with_coords, aes(color = factor(drip_use)), size = 1.5) +
  scale_color_manual(
    name = "Farmers who used \nat least once",
    values = c("0" = "gray70", "1" = "dodgerblue4"),
    labels = c("0" = "Use", "1" = "Didn't Use")) +
  labs(title = "Drip Irrigation Users In & Out Ramthal Project (2016-2023)",x = NULL,y = NULL
  ) +theme_minimal(base_family = "serif")

# Map | drip_use in southern boundary"   ----

rd_water_south1 <- rd_water_with_coords %>% filter(south1_north0 ==1)
# rd_water_south1 <- rd_water_with_coords %>% filter(dist_to_south_m < 1500)

ggplot() +
  geom_sf(data = rd_water_south1 %>% filter(drip_use == 1), 
          color = "dodgerblue4", size = 1.5) +
  geom_sf(data = rd_water_south1 %>% filter(drip_use == 0), 
          color = "gray60", size = 1.5) +
  geom_sf(data = south_edge_sf,color = "red4", linewidth= 1) +
  labs( title= "Drip Adoption Around the Southern Boundary",
  ) + theme_minimal(base_family = "serif")


# Map | acre_drip   ----

# Create color group
rd_land_with_coords2 <- rd_land_with_coords%>% 
  mutate(acre_drip2= # To distinguish between 0 acre_drip of drip_use==1 and 0 acre_drip of drip_use==0
           ifelse(drip_use==1 & acre_drip==0,0.0001,acre_drip)) %>% 
  mutate(color_group = case_when(
    acre_drip2  == 0.0001 ~ "gray70",
    acre_drip2  == 0      ~ "gray90",
    acre_drip  <= 2       ~ "dodgerblue",
    acre_drip  > 2        ~ "dodgerblue4",
    TRUE          ~ "gray"    ))%>% arrange(desc(color_group))

# acre_drip
rd_land_with_coords2 %>% # Export 650 X 490
  ggplot() +
  geom_sf(aes(color = factor(color_group)), size = 2) +  # map color_group as a factor
  scale_color_manual(name = "Drip-Irrigated Acres",
                     values = c("gray70" = "gray70",          # 0 acre for drip_use==1
                                "gray90" = "gray90",          # 0 acre for drip_use==0
                                "dodgerblue" = "dodgerblue",  # <= 2 acres
                                "dodgerblue4" = "dodgerblue4" ),# > 2 acres
                     labels = c("gray70" = "0 acres | Use before 2022",
                                "gray90" = "Non-users",
                                "dodgerblue" = "≤ 2 acres",
                                "dodgerblue4" = "> 2 acres") ) +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  labs(title = "Acre Land Under Drip Irrigation") +
  theme_minimal(base_family = "serif")

# Map | acre_drip in southern boundary  ----
rd_land_with_coords2%>% # Export 650 X 490
  filter(south1_north0 ==1) %>%
  ggplot() +
  geom_sf(aes(color = factor(color_group)), size = 2) +  # map color_group as a factor
  scale_color_manual(name = "Drip-Irrigated Acres",
                     values = c("gray70" = "gray70",          # 0 acre for drip_use==1
                                "gray90" = "gray90",          # 0 acre for drip_use==0
                                "dodgerblue" = "dodgerblue",  # <= 2 acres
                                "dodgerblue4" = "dodgerblue4" ),# > 2 acres
                     labels = c("gray70" = "0 acres | Use before 2022",
                                "gray90" = "Non-users",
                                "dodgerblue" = "≤ 2 acres",
                                "dodgerblue4" = "> 2 acres") ) +
  geom_sf(data = south_edge_sf, color = "red3", linewidth = 1) +
  labs(title = "Acre Land Under Drip Irrigation | southern boundary") +
  theme_minimal(base_family = "serif")





# library(RColorBrewer)  ----
library(RColorBrewer)
display.brewer.all()
graphics.off()  # shuts down all graphics devices


YIOrRd
YIOrBr
YIGnBu
YIGn
Reds
RdPu
Purples
PuRd
PuBuGn
PuBu
OrRd
Oranges
Greys
"Greens"
"GnBu"
BuPu
"BuGn"
Blues

Set3
Set2
Set1
Pastel2
Pastel1
Paired
Dark2
Accent

Spectral
RdYIGn
RdYIBu
RdGy
RdBu
PuOr
PRGn
PİYG
BrBG

















# =============================================================================
# OLD CODE ............................................................. ----
# library(raster)
# shp <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
library(dplyr)
library(ggplot2)
library(sf)

# Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
# Ramthal_East_shp %>% ggplot() +geom_sf()

shapefile_wgs84 <- st_transform(shapefile, crs = st_crs(4326))  # 4326 is the EPSG code for WGS84 (longitude and latitude)


#  shapfile_rmtl_2022----
shp_index22_NA <-  # 188 hh NAs
  list_shape_code %>% inner_join(hh_2022) %>% filter(is.na(survey))

shp_rmtl_id <- # id polygon
  Ramthal_East_shp %>% as.data.frame() %>% select(id) %>% distinct() %>% mutate(is=1)

library(tidyr)
survey_nu_22 <- # generating 118 new id's 
  a_plots_size[,c(1,3)] %>% right_join(shp_index22_NA[,1]) %>% 
  group_by(hh_id) %>% slice(1) %>% 
  separate(plotSrvy, into = c("survey", "hiss"), sep = "-") %>% 
  separate(survey, into = c("survey", "hiss"), sep = "/") %>% 
  mutate(survey=ifelse(survey%in% c("_999","12A"),"",survey)) %>% 
  mutate(survey=ifelse(survey=="12A","12",survey)) %>% 
  mutate(survey=ifelse(survey%in% c("44744","44714","44573"),"44",survey)) %>% 
  mutate(survey= as.numeric(survey)) %>% select(-hiss) %>% 
  left_join(shp_index22_NA[,c(1,5)]) %>% 
  mutate(idOLD=id, id= ifelse(!is.na(survey),survey+id,id )) %>%   
  left_join(shp_rmtl_id) %>% 
  mutate(idNA=ifelse(is.na(is),idOLD,id)) %>% 
  select(idNA,hh_id)
  
shp_index22 <- # implimanting 79 of the 118 new id's into the index
  list_shape_code %>% inner_join(hh_2022) %>% 
  left_join(survey_nu_22) %>% 
  mutate(id=ifelse(is.na(idNA),id,idNA)) %>% 
  select(id, hh_id, farmers_hh)

rm(shp_index22_NA,shp_rmtl_id, survey_nu_22)

#### shapfile_rmtl_2022 ###
shapfile_rmtl_2022 <- Ramthal_East_shp %>% left_join(shp_index22)

# MAPS -----
       -----------------------------
#|     |  R map for presentaition  |
#|     |  W 8.91   H  6.5          |
       -----------------------------

# polygons ----
ggplot(shapfile_rmtl_2022) +geom_sf()


# villages map ----

shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scale_fill_distiller(palette = "Greys")
scale_fill_distiller(palette = "YlOrBr")

# for the villages names  
shapfile_rmtl_2022 %>% ggplot()+ geom_sf(aes(fill = layer))+ theme(legend.position = "bottom")

# ZONE ----


zone_village_survey <- # A tibble: 1,700
  jain_rabbi_2019 %>% 
  select(zone,block ,village,survey_number) %>% distinct() %>%
  mutate(survey_number=as.numeric(survey_number)) 

zone_survey <- zone_village_survey %>% select(zone,block, survey_number) %>% distinct()%>% drop_na()
  
index4zone <- 
  shp_index22 %>% 
  right_join(list_shape_code) %>% # A tibble: 1,702
  rename(survey_number=survey) %>% 
  inner_join(zone_survey) %>% 

list_shape_code
  
# project/ non project ----

shapfile_rmtl_2022 %>% mutate(farmers_hh=ifelse(is.na(farmers_hh),"not_sampled",farmers_hh)) %>% 
  ggplot()+ geom_sf(aes(fill = farmers_hh), color="gray55")+
  scale_fill_manual(
    values = c("inside_ramthal" = "dodgerblue4", "outside_ramthal" = "orange3", "not_sampled" = "white"))+
  theme_minimal() +theme(legend.position = "none")

# share hh irrigation ----

shp_rmtl_id <- Ramthal_East_shp %>% as.data.frame() %>% select(id) %>% distinct() %>% mutate(is=1)

# irrigation from Gov supply group_by( id )
A.method <- a_irri_rain_method %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct()
  
A.gov <- 
  a_source_irri[,c(2,7)] %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(irri_source_num= ifelse(5 %in% irri_source_num, "5", ifelse(any(irri_source_num== 2), "2",ifelse(any(irri_source_num== 3), "3",ifelse(any(irri_source_num== 4), "4",ifelse(any(irri_source_num== 7), "7","0"))))) ) %>% ungroup() %>% 
  select(id,irri_source_num ) %>% distinct() 

A.mg=left_join(A.method,A.gov) %>% right_join(shp_rmtl_id) %>% 
  mutate(hh_6methods=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  mutate(irri_source_num=ifelse(is.na(irri_source_num),"not_sampled",irri_source_num ) ) %>% 
  mutate(mg=ifelse(hh_6methods!="rain" & irri_source_num==5,"irri_gov",hh_6methods ) )

# map
shapfile_rmtl_2022 %>% 
  left_join(A.mg) %>% 
  ggplot()+ geom_sf(aes(fill = mg), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("irri_gov" = "aquamarine2","drip" = "deepskyblue",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+
  theme_minimal() 


# irrigation group_by( id )
irrigation22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  select(id ,irrigation)

# irrigation group_by(hh_id )
irrigation.HH.22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(hh_id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% 
  ungroup() %>% select(hh_id,hh_6methods ) %>% distinct() %>% right_join(hh_2022) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"rain",hh_6methods ) ) %>% 
  select(hh_id ,irrigation)




# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80")+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
      "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
      "rain"="gray70" ,"not_sampled" = "white"))+theme(legend.position = "none")+
  theme_minimal() 

# map irrigation only
# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+theme_minimal() 

# location_on_pipe ----

location_on_pipe <-
  rmtl_srvy22 %>% select(hh_id,mm9,mm10,farmers_hh) %>% mutate(mm10=as.factor(mm10)) %>% 
  mutate(on_pipe1 = case_when(mm9>=0 & mm9<6~"0-5",mm9>=6 & mm9<51~"5+",TRUE~"NA")) %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(on_pipe2=ifelse("0-5" %in% on_pipe1 , "0-5",
                        ifelse(any(on_pipe1  == "5+"), "5+",
                               "no")))  %>% ungroup() %>%   
  select(id,on_pipe2 ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(on_pipe=ifelse(is.na(on_pipe2),"not_sampled",on_pipe2 ) ) %>% 
  select(id ,on_pipe)



# mm9
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>% 
  ggplot()+ geom_sf(aes(fill = on_pipe), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("0-5" = "maroon","5+" = "maroon4" ,"no" ="gray70" ,"not_sampled" = "white"))+
  theme_minimal()

#mm10
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>%
  ggplot()+ geom_sf(aes(fill = mm10), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()



# irrigation source 2022  -----

# rank1
source_rank_one <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank1= # former rank_1
           ifelse(5 %in% l7_rank_1 , "5", 
           ifelse(any(l7_rank_1  == 2), "2",
           ifelse(any(l7_rank_1  == 3), "3",
           ifelse(any(l7_rank_1  == 4), "4",
           ifelse(any(l7_rank_1  == 7), "7",
                            "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank1 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank1=ifelse(is.na(source_rank1),"not_sampled",source_rank1)) %>% 
  select(id ,source_rank1) %>%  #irrigation_source
  mutate(source_rank1_1yes0no =
           ifelse(source_rank1==0,0,
           ifelse(source_rank1=="not_sampled",
                                     "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_one) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank1), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()


# rank2
source_rank_two <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank2=  # source_rank1
           ifelse(5 %in% l7_rank_2, "5", 
                  ifelse(any(l7_rank_2== 2), "2",
                         ifelse(any(l7_rank_2== 3), "3",
                                ifelse(any(l7_rank_2== 4), "4",
                                       ifelse(any(l7_rank_2== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank2 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank2=ifelse(is.na(source_rank2),"not_sampled",source_rank2)) %>% 
  select(id ,source_rank2) %>%
  mutate(source_rank2_1yes0no =
           ifelse(source_rank2==0,0,
                  ifelse(source_rank2=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_two ) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank2), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()



# rank1+rank2+rank3 GOV SUPPLY
source_gov_supply <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_Gov=  # source_rank1
           ifelse(5 %in% irri_source_num, "5", 
                  ifelse(any(irri_source_num== 2), "2",
                         ifelse(any(irri_source_num== 3), "3",
                                ifelse(any(irri_source_num== 4), "4",
                                       ifelse(any(irri_source_num== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_Gov) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_Gov=ifelse(is.na(source_Gov),"not_sampled",source_Gov)) %>% 
  select(id ,source_Gov) %>%
  mutate(source_Gov_1yes0no =
           ifelse(source_Gov==0,0,
                  ifelse(source_Gov=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join( source_gov_supply ) %>% 
  ggplot()+ geom_sf(aes(fill = source_Gov), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()







# first make use ----
first_yr <- rmtl_srvy22 %>% select(hh_id,contains("mw1a")) %>% mutate(first_yr_use=ifelse(is.na(mw1a),2000,mw1a))%>% mutate(mw1a=as.integer(mw1a) )
  

shapfile_rmtl_2022 %>% left_join(first_yr) %>%
  ggplot()+ geom_sf(aes(fill = first_yr_use), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()


cor.test(irrigation.HH.22$irri01,first_yr$mw1a) # 👎🏽






# cropping pattern ----

a_plots_crop %>% filter(season == "kha") %>% count(crop_common)
# Chillies Greengram Toor Maize Onions   Sunflower Oilseeds Pearl.millet_bajra                       

a_plots_crop %>% filter(season == "rabi") %>% count(crop_common)
# Bengal_gram Oilseeds Sorghum_jowar Sugarcane Wheat                         

#| OUT: Cereals Groundnut            ;Horticulture   



map_crop %>% filter(season == "kha") %>% count(crop_common)
map_crop %>% filter(season == "rabi") %>% count(crop_common)

map_crop <- 
  a_plots_crop %>% left_join(shp_index22) %>% 
  filter(season !="KHA22", plot_crop =="01_1"  ) %>% 
  select(season ,id,plot_crop,crop_common) %>% distinct() %>% 
  group_by(id, season) %>% slice(1)

map_kha_crop <- map_crop %>% filter(season =="kha")
map_rabi_crop <- map_crop %>% filter(season =="rabi")


shapfile_rmtl_2022 %>% left_join(map_kha_crop ) %>% 
  ggplot()+ 
  geom_sf(aes(fill = crop_common  ), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scico::scale_fill_scico(palette = "bilbao")

  
# https://ggplot2-book.org/scales-colour ----

erupt =
  shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40")+
  theme_minimal() +
  theme(legend.position = "none")

library(viridis)
erupt + scale_fill_viridis_c()
erupt + scale_fill_viridis_c(option = "H") #options A-H

library(RColorBrewer)
display.brewer.all()

erupt + scale_fill_distiller()
erupt + scale_fill_distiller(palette = "RdPu")
erupt + scale_fill_distiller(palette = "YlOrBr")


library(scico)
erupt + scico::scale_fill_scico(palette = "bilbao") # the default
erupt + scico::scale_fill_scico(palette = "vik")
erupt + scico::scale_fill_scico(palette = "lajolla")


