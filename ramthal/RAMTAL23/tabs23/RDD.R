library(dplyr)

# DF rd_land_2022 ----

rd_land_2022 <- cultivated_land_2022 %>% 
  group_by(hh_id ) %>% 
  summarise(acre_drip=sum(acre_drip),
            acre_ir=sum(acre_ir),
            acre_cult=sum(acre_cult),# Sum
            land_holding=mean(land_holding), # Mean
            pct_cult_land=sum(pct_cult_land),
            pct_drip_land=sum(pct_drip_land),
            pct_ir_land=sum(pct_ir_land) ) %>% 
  left_join(
    rmtl_InOut %>% select(hh_id,in1_out0,drip_use, ir_use )
  ) %>% rename(in_project=in1_out0) 














# Ramthal shapefile | Clean  Fix Centroids  ----
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


# STEP 5: Create centroids for each polygon (in meters)
Ramthal_centroids_utm <- Ramthal_clean %>%
  mutate(geometry = st_centroid(geometry))


# STEP 6: Extract X/Y coordinates from the centroids
coords <- st_coordinates(Ramthal_centroids_utm)

  # Combine coordinates with polygon id
centroids_coords <- as.data.frame(coords)
centroids_coords$id <- Ramthal_centroids_utm$id

# df [multi_hh_df]   ----
# Adjust farmer coordinates to offset multiple households per polygon (25m step in X and Y)

# multi_hh_df # A tibble: 1,489 
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
  ungroup()


# df's for water/Land usage with coordinates  ------
rmtl_InOut   # water usage # A tibble: 1,612
rd_land_2022 # Land usage  # A tibble: 1,578
multi_hh_df  # hh_id X Y   # A tibble: 1,489


### rd_water_with_coords  ----
# A tibble: 1,409

# 1. Bind the df's
rd_water_with_coords <- 
  rmtl_InOut %>% select(
    hh_id,in1_out0, mm4 ,hh_6m_2021_22 , elevation, around_boundary,
    south1_north0,a5,drip_use, ir_use) %>% 
  rename(in_project  = in1_out0, drip_installed=mm4) %>% 
  inner_join(multi_hh_df %>% select(hh_id, adj_X, adj_Y), by = "hh_id") %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) %>% 
  mutate(elevation = ifelse(in_project == 1, elevation, -elevation))



# 2. Conver to sf format
rd_water_with_coords <- st_as_sf(
  rd_water_with_coords,
  coords = c("adj_X", "adj_Y"),
  crs = 32643)  # UTM

# 3. Compute distance to the southern boundary line
rd_water_with_coords$dist_to_south_m <- st_distance(
  rd_water_with_coords,
  southern_edge_sf)

# 4. Convert from units to numeric meters (optional but clearer)
rd_water_with_coords$dist_to_south_m <- as.numeric(rd_water_with_coords$dist_to_south_m)

# 5. Make distance negative for farmers outside the project
rd_water_with_coords$rd_distance <- ifelse(
  rd_water_with_coords$in_project == 0,
  -rd_water_with_coords$dist_to_south_m,
  rd_water_with_coords$dist_to_south_m)

library(RColorBrewer)
display.brewer.all()
graphics.off()  # shuts down all graphics devices
rd_water_with_coords$elevation <- as.factor(rd_water_with_coords$elevation)
dev.new()
ggplot() +
  geom_sf(data = rd_water_with_coords, aes(color = elevation), size = 3.5) +
  scale_color_brewer(palette = "PRGn", name = "Elevation Bin") +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1.2) +
  labs(
    title = "Farmer Locations by Elevation",
    subtitle = "Binned every 4m (absolute distance)",x = NULL, y = NULL
  ) +theme_minimal(base_family = "serif")




ggplot() +
  geom_sf(data = rd_water_with_coords, # Layer 1: Elevation (gradient fill)
          aes(color = elevation, shape = factor(drip_use)), size = 2.5) +
  scale_color_viridis_c(option = "B", name = "Elevation") +
  scale_shape_manual(
    values = c(`0` = 1, `1` = 16),  # 0 = circle (hollow), 1 = solid circle
    name = "Drip Use"
  ) +
    geom_sf(data = ramthal_border_utm, # Layer 1
          color = "black", linewidth = 1.2, fill = NA) +
    labs(
    title = "Elevation and Drip Use within Ramthal Project",
    subtitle = "Elevation (color) and Drip Usage (shape) across zone boundaries",
    x = NULL, y = NULL
  ) +theme_minimal(base_family = "serif") +theme(legend.position = "right")



### rd_land_with_coords  ----
# A tibble: 1,378

# 1. Bind the df's
rd_land_with_coords <- 
  multi_hh_df %>% select(hh_id, adj_X, adj_Y) %>%
  inner_join(rd_land_2022, by = "hh_id") %>% 
  left_join(rmtl_InOut %>% select(hh_id,elevation) ) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation), 
         elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation),
         elevation = ifelse(in_project == 1, elevation, -elevation))

# 2. Conver to sf format
rd_land_with_coords <- st_as_sf(
  rd_land_with_coords,
  coords = c("adj_X", "adj_Y"),
  crs = 32643)  # UTM

# 3. Compute distance to the southern boundary line
rd_land_with_coords$dist_to_south_m <- st_distance(
  rd_land_with_coords,
  southern_edge_sf) # df southern_edge_sf creating is below

# 4. Convert from units to numeric meters (optional but clearer)
rd_land_with_coords$dist_to_south_m <- as.numeric(rd_land_with_coords$dist_to_south_m)

# 5. Make distance negative for farmers outside the project
rd_land_with_coords$rd_distance <- ifelse(
  rd_land_with_coords$in_project == 0,
  -rd_land_with_coords$dist_to_south_m,
  rd_land_with_coords$dist_to_south_m)







# summary stats --------------------------------

rd_water_with_coords %>% st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup() %>% 
  mutate(Sample=sum(N)/2 )

rd_land_with_coords %>% st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup()%>% 
  mutate(Sample=sum(N)/2 )


rd_land_with_coords %>% st_drop_geometry() %>%
  group_by(in_project) %>%
  summarise(
    n = n(),
    acre_drip  = mean(acre_drip , na.rm = TRUE),
    acre_ir = mean(acre_ir , na.rm = TRUE),
    acre_cult = mean(acre_cult , na.rm = TRUE),
    pct_drip_land = mean(pct_drip_land , na.rm = TRUE),
    land_holding = mean(land_holding, na.rm = TRUE)
  ) %>% arrange(desc(in_project))




# df to Sothe boundary [southern_edge_sf]  =====================================
library(sf)

south1_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/1 south-in.shp")

plot(st_geometry(south1_in ),main = "Project's South Boundary")
dev.new()
plot(st_geometry(south1_in))

st_crs(south1_in)
south1_utm <- st_transform(south1_in, crs = 32643)
st_crs(south1_utm)

# easternmost <- boundary_coords[which.max(boundary_coords[, "X"]), ]
# easternmost <- boundary_coords[which.min(boundary_coords[, "X"]), ]

# 📍 Step 1: Extract boundary coordinates from polygon
boundary_coords <- st_coordinates(south1_utm)

# 👀 Step 2: Visualize coordinates to identify problematic points
plot(boundary_coords[, "X"], boundary_coords[, "Y"],
     type = "l", col = "gray", main = "Polygon Coordinates")
points(boundary_coords[, "X"], boundary_coords[, "Y"], col = "yellow", pch = 19)
text(boundary_coords[, "X"], boundary_coords[, "Y"],
     labels = seq_len(nrow(boundary_coords)), cex = 0.5, pos = 3)

# ✂️ Step 3: Remove bad points (retain only the clean southern segment)
#             Manual selection to remove points 2-88 from step 2 plot
boundary_coords_clean <- boundary_coords[89:nrow(boundary_coords), ]

# 🧵 Step 4: Create a LINESTRING from cleaned coordinates
southern_edge_line <- st_linestring(as.matrix(boundary_coords_clean[, c("X", "Y")]))

# 🌍 Step 5: Wrap into an sf object using the original CRS (UTM zone 43N)
southern_edge_sf <- st_sfc(southern_edge_line, crs = st_crs(south1_utm))

# 🖼️ Step 6: Plot the final cleaned southern edge
plot(st_geometry(southern_edge_sf), col = "red", lwd = 2,
     main = "Southern Border")

# Step 7 😄:  Ensure centroids are in sf format
centroids_sf <- st_as_sf(centroids_coords, coords = c("X", "Y"), crs = 32643)

# Final - Calculate shortest distance to the boundary
centroids_coords$dist_to_boundary <- st_distance(
  centroids_sf,
  southern_edge_sf
)



# df to Ramthal boundary [ramthal_edge_sf]  =====================================
library(sf)

ramthal_border <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/ramthal_border.shp")

dev.new()
plot(st_geometry(ramthal_border ),main = "Project's Boundary")

st_crs(ramthal_border)


dev.new()  # opens a new plotting window
plot(st_geometry(ramthal_border))
st_crs(ramthal_border)

library(sf)

# 🧭 1. Fix the ramthal_border (project boundary lines)
# Drop Z dimension (from LINESTRING Z to LINESTRING)
ramthal_border_utm <- st_zm(ramthal_border, drop = TRUE)

# The original file had wrong CRS label (WGS84), but it's actually in UTM meters
# So: we assign (not transform) the correct UTM CRS — EPSG:32643
st_crs(ramthal_border_utm) <- 32643


# 📍 2. Convert centroids_coords to spatial points
# Convert to sf object with correct CRS
centroids_sf <- st_as_sf(
  centroids_coords,
  coords = c("X", "Y"),
  crs = 32643)  # UTM zone 43N (meters)

ggplot() +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  geom_sf(data = centroids_sf, color = "dodgerblue2", size = 1.5, alpha = 0.6) +
  labs(
    title = "Ramthal Project Boundaries and Farmer Centroids",
    subtitle = "Black lines: Project zone boundaries | Blue dots: Farmer locations",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "serif")

# Map of drip users ---------------------------------
# Merge spatial info
rd_land_with_coords <- rd_land_2022 %>%
  left_join(multi_hh_df %>% select(hh_id, adj_X, adj_Y), by = "hh_id") %>%
  filter(!is.na(adj_X), !is.na(adj_Y))  # keep only points with coordinates

# Convert to sf object
rd_land_sf <- st_as_sf(
  rd_land_with_coords,
  coords = c("adj_X", "adj_Y"),
  crs = 32643 )  # UTM

# Plot map
ggplot() +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  geom_sf(data = rd_land_sf, aes(color = factor(drip_use)), size = 1.5, alpha = 0.8) +
  scale_color_manual(
    name = "",
    values = c("0" = "gray60", "1" = "dodgerblue4"),
    labels = c("0" = "Use DI", "1" = "Didn't Use Di")
  ) +
  labs(
    title = "Drip Irrigation Users in the Ramthal Project Area (2016-2023)",
    subtitle = "HH who used at least once | N = 1,378",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "serif")


# BIND DFs --------------------------------------------------------

# df 1
Ramthal_clean # geometry=POLYGON [m]

# df2
centroids_coords # 3548 [X, Y, id ]

# df 3
rd_land_2022  # A tibble: 1,578 [in_project ,DI, Ir ]

# ===================================================

# df 4 bind

# df all obs
rd_data <- one_per_polygon[,-(2)] %>%
  left_join(rd_land_2022, by = "hh_id") %>% 
  left_join(centroids_coords, by = "id") %>% 
  mutate(rd_distance = ifelse(in_project == 0,
                              -as.numeric(dist_to_boundary),
                              as.numeric(dist_to_boundary)))

# drip_use only
rd_data <- rd_data %>% filter(drip_use==1)

# drip_use in 1.5Km distance
rd_data <- rd_data %>% filter(drip_use==1,
    rd_distance<1500,rd_distance>(-1500) )



# summary stats --------------------------------

rd_water_with_coords %>% st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup() %>% 
  mutate(Sample=sum(N)/2 )

rd_land_with_coords %>% st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup()%>% 
  mutate(Sample=sum(N)/2 )


rd_land_with_coords %>% st_drop_geometry() %>%
  group_by(in_project) %>%
  summarise(
    n = n(),
    acre_drip  = mean(acre_drip , na.rm = TRUE),
    acre_ir = mean(acre_ir , na.rm = TRUE),
    acre_cult = mean(acre_cult , na.rm = TRUE),
    pct_drip_land = mean(pct_drip_land , na.rm = TRUE),
    land_holding = mean(land_holding, na.rm = TRUE)
  ) %>% arrange(desc(in_project))


# RD ----
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







# 1. BAR PLOT: DI acres per farmer by distance to boundary  ----

quantile(rd_data$DI, probs = c(0.01, 0.99), na.rm = TRUE)

rd_data %>%
  filter(acre_drip < 5.97) %>%  # Keep zeros, drop top 1% outliers
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
rd_data %>% 
  filter(rd_distance<1500,rd_distance>(-1500)) %>% 
ggplot(aes(x = rd_distance)) +
  geom_histogram(fill = "lightblue", bins = 50) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red3") +
  labs(
    title = "Histogram of Distance to Project Boundary",
    x = "Distance to Boundary (meters)",
    y = "Number of Farmers"
  ) +
  theme_minimal(base_family = "serif")


# 3. MAP:  ----
library(ggplot2)
library(sf)

# Step 1: Convert rd_data to sf using X/Y (only non-missing)
rd_data_sf <- rd_data %>%
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 32643)

# Step 2: Plot
ggplot() +
  geom_sf(data = rd_data_sf %>% filter(drip_use == 1), # Farmers: drip_use == 1 (lightblue)
          color = "dodgerblue4", size = 2, alpha = 0.8) +
  geom_sf(data = rd_data_sf %>% filter(drip_use == 0), # Farmers: drip_use == 0 (gray)
          color = "gray", size = 2, alpha = 0.5) +
  geom_sf(data = southern_edge_sf,                     # Project boundary
          color = "red3", linewidth = 1.2) +
  labs(
    title = "Drip Adoption Around the Project Boundary",
    subtitle = "Drip users (Light blue) and non drip users (Gray) \nRed line | Official southern project elevation boundary"
  ) +
  theme_minimal(base_family = "serif")



# Filter rd_data_sf to ±1500 meters from boundary
rd_data_sf %>%
  filter(rd_distance >= -1500, rd_distance <= 1500) %>% 
  ggplot() +
  geom_sf(data = rd_data_zoom %>% filter(drip_use == 1), 
          color = "dodgerblue4", size = 2, alpha = 0.8) +
  geom_sf(data = rd_data_zoom %>% filter(drip_use == 0), 
          color = "gray", size = 2, alpha = 0.5) +
  geom_sf(data = southern_edge_sf, 
          color = "red3", linewidth = 1.2) +
  labs(
    title = "Drip Use (2016–2023) within ±1500m of the Project Boundary",
    subtitle = "Drip users (Light blue) and non drip users (Gray) \nIn Red - the official southern project boundary"
  ) +
  theme_minimal(base_family = "serif")

# POLT ----

# Step 1: Create color group
rd_data_sf <- rd_data_sf %>%
  mutate(color_group = case_when(
    DI == 0       ~ "gray",
    DI <= 2       ~ "dodgerblue",
    DI > 2        ~ "dodgerblue4",
    TRUE          ~ "gray"  # fallback
  ))


# Step 2: Filter to ±1500m for zoomed view
rd_data_zoom <- rd_data_sf %>%
  filter(rd_distance >= -1500, rd_distance <= 1500)

ggplot() +
  geom_sf(data = rd_data_zoom, aes(color = color_group), size = 2) +
  scale_color_identity() +
  geom_sf(data = southern_edge_sf, 
          color = "red3", linewidth = 1.2) +
  labs(
    title = "Drip Use Intensity within ±1500m of the Project Boundary",
    subtitle = "Gray = no use, Light Blue = ≤2 acres, Dark Blue = >2 acres",
    caption = "Red = southern boundary"
  ) +
  theme_minimal(base_family = "serif")



# Mean plots ----
rdd_land_2022 <- cultivated_land_2022 %>% 
  filter(acre_drip<7) %>% 
  left_join(rmtl_InOut) %>% rename(in_project=in1_out0) %>% 
  filter(!is.na(elevation)) %>% 
  mutate(elevation=ifelse(elevation=="7+","7",elevation),
         Elevation=as.numeric(elevation)) %>%
  #filter(south1_north0==1, !is.na(distance_km)) %>% 
  group_by(hh_id,in_project,Elevation) %>% 
  summarise(DI=sum(acre_drip ),
            Ir=sum(acre_ir) ) %>% 
  group_by(in_project,Elevation) %>% 
  summarise(DI=mean(DI ),
            Ir=mean(Ir) ) %>% 
  mutate(Elevation=ifelse(in_project==0,Elevation*-1,Elevation)) %>% ungroup()


rdd_land_2022 %>% 
  ggplot() +
  #geom_line(aes(x = Elevation, y = DI, group = in_project, color = as.factor(in_project)), size = 1) +
  geom_line(aes(x = Elevation, y = Ir, group = in_project, color = as.factor(in_project)), size = 1) +
  scale_color_manual(values = c("orange3", "royalblue4"), guide = "none") +
  labs(x = "Out of Project                              In Project\nElevation", y = "Acre") +  # X axis title with two rows
  theme_classic() + scale_x_continuous(breaks = seq(-7, 7, 1))
# + facet_wrap(~season, ncol = 1)



rd_gca_2022 <- 
  df_gca %>% filter(Year==2022) %>% 
  left_join(rmtl_InOut) %>% 
  filter(!is.na(elevation)) %>% 
  mutate(elevation=ifelse(elevation=="7+","7",elevation),
         Elevation=as.numeric(elevation)) %>%
  #filter(south1_north0==1, !is.na(distance_km)) %>% 
  group_by(in_project,Elevation) %>% 
  summarise(GCA=mean(gca),
            NCA=mean(nca)) %>% 
  mutate(Elevation=ifelse(in_project==0,Elevation*-1,Elevation)) %>% 
  ungroup(
  ) # %>% filter(elevation >(-2) )

# Create the line plot with custom colors for GCA and NCA
ggplot(rd_gca_2022) +
  geom_line(aes(x = Elevation, y = GCA, group = in_project, color = as.factor(in_project)), size = 1.5) +  # GCA color mapping
  scale_color_manual(values = c("orange3", "royalblue4"), 
                     guide = "none") +  # Custom colors for GCA (lines)
  geom_line(aes(x = Elevation, y = NCA, group = in_project, color = as.factor(in_project)), size = 1) +  # NCA color mapping
  labs(x = "Out of Project                              In Project\nElevation", y = "Acre") +  # X axis title with two rows
  ggtitle("Table. Mean land size by elevation", 
          subtitle = "in thick line - 'Gross Crop Area' in acre and in thin line 'Net sown area' in acre") +  # Title and subtitle
  theme_classic() + 
  scale_x_continuous(breaks = seq(-7, 7, 1))  # Customize x-axis ticks








# Spatial regression discontinuity ----

rd_land_2022 <- cultivated_land_2022 %>% 
  filter(acre_drip<7) %>% 
  left_join(rmtl_InOut) %>% 
#  filter(drip_use == 1) %>% 
  rename(in_project=in1_out0) %>% 
  filter(!is.na(elevation)) %>% 
  mutate(elevation=ifelse(elevation=="7+","7",elevation),
         Elevation=as.numeric(elevation)) %>%
  #filter(south1_north0==1, !is.na(distance_km)) %>% 
  group_by(hh_id,in_project,Elevation) %>% 
  summarise(DI=sum(acre_drip ),
            Ir=sum(acre_ir)) %>% ungroup() %>% 
  mutate(run_var = ifelse(in_project == 1, Elevation, -Elevation)
         ) #%>%
  # mutate(actual_elev = 520 + Elevation * 5,
  #        run_var = actual_elev - 520) # cutoff at elevation = 520m (0 meters is boundary)


library(rdrobust)
library(dplyr)
library(ggplot2)

# Check running variable distribution
ggplot(rd_land_2022, aes(run_var, DI)) +
  geom_point(alpha=0.5) +
  geom_vline(xintercept=0, color='red', linetype='dashed') +
  geom_smooth(method='loess') +
  labs(x='Elevation above boundary (meters)',
       y='DI irrigated land (Acres)',
       title='Spatial RD Plot: DI vs. Elevation')

rdplot(rd_land_2022$DI, rd_land_2022$run_var, c = 0,
       title = "RD Plot of DI acres around Project Boundary",
       x.label = "Elevation from Boundary (units 0–7)",
       y.label = "Irrigated Land by DI (acres)")


# RD analysis at the boundary
rd_result <- rdrobust(y = rd_land_2022$DI, 
                      x = rd_land_2022$run_var, 
                      c = 0)  # cutoff exactly at Elevation = 0
summary(rd_result)

# produs table
library(modelsummary)
rd_land_2022 <- rd1_land_2022 %>% 
  mutate(treated = ifelse(run_var >= 0, 1, 0))

lm_rd <- lm(DI ~ treated + run_var, 
            data = rd_land_2022 %>% filter(abs(run_var) <= 2.183))
summary(lm_rd)
modelsummary(lm_rd,
             coef_rename = c('treated' = 'RD Effect',
                             'run_var' = 'Distance from Boundary'),
             statistic = c("std.error", "p.value"),
             title = "Regression Discontinuity (RD) Results for DI Irrigated Land",
             gof_omit = 'IC|Log.Lik.|F') 

modelsummary(lm_rd,
             coef_rename = c('treated' = 'RD Effect',
                             'run_var' = 'Distance from Boundary'),
             statistic = c("std.error", "p.value"),
             title = "Regression Discontinuity Results") %>%
  kableExtra::kable_styling()


# ......  DF FOR LATTER  ............................................................ ----
# (df 5 multi_hh_latlon) lon/lat columns  ----

multi_hh_sf <- st_as_sf(multi_hh_df, coords = c("adj_X", "adj_Y"), crs = 32643)
multi_hh_latlon <- st_transform(multi_hh_sf, crs = 4326)
coords_latlon <- st_coordinates(multi_hh_latlon)
multi_hh_latlon$lon <- coords_latlon[, "X"]
multi_hh_latlon$lat <- coords_latlon[, "Y"]

# ZONES map  ----

# View all column names to find the right zone identifier
names(ramthal_border)

# Extract unique zone names
unique(ramthal_border$layer)  # replace 'layer' with correct column name

# Assign zone number manually
ramthal_border_utm$zone_id <- paste0("Zone_", seq_len(nrow(ramthal_border_utm)))

# Redo join with zone IDs
centroids_with_zones <- st_join(centroids_sf, ramthal_border_utm["zone_id"])

# Use nearest line (zone boundary) to assign zone
nearest_zone_index <- st_nearest_feature(centroids_sf, ramthal_border_utm)

# Attach the zone name (from the `layer` column)
centroids_with_zones <- centroids_sf %>%
  mutate(zone_name = ramthal_border$layer[nearest_zone_index])




# 3D Ramthal  ------
library(sf)
library(plotly)

# Extract coordinates with Z
coords <- st_coordinates(ramthal_border)

# Make sure Z exists
head(coords)  # should show X, Y, Z

# Plot 3D line
plot_ly(
  x = coords[, "X"],
  y = coords[, "Y"],
  z = coords[, "Z"],
  type = "scatter3d",
  mode = "lines",
  line = list(color = 'red')
) %>%
  layout(
    title = "3D Plot of Ramthal Border (with Elevation)",
    scene = list(
      xaxis = list(title = "Easting"),
      yaxis = list(title = "Northing"),
      zaxis = list(title = "Elevation (Z)")
    )
  )


# ramthal boundry  ------

library(sf)

# Step 1: Convert 3D to 2D (remove Z) if not already done
ramthal_border_2d <- st_zm(ramthal_border)

# Step 2: Extract coordinates from all geometries
border_coords <- st_coordinates(ramthal_border_2d)

# Step 3: Visualize + label coordinates
dev.new()
plot(border_coords[, "X"], border_coords[, "Y"],
     type = "l", col = "gray", main = "Project Border Coordinates")
points(border_coords[, "X"], border_coords[, "Y"], col = "yellow", pch = 19)
text(border_coords[, "X"], border_coords[, "Y"],
     labels = seq_len(nrow(border_coords)), cex = 0.5, pos = 3)

# Filter for a zoomed-in bounding box (adjust as needed)
summary(border_coords[, c("X", "Y")])

# 🔹 Chunk 1 – Southwest
coords_chunk1 <- border_coords %>%
  as.data.frame() %>%
  filter(X < 617000, Y < 1776200)

plot(coords_chunk1$X, coords_chunk1$Y,
     type = "l", col = "gray", main = "Chunk 1: Southwest")
points(coords_chunk1$X, coords_chunk1$Y, col = "yellow", pch = 19)
text(coords_chunk1$X, coords_chunk1$Y,
     labels = rownames(coords_chunk1), cex = 0.6, pos = 3)

# 🔹 Chunk 2 – Southeast
coords_chunk2 <- border_coords %>%
  as.data.frame() %>%
  filter(X >= 617000, Y < 1776200)

plot(coords_chunk2$X, coords_chunk2$Y,
     type = "l", col = "gray", main = "Chunk 2: Southeast")
points(coords_chunk2$X, coords_chunk2$Y, col = "yellow", pch = 19)
text(coords_chunk2$X, coords_chunk2$Y,
     labels = rownames(coords_chunk2), cex = 0.6, pos = 3)

# 🔹 Chunk 3 – Northwest
coords_chunk3 <- border_coords %>%
  as.data.frame() %>%
  filter(X < 617000, Y >= 1776200)

plot(coords_chunk3$X, coords_chunk3$Y,
     type = "l", col = "gray", main = "Chunk 3: Northwest")
points(coords_chunk3$X, coords_chunk3$Y, col = "yellow", pch = 19)
text(coords_chunk3$X, coords_chunk3$Y,
     labels = rownames(coords_chunk3), cex = 0.6, pos = 3)

# 🔹 Chunk 4 – Northeast
coords_chunk4 <- border_coords %>%
  as.data.frame() %>%
  filter(X >= 617000, Y >= 1776200)

plot(coords_chunk4$X, coords_chunk4$Y,
     type = "l", col = "gray", main = "Chunk 4: Northeast")
points(coords_chunk4$X, coords_chunk4$Y, col = "yellow", pch = 19)
text(coords_chunk4$X, coords_chunk4$Y,
     labels = rownames(coords_chunk4), cex = 0.6, pos = 3)




ggplot() +
  geom_sf(data = ramthal_border_utm, aes(color = layer), linewidth = 1) +
  scale_color_brewer(palette = "Dark2", name = "Zone Layer") +
  labs(
    title = "Ramthal Project Zone Boundaries by Layer",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "serif")






# TO BE CONTINUES .....



