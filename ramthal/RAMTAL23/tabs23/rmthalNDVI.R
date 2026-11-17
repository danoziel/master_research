
library(readr)
ramthal_candidate_coordinates <- read_csv("C:/Users/Dan/OneDrive/ramthal_ndvi/ramthal_candidate_coordinates.csv")

# Display the column names and first six records.
names(ramthal_candidate_coordinates)
head(ramthal_candidate_coordinates)

# Select one TIFF file from the folder
folder <- dirname(file.choose())

# Find all nine seasonal files
files <- list.files(
  folder,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Display the files
basename(files)
length(files)

library(terra)
ramthal_2017 <- rast(files[1])
plot(ramthal_2017[["NDVI_max"]])

# Load all nine Rabi seasons into a single raster object.
# Each season contains three layers: NDVI_max, NDVI_mean, and valid_count.
# The original GeoTIFF files remain unchanged.
ramthal_ndvi <- terra::rast(files)

# Display the dimensions, resolution, coordinate system, and layer names.
ramthal_ndvi

##   ו־Spatial Attribution

# Convert filter coordinates into spatial points (WGS84).
ramthal_filters <- terra::vect(
  ramthal_candidate_coordinates,
  geom = c("longitude_WGS84", "latitude_WGS84"),
  crs = "EPSG:4326"
)

# Transform filter locations to the raster coordinate system (meters).
ramthal_filters <- terra::project(ramthal_filters, "EPSG:32643")

# Display the number of filter locations.
nrow(ramthal_filters)


#### Distance Measurement

# Calculate the distance (in meters) from each 10 m pixel
# to its nearest irrigation filter.
ramthal_distance <- terra::distance(
  ramthal_ndvi[[1]],
  ramthal_filters
)

# Display the distance raster properties.
ramthal_distance

# Keep distances up to 750 meters within the study area.
ramthal_distance <- ifel(
  ramthal_distance <= 750 & !is.na(ramthal_ndvi[[1]]),
  ramthal_distance,
  NA
)

# Display the updated distance range.
global(ramthal_distance, c("min", "max"), na.rm = TRUE)


# השלב הבא לפי האאוטליין הוא Spatial Attribution — שיוך כל פיקסל לפילטר הקרוב אליו (Nearest-Filter Assignment).
# כבר חישבנו את המרחק; עכשיו צריך לזהות לאיזה מבין 131 הפילטרים שייך כל פיקסל.

# Create Voronoi polygons around the 131 irrigation filters.
# Each polygon represents the area closest to one filter.
ramthal_voronoi <- terra::voronoi(ramthal_filters)

# Display the polygon count and attributes.
ramthal_voronoi

# Plot Voronoi polygons and overlay irrigation filter locations.
# Plot Voronoi polygons within the study area (750 m).
plot(ramthal_voronoi,
     col = "lightblue",
     xlim = c(608000, 625500),
     ylim = c(1766700, 1784200))
# Add irrigation filter locations.
points(ramthal_filters, col = "red", pch = 16, cex = 0.5)


# Assign each 10 m pixel to its nearest irrigation filter.
# Preserve the original candidate_id and exclude pixels beyond 750 m.
ramthal_filter_id <- terra::rasterize(
  ramthal_voronoi,
  ramthal_distance,
  field = "candidate_id"
)

ramthal_filter_id <- terra::mask(
  ramthal_filter_id,
  ramthal_distance
)

# Count the number of filter IDs represented in the raster.
length(terra::unique(values(ramthal_filter_id, na.rm = TRUE)))

# Create a pixel-level dataset for the 2017-2018 Rabi season.
# Each row represents one 10 x 10 m pixel.
# Include coordinates, vegetation indicators, distance, and nearest filter ID.

ramthal_data_2017 <- as.data.frame(
  c(ramthal_ndvi[[1:3]], ramthal_distance, ramthal_filter_id),
  xy = TRUE,
  na.rm = TRUE
)
# Rename the distance column in the pixel-level dataset.
names(ramthal_data_2017)[6] <- "distance_m"
# Display the first six observations.
head(ramthal_data_2017)
# 
# כל שורה מייצגת פיקסל אחד של 10×10 מטר, כלומר 100 מ"ר.
# עמודה	משמעות
# x	קואורדינטת מזרח של מרכז הפיקסל, במטרים
# y	קואורדינטת צפון של מרכז הפיקסל, במטרים
# NDVI_max	ערך הצימוח המרבי של הפיקסל במהלך העונה
# NDVI_mean	ערך הצימוח הממוצע שלו במהלך העונה
# valid_count	מספר צילומי הלוויין התקינים של הפיקסל
# NDVI_max (השנייה)	המרחק מהפילטר הקרוב ביותר, במטרים
# candidate_id	מזהה הפילטר הקרוב ביותר




# # # Next, we will create a single database for all nine seasons, where each row represents a pixel in a specific Rabi season.
# # # The code will save the result as a CSV file in the `ramthal_ndvi` folder.
# # Name the distance and filter identification layers.
names(ramthal_distance) <- "distance_m"
names(ramthal_filter_id) <- "candidate_id"

# Define the output database file.
ramthal_output <- file.path(folder, "ramthal_ndvi_database.csv")

# Process all nine Rabi seasons and combine them into one CSV file.
for (i in 1:9) {
  
  # Select the three vegetation layers for the current season.
  layers <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
  
  # Extract pixel coordinates, vegetation data, distance, and filter ID.
  season_data <- as.data.frame(
    c(ramthal_ndvi[[layers]], ramthal_distance, ramthal_filter_id),
    xy = TRUE,
    na.rm = TRUE
  )
  
  # Identify the Rabi season.
  season_data$season <- paste0(2016 + i, "-", 2017 + i)
  
  # Add the current season to the database.
  write.table(
    season_data,
    file = ramthal_output,
    sep = ",",
    row.names = FALSE,
    col.names = (i == 1),
    append = (i > 1)
  )
  
  cat("Completed:", unique(season_data$season), "\n")
}

# מה הקוד עושה? קורא כל עונה בנפרד, מוסיף לה את המרחק ואת מזהה הפילטר הקרוב, ומצרף אותה לבסיס הנתונים.



# The next step is to check the integrity of the database—ensuring that all nine seasons are included and that the number of pixels in each season makes sense.
# Count pixel observations in each Rabi season.
table(read.csv(ramthal_output)$season)

# סיימנו את בניית בסיס הנתונים לפי שיטת Nearest-Filter (Voronoi).
#............................................................................

# השלב הבא באאוטליין: Fixed-Radius Buffer Analysis 
# — יצירת בסיס נתונים מקביל שמאפשר חפיפה בין אזורי הפילטרים, לצורך השוואה בין שתי שיטות השיוך.

# Step: Create 750-meter buffers around all irrigation filters.
# Preserve overlapping areas for the second spatial analysis method.

ramthal_buffers <- terra::buffer(ramthal_filters, width = 750)

# Display the number of buffer polygons.
nrow(ramthal_buffers)

# Step: Visualize the 750 m buffers and their overlaps.
# Red points represent irrigation filters.
# Display all 131 buffer boundaries without filled polygons.
plot(ramthal_buffers, col = NA, border = "blue",
     xlim = c(608000, 625500),
     ylim = c(1766700, 1784200))

# Show the filter locations.
points(ramthal_filters, col = "red", pch = 16, cex = 0.5)

# Step: Extract pixel-level NDVI data for overlapping filter buffers.
# Preserve each filter-pixel association, including shared pixels.

ramthal_buffer_pixels <- terra::extract(
  ramthal_ndvi[[1:3]],
  ramthal_buffers,
  cells = TRUE
)

# Display the first six records.
head(ramthal_buffer_pixels)



# Step: Assess missing NDVI data in the buffer analysis.
# Count pixels with valid and missing NDVI values.\
table(is.na(ramthal_buffer_pixels$NDVI_max))
head(ramthal_buffer_pixels)


# Step: Remove observations with missing NDVI data.
# Keep only valid pixel-filter observations for the buffer analysis.

ramthal_buffer_pixels <- ramthal_buffer_pixels[
  !is.na(ramthal_buffer_pixels$NDVI_max),
]


# Step: Calculate the distance between each pixel and its assigned filter.
# A pixel may have different distances to different filters.

pixel_xy <- terra::xyFromCell(ramthal_ndvi, ramthal_buffer_pixels$cell)
filter_xy <- terra::crds(ramthal_filters)[ramthal_buffer_pixels$ID, ]

ramthal_buffer_pixels$distance_m <- sqrt(
  rowSums((pixel_xy - filter_xy)^2)
)

# Display the updated data.
head(ramthal_buffer_pixels)


# Step: Build the Fixed-Radius Buffer database for all nine Rabi seasons.
# Each pixel is recorded separately for every filter within 750 meters.
# Preserve overlapping observations, NDVI values, distances, and filter IDs.

ramthal_buffer_output <- file.path(folder, "ramthal_buffer_database.csv")

filter_xy <- terra::crds(ramthal_filters)

for (i in 1:9) {
  
  # Select the three NDVI layers for the current season.
  layers <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
  
  # Extract pixel data within each filter's 750 m buffer.
  season_data <- terra::extract(
    ramthal_ndvi[[layers]],
    ramthal_buffers,
    cells = TRUE
  )
  
  # Remove observations with missing NDVI values.
  season_data <- season_data[!is.na(season_data$NDVI_max), ]
  
  # Calculate pixel coordinates and distance to each associated filter.
  pixel_xy <- terra::xyFromCell(ramthal_ndvi, season_data$cell)
  
  season_data$distance_m <- sqrt(
    (pixel_xy[, 1] - filter_xy[season_data$ID, 1])^2 +
      (pixel_xy[, 2] - filter_xy[season_data$ID, 2])^2
  )
  
  season_data$x <- pixel_xy[, 1]
  season_data$y <- pixel_xy[, 2]
  season_data$candidate_id <- ramthal_filters$candidate_id[season_data$ID]
  season_data$season <- paste0(2016 + i, "-", 2017 + i)
  
  # Arrange the database columns.
  season_data <- season_data[, c(
    "x", "y", "cell", "NDVI_max", "NDVI_mean",
    "valid_count", "distance_m", "candidate_id", "season"
  )]
  
  # Save each season to the same CSV file.
  write.table(
    season_data,
    file = ramthal_buffer_output,
    sep = ",",
    row.names = FALSE,
    col.names = (i == 1),
    append = (i > 1)
  )
  
  cat("Completed:", unique(season_data$season), "\n")
}

# Step: Validate the Fixed-Radius Buffer database.
# Check that all nine Rabi seasons are included,
# count observations per season, and inspect the first records.

ramthal_buffer_data <- read.csv(ramthal_buffer_output)

table(ramthal_buffer_data$season)

head(ramthal_buffer_data)



# Step: Check the Fixed-Radius Buffer database.
# Count observations in each of the nine Rabi seasons.

table(ramthal_buffer_data$season)


# Step: Validate the Fixed-Radius Buffer database.
# Check NDVI and distance ranges without loading the full CSV into RAM.

library(data.table)

ramthal_check <- fread(
  ramthal_buffer_output,
  select = c("NDVI_max", "NDVI_mean", "distance_m")
)
summary(ramthal_check)


# Step: Validate filter coverage in the buffer database.
# Confirm that all 131 filters are represented.

ramthal_filter_check <- data.table::fread(
  ramthal_buffer_output,
  select = "candidate_id"
)

data.table::uniqueN(ramthal_filter_check$candidate_id)



# =============================================================================
# ־Descriptive Statistics.
# =============================================================================

# Step: Calculate descriptive vegetation statistics by distance.
# Group observations into 10-meter distance bins.
# Calculate mean NDVI values and observation counts for each bin.

ramthal_by_distance <- ramthal_check[, .(
  NDVI_max = mean(NDVI_max),
  NDVI_mean = mean(NDVI_mean),
  observations = .N
), by = .(distance_m = floor(distance_m / 10) * 10)]

head(ramthal_by_distance)

# Step: Visualize vegetation intensity by distance from irrigation filters.
# Compare maximum and mean NDVI across 10-meter distance bins.

ramthal_by_distance <- ramthal_by_distance[order(distance_m)]

# Step: Visualize the pooled distance-NDVI relationship.
# Use 10-meter distance bins across all nine Rabi seasons.


# Step: Create a pooled descriptive graph for all nine Rabi seasons.
# Compare three vegetation indicators by distance from water outlets.
# Use 10-meter distance bins and a provisional NDVI threshold of 0.36.

library(data.table)
library(ggplot2)


# Step: Calculate and visualize all three vegetation indicators.
# Pool all nine Rabi seasons using the Fixed-Radius Buffer method.
# Use 10-meter distance bins and an NDVI threshold of 0.36.

library(data.table)
library(ggplot2)

ramthal_threshold <- 0.36

ramthal_all_seasons <- fread(
  ramthal_buffer_output,
  select = c("distance_m", "NDVI_max", "NDVI_mean")
)

ramthal_by_distance <- ramthal_all_seasons[, .(
  Maximum_NDVI = mean(NDVI_max),
  Mean_NDVI = mean(NDVI_mean),
  GVF = mean(NDVI_max >= ramthal_threshold)
), by = .(distance_m = floor(distance_m / 10) * 10)]

rm(ramthal_all_seasons)

# Plot the three indicators with a secondary percentage axis.
ggplot(ramthal_by_distance, aes(x = distance_m)) +
  geom_line(aes(y = Maximum_NDVI, color = "Maximum NDVI"),
            linewidth = 1.2) +
  geom_line(aes(y = Mean_NDVI, color = "Mean NDVI"),
            linewidth = 1.2) +
  geom_line(aes(y = GVF, color = "Green Vegetation Fraction"),
            linewidth = 1.2) +
  scale_color_manual(values = c(
    "Maximum NDVI" = "#16856D",
    "Mean NDVI" = "#D58B35",
    "Green Vegetation Fraction" = "#5366B8"
  )) +
  scale_x_continuous(breaks = seq(0, 750, 150)) +
  scale_y_continuous(
    limits = c(0, 1),
    name = "NDVI",
    sec.axis = sec_axis(~ . * 100,
                        name = "Green Vegetation Fraction (%)")
  ) +
  labs(
    title = "Vegetation Patterns by Distance from Water Outlets",
    subtitle = "Ramthal Irrigation Project | Nine Rabi Seasons (2017–2026)",
    x = "Distance from water outlet (m)",
    color = NULL,
    caption = "GVF threshold: maximum seasonal NDVI ≥ 0.36 (provisional)"
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(data.table)

# Define colors for the three vegetation indicators.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#2E7D32",
  Mean_NDVI    = "#66BB6A",
  GVF          = "#1F78B4"
)


# Plot GVF as a proportion on the same scale as NDVI.
# The secondary axis displays GVF as a percentage.
# Prepare the existing 10-meter distance data for plotting.
plot_data_vegetation_distance <- data.table::melt(
  data.table::copy(data.table::as.data.table(ramthal_by_distance)),
  id.vars = "distance_m",
  measure.vars = c("Maximum_NDVI", "Mean_NDVI", "GVF"),
  variable.name = "Indicator",
  value.name = "Value"
)


library(ggplot2)
library(data.table)

# Define the consistent research color palette.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#1F78B4",
  Mean_NDVI    = "#74B9E6",
  GVF          = "#2E7D32"
)

# Prepare the existing pooled 10-meter distance data.
plot_data_vegetation_distance <- melt(
  copy(as.data.table(ramthal_by_distance)),
  id.vars = "distance_m",
  measure.vars = c("Maximum_NDVI", "Mean_NDVI", "GVF"),
  variable.name = "Indicator",
  value.name = "Value"
)

# Position observations at the midpoint of each 10-meter interval.
plot_data_vegetation_distance[
  , distance_midpoint := distance_m + 5
]

# Display GVF as a percentage.
plot_data_vegetation_distance[
  Indicator == "GVF", Value := Value * 100
]

# Use readable indicator labels.
plot_data_vegetation_distance[
  , Indicator := factor(
    Indicator,
    levels = names(colors_vegetation_indicators),
    labels = c("Maximum NDVI", "Mean NDVI", "GVF (%)")
  )
]

# Create three side-by-side panels with independent Y-axis ranges.
plot_vegetation_distance <- ggplot(
  plot_data_vegetation_distance,
  aes(x = distance_midpoint, y = Value, color = Indicator)
) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Indicator, nrow = 1, scales = "free_y") +
  scale_color_manual(
    values = setNames(
      unname(colors_vegetation_indicators),
      c("Maximum NDVI", "Mean NDVI", "GVF (%)")
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 250, 500, 750),
    limits = c(0, 750)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Vegetation Patterns by Distance from Water Outlets",
    subtitle = "Fixed-Radius Buffer Analysis (750 m) | Nine Rabi Seasons (2017–2026)",
    x = "Distance from water outlet (m)",
    y = NULL
  ) +
  theme_classic(base_size = 12, base_family = "serif") +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    panel.spacing = grid::unit(1.5, "lines")
  )

print(plot_vegetation_distance)











# Step: Calculate descriptive statistics for the three vegetation indicators.
# Summarize the 10-meter distance bins across all nine Rabi seasons.
# Results describe distance-bin averages, not individual pixel distributions.

ramthal_summary <- ramthal_by_distance[, lapply(
  .SD,
  function(x) c(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x)
  )
), .SDcols = c("Maximum_NDVI", "Mean_NDVI", "GVF")]

round(ramthal_summary, 3)


# Step: Calculate pixel-level descriptive statistics.
# Pool all nine Rabi seasons using the Fixed-Radius Buffer method.
# Summarize vegetation measurements before examining individual seasons.
# Overlapping pixels are counted once for each associated water outlet.

ramthal_pixel_summary <- ramthal_check[, .(
  Mean = c(
    mean(NDVI_max),
    mean(NDVI_mean),
    mean(NDVI_max >= 0.36)
  ),
  Median = c(
    median(NDVI_max),
    median(NDVI_mean),
    median(as.numeric(NDVI_max >= 0.36))
  ),
  SD = c(
    sd(NDVI_max),
    sd(NDVI_mean),
    sd(as.numeric(NDVI_max >= 0.36))
  ),
  Min = c(min(NDVI_max), min(NDVI_mean), 0),
  Max = c(max(NDVI_max), max(NDVI_mean), 1)
)]

ramthal_pixel_summary$Indicator <- c(
  "Maximum NDVI", "Mean NDVI", "GVF"
)

round(ramthal_pixel_summary[, 1:5], 3)


# Export the pooled vegetation indicators by 10-meter distance bins.
# Save the results for preparing Table 1 in the manuscript.

data.table::fwrite(
  ramthal_by_distance,
  file.path(folder, "ramthal_distance_summary.csv")
)







library(data.table)

# Create Table 1 from the existing 10-meter distance summary.
ramthal_buffer_distance_table <- as.data.table(
  copy(ramthal_by_distance)
)[
  , .(
    Maximum_NDVI = round(mean(Maximum_NDVI, na.rm = TRUE), 3),
    Mean_NDVI = round(mean(Mean_NDVI, na.rm = TRUE), 3),
    GVF_percent = round(mean(GVF, na.rm = TRUE) * 100, 1)
  ),
  by = .(band = floor(distance_m / 150))
]

# Add 150-meter distance labels.
ramthal_buffer_distance_table[
  , Distance_m := paste0(band * 150, "-", (band + 1) * 150)
]

# Arrange the results.
setorder(ramthal_buffer_distance_table, band)

setcolorder(
  ramthal_buffer_distance_table,
  c("Distance_m", "Maximum_NDVI", "Mean_NDVI", "GVF_percent")
)

ramthal_buffer_distance_table[, band := NULL]

# Display and export Table 1.
print(ramthal_buffer_distance_table)








library(data.table)

# Load the existing Fixed-Radius Buffers database.
ramthal_buffer_data <- fread("ramthal_buffer_database.csv")

# Define 50-meter distance bands within the 750-meter radius.
ramthal_buffer_data <- ramthal_buffer_data[
  !is.na(distance_m) &
    distance_m >= 0 &
    distance_m <= 750
]

ramthal_buffer_data[
  , distance_band_id := pmin(floor(distance_m / 50), 14L)
]

ramthal_buffer_data[
  , distance_band := paste0(
    distance_band_id * 50, "-",
    (distance_band_id + 1) * 50
  )
]

# Calculate vegetation indicators for each season and distance band.
ramthal_buffer_seasonal_50m <- ramthal_buffer_data[
  ,
  .(
    Maximum_NDVI = mean(NDVI_max, na.rm = TRUE),
    Mean_NDVI = mean(NDVI_mean, na.rm = TRUE),
    GVF = 100 * mean(NDVI_max >= 0.36, na.rm = TRUE),
    N = .N
  ),
  by = .(season, distance_band_id, distance_band)
]

setorder(
  ramthal_buffer_seasonal_50m,
  season,
  distance_band_id
)

# Save the seasonal descriptive results.
fwrite(
  ramthal_buffer_seasonal_50m,
  "ramthal_buffer_seasonal_50m.csv"
)

# Compare vegetation near and far from outlets in each season.
ramthal_buffer_seasonal_comparison <- ramthal_buffer_seasonal_50m[
  ,
  .(
    Near_Max_NDVI = Maximum_NDVI[distance_band_id == 0],
    Far_Max_NDVI = Maximum_NDVI[distance_band_id == 14],
    NDVI_Difference =
      Maximum_NDVI[distance_band_id == 0] -
      Maximum_NDVI[distance_band_id == 14],
    Near_GVF = GVF[distance_band_id == 0],
    Far_GVF = GVF[distance_band_id == 14],
    GVF_Difference =
      GVF[distance_band_id == 0] -
      GVF[distance_band_id == 14]
  ),
  by = season
]

# Display the comparison across nine Rabi seasons.
print(ramthal_buffer_seasonal_comparison)

rm(ramthal_buffer_data)
gc()




library(ggplot2)

# Use the established colors for the three indicators.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#1F78B4",
  Mean_NDVI    = "#74B9E6",
  GVF          = "#2E7D32"
)

# Create one 3x3 seasonal figure for a selected indicator.
create_seasonal_vegetation_plot <- function(indicator, y_label) {
  
  ggplot(
    ramthal_buffer_seasonal_50m,
    aes(
      x = distance_band_id * 50 + 25,
      y = .data[[indicator]]
    )
  ) +
    geom_line(
      color = colors_vegetation_indicators[[indicator]],
      linewidth = 0.9
    ) +
    geom_point(
      color = colors_vegetation_indicators[[indicator]],
      size = 1.2
    ) +
    facet_wrap(~ season, ncol = 3, scales = "free_y") +
    scale_x_continuous(
      breaks = c(0, 250, 500, 750),
      limits = c(0, 750)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    labs(
      title = paste(y_label, "by Distance from Water Outlets"),
      subtitle = "Fixed-Radius Buffer Analysis (750 m) | Nine Rabi Seasons",
      x = "Distance from water outlet (m)",
      y = y_label
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      panel.spacing = grid::unit(1.2, "lines")
    )
}

# Maximum NDVI: nine seasonal panels.
plot_seasons_maximum_ndvi <- create_seasonal_vegetation_plot(
  "Maximum_NDVI", "Maximum NDVI"
)

# Mean NDVI: nine seasonal panels.
plot_seasons_mean_ndvi <- create_seasonal_vegetation_plot(
  "Mean_NDVI", "Mean NDVI"
)

# GVF: nine seasonal panels.
plot_seasons_gvf <- create_seasonal_vegetation_plot(
  "GVF", "GVF (%)"
)

# Display all three figures.
print(plot_seasons_maximum_ndvi)
print(plot_seasons_mean_ndvi)
print(plot_seasons_gvf)







library(data.table)

# Summarize near-to-far vegetation differences for each Rabi season.
ramthal_buffer_seasonal_table <- ramthal_buffer_seasonal_50m[
  ,
  .(
    Maximum_NDVI_Near = Maximum_NDVI[distance_band_id == 0],
    Maximum_NDVI_Far = Maximum_NDVI[distance_band_id == 14],
    Maximum_NDVI_Diff = Maximum_NDVI[distance_band_id == 0] -
      Maximum_NDVI[distance_band_id == 14],
    
    Mean_NDVI_Near = Mean_NDVI[distance_band_id == 0],
    Mean_NDVI_Far = Mean_NDVI[distance_band_id == 14],
    Mean_NDVI_Diff = Mean_NDVI[distance_band_id == 0] -
      Mean_NDVI[distance_band_id == 14],
    
    GVF_Near = GVF[distance_band_id == 0],
    GVF_Far = GVF[distance_band_id == 14],
    GVF_Diff = GVF[distance_band_id == 0] -
      GVF[distance_band_id == 14]
  ),
  by = season
]

# Round the results for publication.
ramthal_buffer_seasonal_table[
  , (names(ramthal_buffer_seasonal_table)[-1]) :=
    lapply(.SD, round, 3),
  .SDcols = -1
]

# Display and export the seasonal comparison table.
print(ramthal_buffer_seasonal_table)

fwrite(
  ramthal_buffer_seasonal_table,
  "ramthal_buffer_seasonal_table.csv"
)



library(ggplot2)

# Define the agreed colors for the vegetation indicators.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#1F78B4",
  Mean_NDVI    = "#74B9E6",
  GVF          = "#2E7D32"
)

# Generate a 3x3 seasonal plot for one vegetation indicator.
plot_buffer_seasons_10m <- function(indicator, label) {
  ggplot(
    ramthal_buffer_seasonal_10m,
    aes(
      x = distance_bin_10m + 5,
      y = .data[[indicator]]
    )
  ) +
    geom_line(
      color = colors_vegetation_indicators[[indicator]],
      linewidth = 0.9
    ) +
    facet_wrap(~ season, ncol = 3, scales = "free_y") +
    scale_x_continuous(
      breaks = c(0, 250, 500, 750),
      limits = c(0, 750)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    labs(
      title = paste(label, "by Distance from Water Outlets"),
      subtitle = "Fixed-Radius Buffer Analysis (750 m) | Nine Rabi Seasons | 10-m intervals",
      x = "Distance from water outlet (m)",
      y = label
    ) +
    theme_classic(base_family = "serif", base_size = 12) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# Create and display the three seasonal figures.
plot_buffer_maximum_10m <- plot_buffer_seasons_10m(
  "Maximum_NDVI", "Maximum NDVI"
)

plot_buffer_mean_10m <- plot_buffer_seasons_10m(
  "Mean_NDVI", "Mean NDVI"
)

plot_buffer_gvf_10m <- plot_buffer_seasons_10m(
  "GVF", "GVF (%)"
)

print(plot_buffer_maximum_10m)
print(plot_buffer_mean_10m)
print(plot_buffer_gvf_10m)



library(data.table)

# Maximum NDVI: 75 distance bands across nine Rabi seasons.
table_buffer_maximum_10m <- dcast(
  ramthal_buffer_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "Maximum_NDVI"
)

# Mean NDVI: 75 distance bands across nine Rabi seasons.
table_buffer_mean_10m <- dcast(
  ramthal_buffer_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "Mean_NDVI"
)

# GVF (%): 75 distance bands across nine Rabi seasons.
table_buffer_gvf_10m <- dcast(
  ramthal_buffer_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "GVF"
)

# Round the displayed values without changing the original summaries.
table_buffer_maximum_10m[
  , (names(table_buffer_maximum_10m)[-1]) :=
    lapply(.SD, round, 3),
  .SDcols = -1
]

table_buffer_mean_10m[
  , (names(table_buffer_mean_10m)[-1]) :=
    lapply(.SD, round, 3),
  .SDcols = -1
]

table_buffer_gvf_10m[
  , (names(table_buffer_gvf_10m)[-1]) :=
    lapply(.SD, round, 1),
  .SDcols = -1
]

# View each table in RStudio.
View(table_buffer_maximum_10m)
View(table_buffer_mean_10m)
View(table_buffer_gvf_10m)


writexl::write_xlsx(
  list(
    Maximum_NDVI = table_buffer_maximum_10m,
    Mean_NDVI = table_buffer_mean_10m,
    GVF = table_buffer_gvf_10m
  ),
  "ramthal_buffer_seasonal_tables_10m.xlsx"
)
shell.exec(getwd())

# ===========================================================================


library(DBI)
library(duckdb)
library(data.table)

# Connect to DuckDB for processing the existing Voronoi database.
ramthal_voronoi_connection <- dbConnect(
  duckdb(),
  dbdir = ":memory:"
)

# Use the previously defined path to the Voronoi CSV.
ramthal_voronoi_file <- as.character(
  dbQuoteString(ramthal_voronoi_connection, ramthal_output)
)

# Calculate vegetation indicators for every season and 10-meter band.
ramthal_voronoi_seasonal_10m <- as.data.table(
  dbGetQuery(
    ramthal_voronoi_connection,
    paste0("
      SELECT
        season,
        CAST(FLOOR(distance_m / 10) AS INTEGER) * 10
          AS distance_bin_10m,
        AVG(NDVI_max) AS Maximum_NDVI,
        AVG(NDVI_mean) AS Mean_NDVI,
        100 * AVG(
          CASE WHEN NDVI_max >= 0.36 THEN 1.0 ELSE 0.0 END
        ) AS GVF,
        COUNT(*) AS N
      FROM read_csv_auto(", ramthal_voronoi_file, ")
      WHERE distance_m >= 0
        AND distance_m < 750
      GROUP BY season, distance_bin_10m
      ORDER BY season, distance_bin_10m
    ")
  )
)

# Calculate the pooled indicators using all nine seasons.
ramthal_voronoi_pooled_10m <- ramthal_voronoi_seasonal_10m[
  ,
  .(
    Maximum_NDVI = weighted.mean(Maximum_NDVI, N),
    Mean_NDVI = weighted.mean(Mean_NDVI, N),
    GVF = weighted.mean(GVF, N),
    N = sum(N)
  ),
  by = distance_bin_10m
]

# Close the database connection.
dbDisconnect(ramthal_voronoi_connection, shutdown = TRUE)

# Display the resulting data structures.
print(dim(ramthal_voronoi_seasonal_10m))
print(dim(ramthal_voronoi_pooled_10m))










library(ggplot2)
library(data.table)

# Consistent colors for the vegetation indicators.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#1F78B4",
  Mean_NDVI    = "#74B9E6",
  GVF          = "#2E7D32"
)

# Prepare pooled data for the three-panel figure.
ramthal_voronoi_plot_data <- melt(
  copy(ramthal_voronoi_pooled_10m),
  id.vars = "distance_bin_10m",
  measure.vars = c("Maximum_NDVI", "Mean_NDVI", "GVF"),
  variable.name = "Indicator",
  value.name = "Value"
)

ramthal_voronoi_plot_data[
  , Indicator := factor(
    Indicator,
    levels = names(colors_vegetation_indicators),
    labels = c("Maximum NDVI", "Mean NDVI", "GVF (%)")
  )
]

# Figure 1: pooled vegetation patterns, three panels.
ramthal_voronoi_pooled_plot <- ggplot(
  ramthal_voronoi_plot_data,
  aes(
    x = distance_bin_10m + 5,
    y = Value,
    color = Indicator
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Indicator, nrow = 1, scales = "free_y") +
  scale_color_manual(
    values = setNames(
      unname(colors_vegetation_indicators),
      c("Maximum NDVI", "Mean NDVI", "GVF (%)")
    )
  ) +
  scale_x_continuous(
    breaks = c(0, 250, 500, 750),
    limits = c(0, 750)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Vegetation Patterns by Distance from Water Outlets",
    subtitle = "Voronoi Analysis (750 m) | Nine Rabi Seasons (2017–2026)",
    x = "Distance from water outlet (m)",
    y = NULL
  ) +
  theme_classic(base_family = "serif", base_size = 12) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

# Function for a 3x3 seasonal figure.
plot_voronoi_seasons_10m <- function(indicator, label) {
  ggplot(
    ramthal_voronoi_seasonal_10m,
    aes(
      x = distance_bin_10m + 5,
      y = .data[[indicator]]
    )
  ) +
    geom_line(
      color = colors_vegetation_indicators[[indicator]],
      linewidth = 0.9
    ) +
    facet_wrap(~ season, ncol = 3, scales = "free_y") +
    scale_x_continuous(
      breaks = c(0, 250, 500, 750),
      limits = c(0, 750)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    labs(
      title = paste(label, "by Distance from Water Outlets"),
      subtitle = "Voronoi Analysis (750 m) | Nine Rabi Seasons | 10-m intervals",
      x = "Distance from water outlet (m)",
      y = label
    ) +
    theme_classic(base_family = "serif", base_size = 12) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# Figures 2-4: nine seasons for each indicator.
ramthal_voronoi_maximum_plot <- plot_voronoi_seasons_10m(
  "Maximum_NDVI", "Maximum NDVI"
)

ramthal_voronoi_mean_plot <- plot_voronoi_seasons_10m(
  "Mean_NDVI", "Mean NDVI"
)

ramthal_voronoi_gvf_plot <- plot_voronoi_seasons_10m(
  "GVF", "GVF (%)"
)

# Display the four figures.
print(ramthal_voronoi_pooled_plot)
print(ramthal_voronoi_maximum_plot)
print(ramthal_voronoi_mean_plot)
print(ramthal_voronoi_gvf_plot)


# ----- TABLE______ -----


library(data.table)

# Create the pooled table with all three vegetation indicators.
table_pooled_voronoi_10m <- copy(
  ramthal_voronoi_pooled_10m[
    , .(
      Distance_m = distance_bin_10m,
      Maximum_NDVI,
      Mean_NDVI,
      GVF
    )
  ]
)

# Create seasonal tables: distance bands in rows, seasons in columns.
table_maximum_voronoi_10m <- dcast(
  ramthal_voronoi_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "Maximum_NDVI"
)

table_mean_voronoi_10m <- dcast(
  ramthal_voronoi_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "Mean_NDVI"
)

table_gvf_voronoi_10m <- dcast(
  ramthal_voronoi_seasonal_10m,
  distance_bin_10m ~ season,
  value.var = "GVF"
)

# Keep the original values; round only the exported copies.
tables_export_voronoi <- list(
  Pooled = copy(table_pooled_voronoi_10m),
  Maximum_NDVI = copy(table_maximum_voronoi_10m),
  Mean_NDVI = copy(table_mean_voronoi_10m),
  GVF = copy(table_gvf_voronoi_10m)
)

# Format NDVI to three decimals and GVF to one decimal.
for (sheet_name in names(tables_export_voronoi)) {
  seasonal_table <- tables_export_voronoi[[sheet_name]]
  
  ndvi_columns <- grep("NDVI|^[0-9]{4}-", names(seasonal_table), value = TRUE)
  if (sheet_name != "GVF") {
    if (sheet_name == "Pooled") {
      ndvi_columns <- c("Maximum_NDVI", "Mean_NDVI")
    }
    seasonal_table[, (ndvi_columns) := lapply(.SD, round, 3),
                   .SDcols = ndvi_columns]
  }
  
  if (sheet_name == "GVF") {
    season_columns <- setdiff(names(seasonal_table), "distance_bin_10m")
    seasonal_table[, (season_columns) := lapply(.SD, round, 1),
                   .SDcols = season_columns]
  }
  
  if (sheet_name == "Pooled") {
    seasonal_table[, GVF := round(GVF, 1)]
  }
}

# Export all four tables to one Excel workbook.
writexl::write_xlsx(
  tables_export_voronoi,
  "ramthal_voronoi_tables_10m.xlsx"
)

# Open the folder containing the Excel file.
shell.exec(getwd())








# Compare pooled vegetation indicators across the two methods. -=====================
library(data.table)

comparison_vegetation_methods <- merge(
  ramthal_by_distance[
    , .(
      distance_m,
      Buffer_Maximum_NDVI = Maximum_NDVI,
      Buffer_Mean_NDVI = Mean_NDVI,
      Buffer_GVF = GVF * 100
    )
  ],
  ramthal_voronoi_pooled_10m[
    , .(
      distance_m = distance_bin_10m,
      Voronoi_Maximum_NDVI = Maximum_NDVI,
      Voronoi_Mean_NDVI = Mean_NDVI,
      Voronoi_GVF = GVF
    )
  ],
  by = "distance_m"
)

# Calculate differences between methods at each distance.
comparison_vegetation_methods[
  , `:=`(
    Difference_Maximum_NDVI = Buffer_Maximum_NDVI - Voronoi_Maximum_NDVI,
    Difference_Mean_NDVI = Buffer_Mean_NDVI - Voronoi_Mean_NDVI,
    Difference_GVF = Buffer_GVF - Voronoi_GVF
  )
]

# Summarize agreement across all 75 distance bands.
summary_vegetation_methods <- data.table(
  Indicator = c("Maximum NDVI", "Mean NDVI", "GVF (%)"),
  Mean_Absolute_Difference = c(
    mean(abs(comparison_vegetation_methods$Difference_Maximum_NDVI)),
    mean(abs(comparison_vegetation_methods$Difference_Mean_NDVI)),
    mean(abs(comparison_vegetation_methods$Difference_GVF))
  ),
  Maximum_Absolute_Difference = c(
    max(abs(comparison_vegetation_methods$Difference_Maximum_NDVI)),
    max(abs(comparison_vegetation_methods$Difference_Mean_NDVI)),
    max(abs(comparison_vegetation_methods$Difference_GVF))
  ),
  Correlation = c(
    cor(comparison_vegetation_methods$Buffer_Maximum_NDVI,
        comparison_vegetation_methods$Voronoi_Maximum_NDVI),
    cor(comparison_vegetation_methods$Buffer_Mean_NDVI,
        comparison_vegetation_methods$Voronoi_Mean_NDVI),
    cor(comparison_vegetation_methods$Buffer_GVF,
        comparison_vegetation_methods$Voronoi_GVF)
  )
)

print(round(summary_vegetation_methods[, -1], 4))
print(summary_vegetation_methods)


library(data.table)
library(fixest)

# --------------------------------------------------
# 1. Load the original Fixed-Radius Buffers database.
# Load only the columns required for regression.
# --------------------------------------------------

ramthal_buffer_regression_data <- fread(
  ramthal_buffer_output,
  select = c(
    "NDVI_max",
    "NDVI_mean",
    "distance_m",
    "season",
    "candidate_id"
  )
)

# --------------------------------------------------
# 2. Create the GVF binary outcome.
# 1 = Maximum NDVI >= 0.36; 0 = below 0.36.
# OLS on this outcome is a Linear Probability Model.
# --------------------------------------------------

ramthal_buffer_regression_data[
  , GVF_binary := as.integer(NDVI_max >= 0.36)
]

# --------------------------------------------------
# 3. Define three regression specifications.
#
# Model 1: Distance only.
# Model 2: Distance + season fixed effects.
# Model 3: Distance + season and outlet fixed effects.
#
# CORRECTION:
# Cluster standard errors by water outlet during
# estimation, rather than using IID standard errors.
# This accounts for within-outlet dependence.
# --------------------------------------------------

ramthal_buffer_regression_specs <- c(
  Simple = " ~ distance_m",
  Season = " ~ distance_m | season",
  Season_Outlet = " ~ distance_m | season + candidate_id"
)

# --------------------------------------------------
# 4. Estimate all nine regressions.
# Use the same three specifications for each outcome.
# --------------------------------------------------

ramthal_buffer_regression_models <- list()

for (outcome in c("NDVI_max", "NDVI_mean", "GVF_binary")) {
  
  ramthal_buffer_regression_models[[outcome]] <- lapply(
    ramthal_buffer_regression_specs,
    function(specification) {
      
      feols(
        as.formula(paste0(outcome, specification)),
        data = ramthal_buffer_regression_data,
        vcov = ~candidate_id
      )
    }
  )
}

# --------------------------------------------------
# 5. Display regression tables.
#
# CORRECTION:
# Report actual p-values instead of significance stars.
# Standard errors are already clustered by outlet.
# --------------------------------------------------

etable(
  ramthal_buffer_regression_models[["NDVI_max"]],
  coefstat = "pvalue",
  signif.code = NA,
  digits = 6
)

etable(
  ramthal_buffer_regression_models[["NDVI_mean"]],
  coefstat = "pvalue",
  signif.code = NA,
  digits = 6
)

etable(
  ramthal_buffer_regression_models[["GVF_binary"]],
  coefstat = "pvalue",
  signif.code = NA,
  digits = 6
)

# --------------------------------------------------
# 6. Release the large dataset from R memory.
# All nine estimated models remain available.
# --------------------------------------------------

rm(ramthal_buffer_regression_data)
gc()

# Save the three regression model objects.
saveRDS(ramthal_buffer_models_maximum, "ramthal_buffer_models_maximum.rds")
saveRDS(ramthal_buffer_models_mean, "ramthal_buffer_models_mean.rds")
saveRDS(ramthal_buffer_regression_models, "ramthal_buffer_regression_models.rds")

shell.exec(getwd())
getwd()

# Remove them from memory only after successful saving.
rm(
  ramthal_buffer_models_maximum,
  ramthal_buffer_models_mean,
  ramthal_buffer_regression_models
)

gc()

# END ----












