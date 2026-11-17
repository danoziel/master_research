library(readr)
library(terra)

ramthal_candidate_coordinates <- read_csv(
  "C:/Users/Dan/OneDrive/ramthal_ndvi/ramthal_candidate_coordinates.csv"
)

# Display the column names and first six records.
names(ramthal_candidate_coordinates)
head(ramthal_candidate_coordinates)

# Convert water outlet coordinates into spatial points (WGS84).
ramthal_filters <- terra::vect(
  ramthal_candidate_coordinates,
  geom = c("longitude_WGS84", "latitude_WGS84"),
  crs = "EPSG:4326"
)

# Transform coordinates to meters (UTM Zone 43N).
ramthal_filters <- terra::project(ramthal_filters, "EPSG:32643")

# Verify the number of water outlets and coordinate system.
nrow(ramthal_filters)
terra::crs(ramthal_filters)


# Define the 12 Rabi seasons.
ramthal_seasons <- data.frame(
  season = paste0(2014:2025, "-", 2015:2026),
  start_date = as.Date(paste0(2014:2025, "-11-01")),
  end_date = as.Date(paste0(2015:2026, "-04-01")),
  period = c(rep("Before", 2), "Transition", rep("After", 9))
)

# Display the season definitions.
print(ramthal_seasons)


# Check  ----

library(data.table)

ramthal_landsat_2014_2015 <- fread(file.choose())

# Check the number of records and columns.
dim(ramthal_landsat_2014_2015)

# Check variable names and first records.
names(ramthal_landsat_2014_2015)
head(ramthal_landsat_2014_2015)

# Verify outlet coverage and distance range.
uniqueN(ramthal_landsat_2014_2015$outlet_id)
range(ramthal_landsat_2014_2015$distance_m)




library(data.table)

ramthal_files <- list.files(
  "C:/Users/Dan/OneDrive/Ramthal_Landsat",
  pattern = "\\.csv$",
  full.names = TRUE
)

ramthal_landsat_database <- rbindlist(lapply(ramthal_files, fread))

dim(ramthal_landsat_database)
table(ramthal_landsat_database$season)












# Calculate binary GVF for each pixel.
ramthal_landsat_database[, GVF := as.integer(NDVI_max >= 0.36)]

# Assign pixels to 30 m distance intervals.
ramthal_landsat_database[, distance_bin_30m :=
                           pmin(floor(distance_m / 30) * 30, 720)]

# Create seasonal vegetation summaries.
ramthal_landsat_seasonal_30m <- ramthal_landsat_database[
  ,
  .(
    Maximum_NDVI = mean(NDVI_max, na.rm = TRUE),
    Mean_NDVI = mean(NDVI_mean, na.rm = TRUE),
    GVF = mean(GVF, na.rm = TRUE) * 100,
    N = .N
  ),
  by = .(season, distance_bin_30m)
][order(season, distance_bin_30m)]

# Verify the resulting table.
dim(ramthal_landsat_seasonal_30m)
head(ramthal_landsat_seasonal_30m)






library(ggplot2)

# Use the established colors for the three indicators.
colors_vegetation_indicators <- c(
  Maximum_NDVI = "#1F78B4",
  Mean_NDVI    = "#74B9E6",
  GVF          = "#2E7D32"
)


plot_landsat_seasons <- function(indicator, title, y_label) {
  
  ggplot(
    ramthal_landsat_seasonal_30m,
    aes(x = distance_bin_30m + 15, y = .data[[indicator]])
  ) +
    geom_line(
      color = colors_vegetation_indicators[indicator],
      linewidth = 0.85
    ) +
    facet_wrap(~ season, ncol = 3, scales = "free_y") +
    scale_x_continuous(
      breaks = c(0, 250, 500, 750),
      limits = c(0, 750),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = 0.08)
    ) +
    labs(
      title = title,
      x = "Distance from water outlet (m)",
      y = y_label
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      plot.title = element_text(
        face = "bold", size = 14, hjust = 0.5,
        margin = margin(b = 18)
      ),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = grid::unit(1.3, "lines"),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 11)
    )
}

# Maximum NDVI
plot_landsat_seasons(
  "Maximum_NDVI", "Maximum NDVI by Rabi Season", "Maximum NDVI"
)

# Mean NDVI
plot_landsat_seasons(
  "Mean_NDVI", "Mean NDVI by Rabi Season", "Mean NDVI"
)

# Green Vegetation Fraction
plot_landsat_seasons(
  "GVF", "GVF by Rabi Season", "GVF (%)"
)










# Descriptive statistics for the two pre-project seasons.
ramthal_before_table <- ramthal_landsat_database[
  season %in% c("2014-2015", "2015-2016"),
  .(
    Maximum_NDVI = round(mean(NDVI_max), 3),
    Mean_NDVI = round(mean(NDVI_mean), 3),
    GVF = round(mean(GVF) * 100, 1)
  ),
  by = .(
    season,
    distance_band = cut(
      distance_m,
      breaks = c(0, 150, 300, 450, 600, Inf),
      labels = c("0–150", "150–300", "300–450",
                 "450–600", "600–750"),
      include.lowest = TRUE
    )
  )
][order(season, distance_band)]

print(ramthal_before_table)










library(fixest)

# Select the two pre-project seasons.
ramthal_baseline <- ramthal_landsat_database[
  season %in% c("2014-2015", "2015-2016")
]

# Estimate OLS models for each vegetation indicator.
ramthal_baseline_models <- lapply(
  c("NDVI_max", "NDVI_mean", "GVF"),
  function(indicator) {
    
    formula_single <- as.formula(
      paste(indicator, "~ distance_m")
    )
    
    formula_combined <- as.formula(
      paste(indicator, "~ distance_m | season")
    )
    
    list(
      Before_2014_2015 = feols(
        formula_single,
        data = ramthal_baseline[season == "2014-2015"],
        vcov = ~outlet_id
      ),
      Before_2015_2016 = feols(
        formula_single,
        data = ramthal_baseline[season == "2015-2016"],
        vcov = ~outlet_id
      ),
      Before_Combined = feols(
        formula_combined,
        data = ramthal_baseline,
        vcov = ~outlet_id
      )
    )
  }
)

names(ramthal_baseline_models) <- c(
  "Maximum_NDVI", "Mean_NDVI", "GVF"
)

# Display coefficients and exact p-values.
for (indicator in names(ramthal_baseline_models)) {
  cat("\n", indicator, "\n")
  print(etable(
    ramthal_baseline_models[[indicator]],
    coefstat = "pvalue",
    signif.code = NA,
    digits = 6
  ))
}

