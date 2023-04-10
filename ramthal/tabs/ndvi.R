https://ourcodingclub.github.io/tutorials/spatial/
  
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)


# .tif file ----
setwd("C:Users/Dan/Documents/master_research/DATAs")
# Load data
tay <- raster('taycrop.tif')

# Get properties of the Tay raster
tay

b1 <- raster('taycrop.tif', band=1)
b2 <- raster('taycrop.tif', band=2)
b3 <- raster('taycrop.tif', band=3)
b4 <- raster('taycrop.tif', band=4)
b5 <- raster('taycrop.tif', band=5)
b6 <- raster('taycrop.tif', band=6)
b7 <- raster('taycrop.tif', band=7)
b8 <- raster('taycrop.tif', band=8)
b9 <- raster('taycrop.tif', band=9)
b10 <- raster('taycrop.tif', band=10)
b11 <- raster('taycrop.tif', band=11)
b12 <- raster('taycrop.tif', band=12)


compareRaster(b2, b3)

plot(b8)

image(b8)

plot(b8)
zoom(b8)    # run this line, then click twice on your plot to define a box

# .nc file ----














