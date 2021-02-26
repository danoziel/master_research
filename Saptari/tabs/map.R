library(leaflet)
library(rgdal)
library(dplyr)
# janakpur 26.847452243592766, 85.94778350761003
p_lon <- 26.84745
p_lat <- 85.94778

leaflet() %>%
  setView(lng = p_lat, lat = p_lon, zoom = 8.4) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = GPS_of_53_sites,
#   radius = sqrt(10^GPS_of_53_sites$plot_acre) *10,
    color = "#008B00",
    fillColor = "#008B00",
    fillOpacity = 0.2,
    popup = paste0(
      "<GPS_of_53_sites>Farmer Name: </GPS_of_53_sites>", GPS_of_53_sites$name_of_respondent, "<br>",
      "<GPS_of_53_sites>District: </GPS_of_53_sites>", GPS_of_53_sites$District, "<br>",
      "<GPS_of_53_sites>VDC: </GPS_of_53_sites>", GPS_of_53_sites$VDC, "<br>"
    ))
