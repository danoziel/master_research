26.734950834599527, 85.9272131400479

# ----nasapower--------------------------------------------
library(nasapower)
ag_d <- get_power(
  community = "SSE",
  lonlat = c(85.92, 26.73),
  pars = c( "KT","CLRSKY_SFC_SW_DWN","ALLSKY_SFC_SW_DWN", "PRECTOT"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY"
)



library(weatherData)
data_okay <- checkDataAvailability("HECA", "2014-01-01")



library(weatherr)
ggele(lat=0,lon=0, output=c('elevation','elevation/resolution','all'),key=NULL)
