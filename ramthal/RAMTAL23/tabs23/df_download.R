write.csv(rmtl_srvy22, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_srvy22.csv")
write.csv(rmtl_midline2018, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_midline2018.csv")
write.csv(rmtl_baseline2016, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.csv")


library(openxlsx)
write.xlsx(x=rmtl_srvy22,file="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_srvy22.xlsx",sheetName="survey",rowNames=F, overwrite =T)
write.xlsx(x=rmtl_midline2018,file="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_midline2018.xlsx",sheetName="survey",rowNames=F, overwrite =T)
write.xlsx(x=rmtl_baseline2016,file="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_baseline2016.xlsx",sheetName="survey",rowNames=F, overwrite =T)



library(sf)
# Export centroids_sf as an ESRI Shapefile into your OneDrive folder
st_write(centroids_sf,"C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp",driver     = "ESRI Shapefile",delete_dsn = T)

st_write(centroids_sf, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.gpkg",delete_dsn = TRUE)
st_write(centroids_sf,"C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids.geojson",delete_dsn = TRUE)

write.csv(centroids_coords, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_coords.csv")



write.csv(a_irri_rain_method, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_irri_rain_method.csv")
write.csv(a_plots_revenue, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_revenue.csv")
write.csv(a_plots_size, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_size.csv")
write.csv(a_source_irri, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_source_irri.csv")
write.csv(irri_acre_plotID, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/irri_acre_plotID.csv")
write.csv(plots_crop_2022, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/plots_crop_2022.csv")
write.csv(plots_crop_yield_2022, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/plots_crop_yield_2022.csv")

write.csv(ml18_irri_methods, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ml18_irri_methods.csv")

write.csv(BL_2015_16_crop_IRsource_IRmethod, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/BL_2015_16_crop_IRsource_IRmethod.csv")
write.csv(bl_hh_irrigate, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl_hh_irrigate.csv")
write.csv(bl_ir_method, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl_ir_method.csv")
write.csv(bl28_irri_plot_season, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl28_irri_plot_season.csv")
write.csv(bl6_plotAcre, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl6_plotAcre.csv")

# yield22_acre
write.csv(yield22_acre, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/yield22_acre.csv")

# shp_index22
write.csv(shp_index22, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/shp_index22.csv")

write.csv(shapfile_rmtl_2022, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/shapfile_rmtl_2022.csv")

list_crop
write.csv(list_crop, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/list_crop.csv")

write.csv(control_vars, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/control_vars.csv")
write.csv(rmtl_con_vars, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_con_vars.csv")
write.csv(irrigation_BL_to_22, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/irrigation_BL_to_22.csv")
write.csv(irrigation_HH, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/irrigation_HH.csv")
write.csv(dist_var, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/dist_var.csv")

# a_plots_size
write.csv(a_plots_size, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/a_plots_size.csv")
land_holding_2022 # in impact2.R

# bl6_plotAcre
write.csv(bl6_plotAcre, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/bl6_plotAcre.csv")
land_holding_2016 # in impact2.R















irrigation_BL_to_22 <- read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/irrigation_BL_to_22.csv")
