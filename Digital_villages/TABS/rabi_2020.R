library(readxl)
rabi_2020_2021_wheat <- read_excel("DATA/rabi_2020_2021_wheat.xlsx") 

plot_area_gps1 <- Plot.Size_WIDE %>%  select(farmer_id,farmer_name,plot_area_gps) 

x1 <- left_join(rabi_2020_2021_wheat,plot_area_gps1)

x2 <- Plot.Size_WIDE %>% filter(farmer_id %in% c(28,30,43,57,69,60,71)) %>% 
  select(farmer_id,farmer_name,plot_area_gps)

rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "tarri singh" ] <- "Tarii singh"
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "Jarnail Singh" ] <- "Jarnail singh"
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "Lakhwinder singh" ] <- "Lakhvinder Singh"
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "pala singh" ] <-"Pala singh" 
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "Pargat singh" ] <- "Parget singh"
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "jeet singh" ] <- "Jeet singh"
rabi_2020_2021_wheat$farmer_name[rabi_2020_2021_wheat$farmer_name == "satpal singh" ] <- "Satpal singh"
  
x3 <- left_join(rabi_2020_2021_wheat,plot_area_gps1)

x4 <- x3 %>% filter(!is.na(farmer_id) ,is.na(plot_area_gps))

x5 <- Seasonal.Survey_WIDE %>%
  filter(farmer_name %in% c("Bira singh","Dara singh","Gagla singh","Kuldeep maan",
                            "Lakhvinder singh","Randher singh","Satnam singh",
                            "Preet Singh","Tarsem singh") ) %>% 
  select(duration,starttime,farmer_id,farmer_name,plot_area_gps)
x5 <- x5 %>% filter(!farmer_id %in% c(44,43,78)) %>% 
  filter(!duration %in% c(605,766)) %>% 
  select(-c(1,2,farmer_name))

x6 <- left_join(x3,x5,by="farmer_id") %>% rename(plot_area_gps_x=plot_area_gps.x,plot_area_gps_y=plot_area_gps.y)
x <- x6 %>% coalesce(plot_area_gps_x, plot_area_gps_y)

write.csv(x6, file = "C:/Users/Dan/Documents/R/Digital_villages/DATA/x6.csv", row.names=FALSE)
