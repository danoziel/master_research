library(tidyverse)
library(kableExtra)
library(formattable)
library(tableone)
library(gridExtra)
library(scales)
library(extrafont)

Babbu singh 59        -

amrik Singh  1        -
Bira singh  10        -
Dara Singh 13         +
Kuldeep Singh 40      -
Lakhvinder Singh 45   -
Randhir Singh 68      -
Preet Singh 75         +

Gurmail Singh 22   +
Jaga singh 26     -
Jarnail Singh 31  -
Kashmir Singh 34  -
Kewal Singh 37    +

xx5 <- Plot.Size_WIDE %>% filter(farmer_id %in% c(31,22,26,34,37)) %>% 
  select(3,11,12,14,13) %>% mutate(source= "Plot_Size")

xx8 <- Seasonal.Survey_WIDE %>% 
  filter(farmer_id %in% c(1,10,13,40,45,68,75)) %>% 
  filter(!farmer_name %in% c("", "Dara Singh","Kuldeep man","Preet") )%>% 
  filter(endtime != "06:32:25 02/12/2019") %>% 
  select(3,11,12,15,14) %>% mutate(source= "Seasonal_survey")

xx13 <- rbind(xx5,xx8) %>%
  rename(date=endtime) %>% 
  mutate(plot_acre= plot_area_calc* 0.0002471) %>%
  mutate(across(is.numeric,round,2)) %>% arrange(farmer_id)

xx13$latitude <- c(29.8986034,29.9017768,29.9013548,29.9079074,29.9030711,
                   29.901966,29.90677,29.9003733,29.9046129,29.8986103,
                   29.9002604,29.9008075,29.9030137)
xx13$longitude <- c(76.0168688,76.0174902,76.0163402,76.0183801,75.9996337,
                    75.9982941 ,76.0206789 ,75.996107 ,76.0163811 ,76.0194806,
                    76.0162191 ,76.0155688 ,76.0162917 )
xx13 <- xx13[,c(1:4,7:9,6)]


xx13 <- xx13 %>% filter(farmer_name != "Balbir singh")

#-------------------------#----
#produce_price_yesno - Did you sell all your produce at the same price?
#average_price -       What was the price for 1 kg (Rs) (average)?
  
paddy_kharif_yield_11_20 <- 
  Weekly_Farmer_12_11_20 %>% 
  filter(starttime < "09/11/2020" ) %>% 
  select(starttime,farmer_id,farmer_name,harvest_KG,
         KG_sold,selling_options,trasportation_cost,produce_price_yesno,
         average_price,revenue) %>% mutate(harvest_KG*average_price)

harvest_paddy_kharif_2020 <- Weekly_Farmer_12_11_20 %>% 
  filter(starttime < "09/11/2020" ) %>% 
  select(starttime,farmer_id,farmer_name,harvest_KG) %>% 
  rename(survey_date = starttime) #renam column land_ TO net_

write.csv(harvest_paddy_kharif_2020,"C:/Users/Dan/Documents/R/Digital_villages/DATA/harvest_paddy_kharif_2020.csv", row.names = FALSE)






  
