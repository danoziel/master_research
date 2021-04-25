write.csv(days_use_hh, file = "C:/Users/Dan/Documents/R/Rautahat.Bara.Sarlahi/data/Agriculture_18_19.csv", row.names=FALSE)
write.csv(days_use_hh, file = "C:/Users/Dan/Documents/master_research/DATAs/data_saptari/days_use_hh.csv", row.names=FALSE)

# Save the data in different vector ----
women_weight <- genderweight %>%
  filter(group == "F") %>%
  pull(weight)

# t test ----
res <- 
  t.test(total_litres_consumed_dieselkero ~ TreatmentControl, data = df.match)

#regression
ggplot(income.data, aes(x=income, y=happiness))+ geom_point()

income.happiness.lm <- lm(happiness ~ income, data = income.data)

summary(income.happiness.lm)

#  NA----
#in full dataset - dplyr
rowwise() %>% 

drop_na(irri_for_season,season_of_crop)%>% library(tidyverse)

#replace NA to 0
x$land_cult [is.na(x$land_cult)] <- 0 

#replace NaN to Na, x=column
mutate(x= sub(NaN, NA, x)) %>%
  
#renam column----
R_intensity_Baseline %>% 
  rename(net_cropped_area = land_for_cultivation) #renam column land_ TO net_

# copy column----
R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)
#subset----
Treats_Lands <- subset(R.Lands_Endline_EPC_2019_,  TC == 1) 
#Controlling number of decimal digits----
mutate(across(is.numeric, round, 2)) 


# freq by % symbol ----
mutate(freq = paste0(round(100 * total_land_cultivated/nca, 0), "%"))

mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

mutate(label_percent()(x))
 
#replace 2 to 1 ----
#replace 2 to 1 
Data$TC[Data$TC == 2] <- 1 

R_Lands_I_Baseline_2018_[6,3]<- 40
#from rows to column ----
Q1 <- spread(Q1, oslosp, freq) 

#split column to two - ifelse----
at_btselem <- at_btselem %>% mutate(total_events_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(total_events_located_yesha  = ifelse(location == 2, "1",NA)) 
#location- old, total_events_located_IL-new


colMeans(land_Treats)

mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),#mean per row - by defined columns
       
       #-----  frequency in percentage   ----    
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         group_by(practice_aquaculture,TreatmentControl) %>%summarise(n=n()) %>% 
         mutate(freq = n / sum(n)) #Percentage per sub group
       
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture,TreatmentControl) %>%mutate(freq = n / sum(n))#Percentage per group
       
       #   R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
       #     group_by(practice_aquaculture)%>%summarise(n=n()) %>%mutate(freq = n / sum(n))
       #                                 _||_
       #                                 \  /
       #                                  \/
       R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture) %>%mutate(freq = n / sum(n))
       
# ---------------------------------
library(data.table)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")
  
date <- seq(as.IDate("1994-01-01"), as.IDate("2019-12-31"), 1))
dt <- data.table(date=date)


summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL"), sum, na.rm = TRUE)
       
peace_index_17_18 <- peace_index_17_18 %>%
  mutate(political_spectrum=case_when(
    party %in% c(2,3,4,5,7,9,10)~1,
    party %in% c(14:24,27:30)~4))

library(extrafont)
theme_minimal() +
  theme(text = element_text(family = "Georgia"))

# from: 1899-12-31 09:00:00 TO 9:00
water01$`Irrigation Start Time` <- format(water01$`Irrigation Start Time`, format="%H:%M")

# add_column in dplyr----
df %>% add_column(new_col = 0)

https://www.listendata.com/2017/03/if-else-in-r.html


#map----
library(leaflet)
library(rgdal)
library(dplyr)

p_lon <- 29.9018696
p_lat <- 76.0084641

leaflet() %>%
  setView(lng = p_lat, lat = p_lon, zoom = 13.5) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = kharif_2020_rice,
    radius = sqrt(10^kharif_2020_rice$plot_acre) *10,
    color = "#008B00",
    fillColor = "#008B00",
    fillOpacity = 0.2,
    popup = paste0(
      "<kharif_2020_rice>Farmer Name: </kharif_2020_rice>", kharif_2020_rice$farmer_name, "<br>",
      "<kharif_2020_rice>Plot Size (acre): </kharif_2020_rice>", kharif_2020_rice$plot_acre, "<br>",
      "<kharif_2020_rice>Farmer ID: </kharif_2020_rice>", kharif_2020_rice$farmer_id, "<br>"
    ))


## tab_model for two models ----
tab_model(model11,model13,digits=3,p.style="stars", show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))


