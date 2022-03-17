library(tidyverse)
library(extrafont)
extrafont::loadfonts(device="win")
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

windowsFonts()

windowsFonts(A = windowsFont("Times New Roman"))  

library(showtext) 
library (extrafontdb)
library(raster)
library(sf)
library(rgdal)
library(sp)

#  Ramthal_East  crop map GIS ----
jain_171819 <- jain_789F%>%
  dplyr::select("id_yoav","village","cropCat","area_ha","season")
rm(jain_789F)

jain_171819[,1] %>% distinct()      # 1980id id_yoav
jain_171819[,c(1,5)] %>% distinct() # 5553id id_yoav,season
jain_171819[,2] %>% distinct()      # 33 villages
ramthal_east@data                   # 7274/3637id
rmtl                                 #1850id

### ### ### ### # ###

Ramthal_East <- readOGR("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Ramthal_East")
ramthal_east <- Ramthal_East
ramthal_east@data <- rename(ramthal_east@data, id_yoav = id)

summary(ramthal_east@data)
plot(ramthal_east)

ramthal_data <- ramthal_east@data

library(broom)
ramthal_east_tidy <- tidy(ramthal_east)

ramthal_east$id <- row.names(ramthal_east)
ramthal_east_tidy2 <- left_join(ramthal_east_tidy, ramthal_east@data)

write.csv(ramthal_east_tidy2, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/ramthal_east_tidy2.csv", row.names=FALSE)
rm(ramthal_east_tidy,ramthal_data)

#### jain_common_crop ####
jain_common_crop <- 
  jain_171819 %>%  group_by(id_yoav,village, season,cropCat) %>%summarise(area_ha=sum(area_ha)) %>% 
  group_by(id_yoav,season) %>% arrange(desc(area_ha)) %>% 
  group_by(id_yoav,season)%>%slice(1)

write.csv(jain_common_crop, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/jain_common_crop.csv", row.names=FALSE)

600/500

jain_common_crop %>% filter(season == "rabbi_2017") %>% right_join(ramthal_east_tidy2) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cropCat)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(x = " ",y = " ",fill = " ")+
  theme_minimal()+
  scale_fill_manual(values=group.colors)

group.colors <- c(Cereals="tan3",
                  Oilseeds= "khaki1",
                  Pulses= "indianred3",
                  Vegetables= "olivedrab3",
                  Others= "lightskyblue3")
    
# kharif_2019 
jain_common_crop %>% filter(season == "kharif_2019") %>% right_join(ramthal_east_tidy2) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cropCat)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  #  labs(title="Ramthal Micro Irrigation-East Area, Karnataka ",subtitle = "Rabbi 2017")+
  labs(x = " ",y = " ",fill = " ")+
  theme_minimal()+
  theme(text = element_text(family = "A"))+
  scale_fill_manual(breaks = c("Cereals","Pulse","Vegetables","Oilseeds","Others"), 
                       values=c("tan3","indianred3" ,"olivedrab3" ,"khaki1","lightskyblue3"))

#### jain_highvalue_crop ####
jain_highvalue_crop <- 
  jain_171819 %>%  group_by(id_yoav,village, season,cropCat) %>%summarise(area_ha=sum(area_ha)) %>% 
  filter(cropCat %in% c("Vegetables","Oilseeds")) %>% 
  group_by(id_yoav,season) %>% arrange(desc(area_ha)) %>% 
  group_by(id_yoav,season)%>%slice(1)

write.csv(jain_highvalue_crop, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/jain_highvalue_crop.csv", row.names=FALSE)

# kharif_2019 
jain_highvalue_crop %>% filter(season == "kharif_2019") %>% right_join(ramthal_east_tidy2) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cropCat)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  #  labs(title="Ramthal Micro Irrigation-East Area, Karnataka ",subtitle = "Rabbi 2017")+
  labs(x = " ",y = " ",fill = " ")+
  theme_minimal()+#theme(legend.position="none")+ 
  theme(text = element_text(family = "A"))+
  scale_fill_manual(breaks = c("Vegetables","Oilseeds"), 
                    values=c("olivedrab3" ,"khaki1"))


#Animated Plots with R: https://mikeyharper.uk/animated-plots-with-r/       








