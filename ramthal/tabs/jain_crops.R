# library() ----
library(tidyverse)
library(extrafont)
library(dplyr)
library(kableExtra)
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

# jain colors ----

# kharif         "cadetblue4" (Monsoon) july-October
# rabbi  (winter)  october -february   "cadetblue3"    
# zaid   (Summer)  maech - june        "#E1B378"

#  Ramthal_East  crop map GIS ----
jain_171819 <- jain_789F%>%
  dplyr::select("id_yoav","village","cropCat","area_ha","season")
rm(jain_789F)

write.csv(jain_171819, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/jain_171819.csv", row.names=FALSE)


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







### Farmers who grow high-value crops frequency ----

farmers_per_season <- 
  jain_171819 %>% select(id_yoav,season) %>%
  distinct() %>% count(season) %>% rename(`Total farmers sampled`=n)

group.colorsII <- c(kharif_2017="cadetblue4",kharif_2018= "cadetblue4",kharif_2019= "cadetblue4",
                    rabbi_2017= "cadetblue3", rabbi_2018= "cadetblue3", rabbi_2019= "cadetblue3")

# Vegetables & Oilseed 

jain_171819 %>% 
  filter(cropCat %in% c("Vegetables", "Oilseeds")) %>% 
  dplyr :: select(-area_ha) %>% 
  distinct() %>% 
  group_by(season) %>%summarise(hvc=n()) %>% 
  inner_join(farmers_per_season) %>% 
  mutate(freq = hvc /n) %>% 
  mutate(prt = paste0(round(100 * hvc /n, 0), "%")) %>% 
  select(1,2,3,5) %>% 
  kable() %>%
  kable_styling(latex_options = "striped") %>% 
  row_spec(1:3, background = 'aliceblue') 


  
  
  jain_171819 %>% 
  filter(cropCat %in% c("Vegetables", "Oilseeds")) %>% 
  dplyr :: select(-area_ha) %>% 
  distinct() %>% 
  group_by(season) %>%summarise(hvc=n()) %>% 
  inner_join(farmers_per_season) %>% 
    mutate(freq = hvc /n) %>% 
    mutate(prt = paste0(round(100 * hvc /n, 0), "%")) %>% 
    
    ggplot() +
    geom_bar(aes(y = freq, x = season, fill = season), stat="identity")+
    scale_fill_manual(values=group.colorsII)+
    labs(x = " ",y = "% of farmers growing\n vegetables and oilseeds")+
    theme_minimal()+theme(legend.position="none")
  
  
  
  
# Vegetables 
jain_171819 %>% 
    filter(cropCat == "Vegetables") %>% 
    dplyr :: select(-area_ha) %>% 
    distinct() %>% 
    group_by(season) %>%summarise(hvc=n()) %>% 
    inner_join(farmers_per_season) %>% 
    mutate(freq = hvc /n) %>%
  mutate(prt = paste0(round(100 * hvc /n, 0), "%")) %>% 
    select(1,2,3,5) %>% 
    kable() %>%
    kable_styling() %>% 
    row_spec(1:3, background = 'aliceblue') 
  
jain_171819 %>% 
    filter(cropCat == "Vegetables") %>% 
    dplyr :: select(-area_ha) %>% 
    distinct() %>% 
    group_by(season) %>%summarise(hvc=n()) %>% 
    inner_join(farmers_per_season) %>% 
    mutate(freq = hvc /n) %>%
    ggplot() +
    geom_bar(aes(y = freq, x = season, fill = season), stat="identity")+
    scale_fill_manual(values=group.colorsII)+
    labs(x = " ",y = "% of farmers growing\n vegetables")+
    theme_minimal()+theme(legend.position="none")



