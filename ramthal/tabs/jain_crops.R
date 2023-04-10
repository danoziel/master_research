
library(tidyverse)
ji_261122
write.csv(ji_261122, file = "C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/bji_261122.csv", row.names=FALSE)
library(readr)
t <- read_csv("~/master_research/DATAs/ramthal_data/bji_261122.csv")
View(bjii_261122)

# library() -----
library(haven)
shape_code <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/shape_code.dta")
library(extrafont)
library(dplyr)
library(kableExtra)

# jain_kharif_2017_kharif_2022 ----
jain_kharif_2017_kharif_2022 %>% 
  select(si_number,farmer_name,crop,survey_hissa,area_ha) %>%
  summarise_all(~sum(is.na(.)))
  
aa <- jain_kharif_2017_kharif_2022
aa$season[aa$season=="rabbi_2017"] <- "rabi_2017"
aa$season[aa$season=="rabbi_2018"] <- "rabi_2018"
aa$season[aa$season=="rabbi_2019"] <- "rabi_2019"

# clean DATA ----
ab <- 
  aa %>% 
  filter(!is.na(si_number)|!is.na(farmer_name)| # clean excel rows of TOTAL LAND per farmer
           !is.na(zone)|!is.na(block)|!is.na(village)|
           !is.na(survey_hissa)|!is.na(crop)) %>% 
  fill(farmer_name, .direction = "downup") %>% # copy farmer_name to NA below
  filter(!is.na(crop))

ac <-  
  ab %>% # separate CROP to diff rows
  separate_rows(crop, sep = " & ") %>% 
  separate_rows(crop, sep = "\\+") %>% 
  separate_rows(crop, sep = ",") %>% 
  separate_rows(crop, sep = "/") %>%  
  separate_rows(crop, sep = "\\.")%>% 
  mutate( id_si = dense_rank(farmer_name)) # numbering farmer_name

ac$id_si <- str_pad(ac$id_si, 5, pad = "0") #add 0000 to id_si
ac$id_si <- paste("f", ac$id_si, sep = "") #add f to id_si
  

#rename CROP ----

ad <-  
  ac %>%
  mutate(
    cropx = case_when(
      crop %in% c(" Corender","Corender","Coriandor","Coriendor","Corinder","Corriander","Corainder","coriander","Coriander") ~ "Coriander",
      crop %in% c("Chilli-badagi", "Chillly","chilly","Chilly","Chilli") ~ "Chili"
      crop %in% c("Wheat","wheat") ~ "Wheat",
      crop %in% c("Veg","Vegetable"  ) ~   "Vegetables"   , 
      crop %in% c( "SF", "Snfwr"," Sunflower" ) ~"Sunflower" , 
      crop %in% c("Sugacane","Sugar" ) ~"Sugarcane",
      crop %in% c("Soyabean") ~"Soybean",
      crop %in% c("Sesam","Sesame","Yallu") ~"Sesamum",
      crop %in% c("Sericulture" ) ~"Cotton",   
                  
      crop %in% c("RD","Redgram","RG"," Redgram","redgram") ~"Red_gram(Pigeon pea)",
      crop %in% c("pumpkin") ~"Pumpkin",
      crop %in% c("onion") ~"Onion",
      crop %in% c("Mulberry" ) ~"Mulberry(Morus)",#petel
      crop %in% c("maize", " Maize","Maize-Sandoge") ~"Maize",
      crop %in% c("jowar", "Jowar","JOwar","jwr","Jwr" ) ~"Jowar(sorghum)",#dura
      crop %in% c("Havaij" ) ~"Hawaij",
      crop %in% c("Groundnut","Groundut") ~ "Groundnut(Peanut)",
      crop %in% c("GG" ,"Gm","greengram","Greengram"," Greengram") ~"Green_gram(Mung bean)",
      crop %in% c("Cocumber", "Gherkin") ~"Cucumber" ,
      crop %in% c("foxtail Millet","Foxtail millet","Foxtail Millet","Foxtailmillet","Foxtail") ~"Foxtail_millet",
      crop %in% c("Bhendi") ~"Okra",
      crop %in% c("BG","bgm","Bgm","bengalgram","Bengalgram","Bg" ) ~"Bengal_gram(Chickpea)",
      crop %in% c("Bajara","Bajra" ) ~"Bajra(Pearl millet)",
      crop %in% c("Ajevan","Ajivan","Ajiwan") ~"Ajiwan", #seeads
                  TRUE ~ crop)) %>% 
  filter(crop != "Demoplot") 

#### also included 
# Safflower ,Papaya ,Garlic, Brinjal, Blackgram   
# Cowpea=yam
# Sunhemp=legume family 

# categorize----
ae <-  ad %>%
  mutate(
    crop_cat1 = case_when(
      cropx %in% c("Okra","Cucumber","Groundnut(Peanut)","Hawaij",
                  "Mulberry(Morus)","Onion","Pumpkin","Vegetables","Chili",
                  "Coriander","Papaya" ,"Garlic", "Brinjal","Cowpea" ) ~ 
        "Vegetables",
      cropx %in% c("Bajra(Pearl millet)","Foxtail_millet","Jowar(sorghum)",
                  "Maize","Wheat" ) ~
        "Cereal",
      cropx %in% c("Ajiwan","Sesamum","Sunflower","Safflower" ) ~
        "Oil_seeads",
      cropx %in% c("Green_gram(Mung bean)","Bengal_gram(Chickpea)","Blackgram",
                   "Soybean","Sunhemp", "Red_gram(Pigeon pea)") ~
        "Legume",
      TRUE ~ cropx)) %>% 
  mutate(
    value_crop = case_when(
      crop_cat1 %in% c("Vegetables","Oil_seeads","Sugarcane","Cotton" ) ~ "High_value",
      crop_cat1 %in% c("Cereal","Legume" ) ~ "Traditional",
      TRUE ~ "aaaaa"))

ji_261122$season[ji_261122$season=="kharif_2017"]<-"kharif_17"
ji_261122$season[ji_261122$season=="kharif_2018"]<-"kharif_18"
ji_261122$season[ji_261122$season=="kharif_2019"]<-"kharif_19"
ji_261122$season[ji_261122$season=="rabi_2017"]<-"rabi_17"
ji_261122$season[ji_261122$season=="rabi_2018"]<-"rabi_18"
ji_261122$season[ji_261122$season=="rabi_2019"]<-"rabi_19"

ji_261122 <- ae %>%
  mutate(
    SEASON = case_when(
      season %in% c('kharif_17', 'kharif_18', 'kharif_19',
                   'kharif_20_21','kharif_21_22') ~ 
        "kharif",
      TRUE ~ "Rabi"))

rm(aa,ab,ac,ad,af)


ac %>% # arrange(id_si) %>%  mutate( ID = paste("F", id_si, "seq", row_number(), sep = "_"))

  ji01 <- 
  ji_261122 %>% 
  select(id_si,season,SEASON, farmer_name,crop,cropx,crop_cat1,value_crop)

ji_01 <- 
  ji_261122 %>% 
  select(id_si,season,SEASON, farmer_name,crop,cropx,crop_cat1,value_crop)


ji_01 %>% group_by(season) %>% 
  summarise(n())

ji_01$season[ji_01$season=="kharif_17"] <- "Kharif 17-18\n1080"
ji_01$season[ji_01$season=="kharif_18"] <- "Kharif 18-19\n1640"
ji_01$season[ji_01$season=="kharif_19"] <- "Kharif 19-20\n1129"
ji_01$season[ji_01$season=="kharif_20_21"] <- "KKharif 20-21\n1979"
ji_01$season[ji_01$season=="kharif_21_22"] <- "Kharif 21-22\n2242"
ji_01$season[ji_01$season=="rabi_17"] <- "Rabi 17-18\n5069"
ji_01$season[ji_01$season=="rabi_18"] <- "Rabi 18-19\n1404"
ji_01$season[ji_01$season=="rabi_19"] <- "Rabi 19-20\n6088"
ji_01$season[ji_01$season=="rabi_20_21"] <- "Rabi 20-21\n4912"
ji_01$season[ji_01$season=="rabi_21_22"] <- "Rabi 21-22\n139"


#CROPS DISTRIBUTION ----

#season_labels----
season_labels <- c('Kharif 17-18\n1080', 'Kharif 18-19\n1640', 'Kharif 19-20\n1129',
                 'Kharif 20-21\n1979','Kharif 21-22\n2242',
                 'Rabi 17-18\n5069','Rabi 18-19\n1404','Rabi 19-20\n6088',
                 'Rabi 20-21\n4912','Rabi 21-22\n139') 

c('kharif_17', 'kharif_18', 'kharif_19','kharif_20_21','kharif_21_22',
  'Rabi_17', 'Rabi_18', 'Rabi_19','Rabi_20_21','Rabi_21_22')

c('Kharif 17-18\n1080', 'Kharif 18-19\n1640', 'Kharif 19-20\n1129',
  'Kharif 20-21\n1979','Kharif 21-22\n2242',
  'Rabi 17-18\n5069','Rabi 18-19\n1404','Rabi 19-20\n6088',
  'Rabi 20-21\n4912','Rabi 21-22\n139')

season.colors <- c(kharif = "#333BFF", rabi = "#E3DB71")
scale_fill_manual(values=season.colors)

#1  Traditional & High value COUNT ----

  ji01 %>% 
  select(id_si,season,farmer_name,crop,cropx,crop_cat1,value_crop) %>% 
  group_by(season,value_crop) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=season, y=n, fill=value_crop)) +
  geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label = " "), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    theme_light()+
    labs(title="Traditional & High-Value Crops",x = " ",y = "No. of crops")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"))

#2 High_value  COUNT ----
  
  ji01 %>% 
  select(id_si,season,farmer_name,crop,cropx,crop_cat1,value_crop) %>% 
  group_by(season,value_crop) %>% 
  summarise(n=n()) %>% 
  filter(value_crop=="High_value") %>% 
  
  ggplot(aes(x=season, y=n, fill=value_crop)) +
  geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label = " "), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    theme_light()+
    labs(title="High-Value Crops",x = " ",y = "No. of crops")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"))

#3 High_value crop type COUNT ----
ji01 %>% 
  group_by(season,cropx) %>% 
  mutate(n_cropx=n()) %>% 
  select(season,value_crop,n_cropx) %>% 
  distinct() %>% 
  group_by(season,value_crop) %>% 
  mutate(sum_season=sum(n_cropx)) %>% 
    filter(value_crop=="High_value") %>% 
  ggplot(aes(x=season, y = n_cropx, fill = cropx )) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = " "), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
  theme_light()+
  labs(title="High-value Crops by Type",x = " ",y = "No. of crops")+
  scale_x_discrete(labels=season_labels)+
  theme(text=element_text(size=16,  family="serif"))

#4 High_value type category  COUNT ----
High_value_cat <- ji01 %>% 
  select(id_si,season,farmer_name,cropx,crop_cat1,value_crop) %>% 
  group_by(season,crop_cat1) %>% 
  mutate(n_crop_cat1=n()) %>% 
  select(season,value_crop,n_crop_cat1) %>% 
  distinct() %>% 
  group_by(season,value_crop) %>% 
  mutate(sum_season=sum(n_crop_cat1)) %>% 
    filter(value_crop=="High_value")
  
  ggplot(High_value_cat,aes(x=season, y = n_crop_cat1, fill = crop_cat1 )) +  # Create stacked bar chart
  geom_bar(stat = "identity")+
    geom_text(aes(label = " "), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    theme_light()+
    labs(title="High-value crops by Category",x = " ",y = "No. of crops")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"))

#5 High-value crops as a % of all season crops ----
J.HVC_prc <-
  ji01 %>%
  group_by(season,value_crop) %>%
  mutate(n_cropx=n()) %>%
  select(season,value_crop,n_cropx) %>%
  distinct() %>%
  group_by(season) %>%
  mutate(sum_season=sum(n_cropx)) %>% 
  filter(value_crop=="High_value") %>% 
  mutate(HVC_prc=n_cropx/sum_season) %>% 
  mutate(freq = paste0(round(100 * n_cropx/sum_season, 0), "%"))
#1000/400
  ggplot(J.HVC_prc,aes(x=season, y=HVC_prc, fill=value_crop)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = freq), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
  theme_light()+ 
    labs(title="High-value crops as a percentage of the entire season",x = " ",y = "igh-value crops")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"),legend.position = "none")

    

  theme_set( theme_light( base_family= "serif"))
  
#6 tradution crops CNT ----
  
  ji01 %>%
  filter(cropx %in% c("Red_gram(Pigeon pea)","Bengal_gram(Chickpea)",
                      "Green_gram(Mung bean)", "Jowar(sorghum)")) %>% 
  group_by(season,cropx) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=season, y=n, fill=cropx)) +
  geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label = " "), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    theme_light()+
    labs(title="High-value crops as a % of all season crops",x = " ",y = "% of crop")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"))

#7 tradution crops as a % of all season crops----
  J.trad_prc <-
    ji01 %>%
    group_by(season) %>% 
    mutate (cropx_season1=n()) %>% 
    group_by(season,cropx) %>% 
    mutate (cropx_season2=n()) %>% 
    select(season,cropx,cropx_season1,cropx_season2) %>%
    distinct() %>%
    filter(cropx %in% c("Red_gram(Pigeon pea)","Bengal_gram(Chickpea)",
                        "Green_gram(Mung bean)", "Jowar(sorghum)")) %>% 
    mutate(prc=cropx_season2/cropx_season1) %>% 
    mutate(freq = paste0(round(100 * cropx_season2/cropx_season1, 0), "%"))
  
  ggplot(J.trad_prc,aes(x=season, y=prc, fill=cropx)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label = freq), size = 3, hjust = 0.5, vjust = 3, position = position_dodge(.9)) +
    theme_light()+
    labs(title=" % of all season crops",x = " ",y = "% of HH")+
    geom_text(aes(label = freq), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    theme_light()+
    labs(title="Traditional crops as a % of all season crops",x = " ",y = "% of crops")+
    scale_x_discrete(labels=season_labels)+
    theme(text=element_text(size=16,  family="serif"))

#8 both crops as a % of all season crops ----
df <-
    ji01 %>%
    group_by(season) %>% 
    mutate (cropx_season1=n()) %>% 
    group_by(season,cropx) %>% 
    mutate (cropx_season2=n()) %>% 
    select(season,cropx,cropx_season1,cropx_season2) %>%
    distinct() %>%
    filter(cropx %in% c("Red_gram(Pigeon pea)","Bengal_gram(Chickpea)",
                        "Green_gram(Mung bean)", "Jowar(sorghum)",
                        "Chili","Onion","Sunflower")) %>% 
    mutate(prc=cropx_season2/cropx_season1) %>% 
    mutate(freq = paste0(round(100 * cropx_season2/cropx_season1, 0), "%"))
  
  

## RAGI ----

ji_261122 %>% filter(cropx %in% c("Bajra(Pearl millet") ) #138

  
jTUR <-
  ji_261122 %>%
  group_by(season) %>% 
  mutate (total_cropx_season=n()) %>% 
  group_by(season,cropx) %>% 
  mutate(type_cropx_season=n(),av_size=mean(area_acre)) %>% 
  group_by(cropx) %>% 
  mutate(count(cropx))
  select(season,cropx,av_size,total_cropx_season,type_cropx_season) %>%
  distinct() %>%
  
   # filter(cropx =="Red_gram(Pigeon pea)")%>% 
   # filter(cropx =="Bengal_gram(Chickpea)") %>%  
   # filter(cropx =="Green_gram(Mung bean)") %>% 
   # filter(cropx =="Jowar(sorghum)") %>% 
   # filter(cropx == "Chili") %>% 
   # filter(cropx =="Onion") %>%
   # filter(cropx =="Sunflower") %>% 
  
  mutate(prc=type_cropx_season/total_cropx_season) %>% 
  mutate(freq = paste0(round(100 * type_cropx_season/total_cropx_season, 0), "%"))

  



ggplot(jTUR,aes(x=season, y=m, fill=n)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_light()+ 
  labs(title="Red Gram (Ragi,Pigeon pea)",x = " ",y = "Land Size (In Acre)")+
  geom_text(aes(label = n), size = 3, hjust = 0.5, vjust = 3, position = "stack",color="white") +
  scale_x_discrete(labels=season_labels)+
  theme(text=element_text(size=16,  family="serif"),legend.position = "none")

  
  



# Vegetables
ji_261122 %>%
  group_by(season) %>% 
  mutate (total_crop_cat1_season=n()) %>% 
  group_by(season,crop_cat1) %>% 
  mutate(type_crop_cat1_season=n(),av_size=mean(area_acre)) %>% 
  filter(SEASON!="kharif") %>% 
  select(season,crop_cat1,av_size,total_crop_cat1_season,type_crop_cat1_season) %>%
  distinct() %>%
  
  filter(crop_cat1 =="Vegetables") %>% 
  mutate(prc=type_crop_cat1_season/total_crop_cat1_season) %>% 
  mutate(freq = paste0(round(100 * type_crop_cat1_season/total_crop_cat1_season, 0), "%"))









  
######  MAP Shape fill  -----

extrafont::loadfonts(device="win")
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

windowsFonts()

windowsFonts(A = windowsFont("Times New Roman"))  
#
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

group.colors <- c(Cereals="tan3",Oilseeds= "khaki1",Pulses= "indianred3",Vegetables= "olivedrab3",Others= "lightskyblue3")

jain_common_crop %>% filter(season == "kharif_2017") %>% right_join(ramthal_east_tidy2) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cropCat)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.2) +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(x = " ",y = " ",fill = " ")+
  theme_minimal()+
  scale_fill_manual(values=group.colors)



group.colors <- 
  c(Cereals="tan2",Oilseeds= "yellow",Pulses= "lightskyblue3",
    Vegetables= "olivedrab1",Others= "lightskyblue4",No_data="gray80")

jcc <- jain_common_crop %>% filter(season == "rabbi_2019") %>%
  right_join(ramthal_east_tidy2) 
jcc$cropCat [is.na(jcc$cropCat)] <- "No_data" 

ggplot(jcc,aes(x = long, y = lat, group = group, fill = cropCat)) +
    geom_polygon() + geom_path(color = "white", size = 0.2) +coord_equal() +
    theme_minimal()+scale_fill_manual(values=group.colors)+
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(x = " ",y = " ",fill = " ")

# kharif_2019 ---- rabbi_2019
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



















library(haven)
Adt_191222 <- read_dta("C:/Users/Dan/Downloads/Adt_raw_19 Dec 2022.dta")








