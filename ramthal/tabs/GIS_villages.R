library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(sp)

# villages map    ----
library(rgdal)
Amaravathi <- readOGR("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Amaravathi")
Banihatti <- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Banihatti")
Binjawadgi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Binjawadgi" )
Budihal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Budihal" )
Chaitawadagi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Chaitawadagi" )

Chatnihal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Chatnihal" )
Chikkabadawadagi <- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Chikkabadawadagi" )
Chinnapur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Chinnapur")
Gadisunkapur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Gadisunkapur" )
Gattiganur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Gattiganur")

Gopasani<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Gopasani" )
Gorabal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Gorabal" )
Hachanur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Hachanur")
Havaragi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Havaragi" )
Hegedal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Hegedal" )

Hemavadgi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Hemavadgi" )
herur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "herur")
Hirebadawadagi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Hirebadawadagi")
Hirehunakunti<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Hirehunakunti")
Hulgera<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Hulgera" )

Hullalli<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Hullalli" )
Hungund<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Hungund")
Ingalagi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Ingalagi")
Jalakamaladinni<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Jalakamaladinni")
Jambladinni<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Jambladinni")

Kadiwal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Kadiwal")
Kesarbhavi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Kesarbhavi")
Konnur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Konnur")
Koppa<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Koppa")
Malligihal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Malligihal")

Marol<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Marol")
Nagur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Nagur")
Nindasanur<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Nindasanur")
Ramawadgi<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Ramawadgi")
Ramthal_East<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "Ramthal_East")

Revadihal<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Revadihal" )
temp<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" , "temp")
Thumba<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Thumba" )
Turamari<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Turamari" )
Virapura<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Virapura" )

Yadahalli<- readOGR ("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Yadahalli" )

rmtl <- rbind(Amaravathi,Banihatti,Binjawadgi,Budihal,Chaitawadagi,
              Chatnihal,Chikkabadawadagi,Chinnapur,Gadisunkapur,Gattiganur,
              Gopasani,Gorabal,Hachanur,Havaragi,Hegedal,
              Hemavadgi,herur,Hirebadawadagi,Hirehunakunti,Hulgera,
              Hullalli,Hungund,Ingalagi,Jalakamaladinni,Jambladinni,
              Kadiwal,Kesarbhavi,Konnur,Koppa,Malligihal,
              Marol,Nagur,Nindasanur,Ramawadgi,Revadihal,
              temp,Thumba,Turamari,Virapura,Yadahalli)

rm(Amaravathi,Banihatti,Binjawadgi,Budihal,Chaitawadagi,
   Chatnihal,Chikkabadawadagi,Chinnapur,Gadisunkapur,Gattiganur,
   Gopasani,Gorabal,Hachanur,Havaragi,Hegedal,
   Hemavadgi,herur,Hirebadawadagi,Hirehunakunti,Hulgera,
   Hullalli,Hungund,Ingalagi,Jalakamaladinni,Jambladinni,
   Kadiwal,Kesarbhavi,Konnur,Koppa,Malligihal,
   Marol,Nagur,Nindasanur,Ramawadgi,Revadihal,
   temp,Thumba,Turamari,Virapura,Yadahalli)

ggplot(rmtl,aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()

#  Ramthal_East   ----

# Top N highest values
kharif_2017P <- 
  jain_171819%>%arrange(desc(area_ha))%>%group_by(id_yoav,season)%>%
  slice(1) %>% filter(season == "kharif_2017") 

### ### ### ### # ###

Ramthal_East <- readOGR("~/master_research/DATAs/ramthal_data/project_map/villages" ,"Ramthal_East")
ramthal_east <- Ramthal_East
ramthal_east@data <- rename(ramthal_east@data, id_yoav = id)

# ** ramthal_east@data <-na.omit(ramthal_east@data)

ramthal.east.data <- ramthal_east@data ### ### ### ###

plot(ramthal_east)

## Plot NYC ----
A <- ramthal_east@data
AA <- ramthal_east@data$id
# in order to plot polygons, first fortify the data
ramthal_east@data$id <- row.names(ramthal_east@data) 

library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# create a data.frame from our spatial object
ramthal_4_data <- fortify(ramthal_east,by = "id")

kharif_17 <-inner_join (kharif_2017P,ramthal_east@data = "id")

# merge the "fortified" data with the data from our spatial object
ramthal_east.df <- merge(kharif_17, ramthal_4_data,by = "id")

#    ----

# ggplot
ggplot(ramthal_east,aes(x=long,y=lat,group=group))+
  geom_polygon(color="black",size=0.1,fill="lightgrey")+
  coord_equal()+
  theme_minimal()

#----
BanihattiID <- Banihatti@data

head(raster::geom(Banihatti))

head(ggplot2::fortify(Banihatti))
head <- ggplot2::fortify(Banihatti)

Banihatti@polygons[[3]]@labpt
Banihatti@data$id[[3]]

Banihatti@polygons[[9]]@Polygons[[1]]@coords


labpt <- lapply(Banihatti@polygons,function(p) data.frame(p@Polygons[[1]]@labpt))
labpt_id <- as.data.frame(Banihatti@data$id)

labpt <- as.data.frame(labpt)
df <- gather(labpt,lab,val) 
df$V1 <- paste0(df$V1, c("long","lat")) 
df <- spread(df, key=V1, value=val)

df %>%
  separate(lab, c("col1", "col2"), "...")
  