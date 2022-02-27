# villages map----
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


#----
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

ggplot(Ramthal_East,aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()

rbind(amaravathi,banihatti,ramawadgi) %>% 
  ggplot(aes(x=long,y=lat,group=group))+geom_polygon(color="black",size=0.1,fill="lightgrey")+coord_equal()+theme_minimal()

Ramthal_East_p <- fortify(Ramthal_East)
banihatti_data_id <- banihatti@data 




