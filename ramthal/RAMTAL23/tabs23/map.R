# library(raster)
# shp <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
library(dplyr)
library(ggplot2)
library(sf)

Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
Ramthal_East_shp %>% ggplot() +geom_sf()

shapefile_wgs84 <- st_transform(shapefile, crs = st_crs(4326))  # 4326 is the EPSG code for WGS84 (longitude and latitude)


#  shapfile_rmtl_2022----
shp_index22_NA <-  # 188 hh NAs
  list_shape_code %>% inner_join(hh_2022) %>% filter(is.na(survey))

shp_rmtl_id <- # id polygon
  Ramthal_East_shp %>% as.data.frame() %>% select(id) %>% distinct() %>% mutate(is=1)

library(tidyr)
survey_nu_22 <- # generating 118 new id's 
  a_plots_size[,c(1,3)] %>% right_join(shp_index22_NA[,1]) %>% 
  group_by(hh_id) %>% slice(1) %>% 
  separate(plotSrvy, into = c("survey", "hiss"), sep = "-") %>% 
  separate(survey, into = c("survey", "hiss"), sep = "/") %>% 
  mutate(survey=ifelse(survey%in% c("_999","12A"),"",survey)) %>% 
  mutate(survey=ifelse(survey=="12A","12",survey)) %>% 
  mutate(survey=ifelse(survey%in% c("44744","44714","44573"),"44",survey)) %>% 
  mutate(survey= as.numeric(survey)) %>% select(-hiss) %>% 
  left_join(shp_index22_NA[,c(1,5)]) %>% 
  mutate(idOLD=id, id= ifelse(!is.na(survey),survey+id,id )) %>%   
  left_join(shp_rmtl_id) %>% 
  mutate(idNA=ifelse(is.na(is),idOLD,id)) %>% 
  select(idNA,hh_id)
  
shp_index22 <- # implimanting 79 of the 118 new id's into the index
  list_shape_code %>% inner_join(hh_2022) %>% 
  left_join(survey_nu_22) %>% 
  mutate(id=ifelse(is.na(idNA),id,idNA)) %>% 
  select(id, hh_id, farmers_hh)

rm(shp_index22_NA,shp_rmtl_id, survey_nu_22)

#### shapfile_rmtl_2022 ###
shapfile_rmtl_2022 <- Ramthal_East_shp %>% left_join(shp_index22)

# MAPS -----
       -----------------------------
#|     |  R map for presentaition  |
#|     |  W 8.91   H  6.5          |
       -----------------------------

# polygons ----
ggplot(shapfile_rmtl_2022) +geom_sf()


# villages map ----

shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scale_fill_distiller(palette = "Greys")
scale_fill_distiller(palette = "YlOrBr")


# for the villages names  
shapfile_rmtl_2022 %>% ggplot()+ geom_sf(aes(fill = layer))+ theme(legend.position = "bottom")



# project/ non project ----

shapfile_rmtl_2022 %>% mutate(farmers_hh=ifelse(is.na(farmers_hh),"not_sampled",farmers_hh)) %>% 
  ggplot()+ geom_sf(aes(fill = farmers_hh), color="gray55")+
  scale_fill_manual(
    values = c("inside_ramthal" = "dodgerblue4", "outside_ramthal" = "orange3", "not_sampled" = "white"))+
  theme_minimal() +theme(legend.position = "none")

# share hh irrigation ----

shp_rmtl_id <- Ramthal_East_shp %>% as.data.frame() %>% select(id) %>% distinct() %>% mutate(is=1)

# irrigation from Gov supply group_by( id )
A.method <- a_irri_rain_method %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct()
  
A.gov <- 
  a_source_irri[,c(2,7)] %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(irri_source_num= ifelse(5 %in% irri_source_num, "5", ifelse(any(irri_source_num== 2), "2",ifelse(any(irri_source_num== 3), "3",ifelse(any(irri_source_num== 4), "4",ifelse(any(irri_source_num== 7), "7","0"))))) ) %>% ungroup() %>% 
  select(id,irri_source_num ) %>% distinct() 

A.mg=left_join(A.method,A.gov) %>% right_join(shp_rmtl_id) %>% 
  mutate(hh_6methods=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  mutate(irri_source_num=ifelse(is.na(irri_source_num),"not_sampled",irri_source_num ) ) %>% 
  mutate(mg=ifelse(hh_6methods!="rain" & irri_source_num==5,"irri_gov",hh_6methods ) )

# map
shapfile_rmtl_2022 %>% 
  left_join(A.mg) %>% 
  ggplot()+ geom_sf(aes(fill = mg), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("irri_gov" = "aquamarine2","drip" = "deepskyblue",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+
  theme_minimal() 


# irrigation group_by( id )
irrigation22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  select(id ,irrigation)

# irrigation group_by(hh_id )
irrigation.HH.22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(hh_id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% 
  ungroup() %>% select(hh_id,hh_6methods ) %>% distinct() %>% right_join(hh_2022) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"rain",hh_6methods ) ) %>% 
  select(hh_id ,irrigation)




# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80")+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
      "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
      "rain"="gray70" ,"not_sampled" = "white"))+theme(legend.position = "none")+
  theme_minimal() 

# map irrigation only
# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+theme_minimal() 

# location_on_pipe ----

location_on_pipe <-
  rmtl_srvy22 %>% select(hh_id,mm9,mm10,farmers_hh) %>% mutate(mm10=as.factor(mm10)) %>% 
  mutate(on_pipe1 = case_when(mm9>=0 & mm9<6~"0-5",mm9>=6 & mm9<51~"5+",TRUE~"NA")) %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(on_pipe2=ifelse("0-5" %in% on_pipe1 , "0-5",
                        ifelse(any(on_pipe1  == "5+"), "5+",
                               "no")))  %>% ungroup() %>%   
  select(id,on_pipe2 ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(on_pipe=ifelse(is.na(on_pipe2),"not_sampled",on_pipe2 ) ) %>% 
  select(id ,on_pipe)



# mm9
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>% 
  ggplot()+ geom_sf(aes(fill = on_pipe), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("0-5" = "maroon","5+" = "maroon4" ,"no" ="gray70" ,"not_sampled" = "white"))+
  theme_minimal()

#mm10
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>%
  ggplot()+ geom_sf(aes(fill = mm10), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()



# irrigation source 2022  -----

# rank1
source_rank_one <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank1= # former rank_1
           ifelse(5 %in% l7_rank_1 , "5", 
           ifelse(any(l7_rank_1  == 2), "2",
           ifelse(any(l7_rank_1  == 3), "3",
           ifelse(any(l7_rank_1  == 4), "4",
           ifelse(any(l7_rank_1  == 7), "7",
                            "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank1 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank1=ifelse(is.na(source_rank1),"not_sampled",source_rank1)) %>% 
  select(id ,source_rank1) %>%  #irrigation_source
  mutate(source_rank1_1yes0no =
           ifelse(source_rank1==0,0,
           ifelse(source_rank1=="not_sampled",
                                     "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_one) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank1), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()


# rank2
source_rank_two <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank2=  # source_rank1
           ifelse(5 %in% l7_rank_2, "5", 
                  ifelse(any(l7_rank_2== 2), "2",
                         ifelse(any(l7_rank_2== 3), "3",
                                ifelse(any(l7_rank_2== 4), "4",
                                       ifelse(any(l7_rank_2== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank2 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank2=ifelse(is.na(source_rank2),"not_sampled",source_rank2)) %>% 
  select(id ,source_rank2) %>%
  mutate(source_rank2_1yes0no =
           ifelse(source_rank2==0,0,
                  ifelse(source_rank2=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_two ) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank2), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()



# rank1+rank2+rank3 GOV SUPPLY
source_gov_supply <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_Gov=  # source_rank1
           ifelse(5 %in% irri_source_num, "5", 
                  ifelse(any(irri_source_num== 2), "2",
                         ifelse(any(irri_source_num== 3), "3",
                                ifelse(any(irri_source_num== 4), "4",
                                       ifelse(any(irri_source_num== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_Gov) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_Gov=ifelse(is.na(source_Gov),"not_sampled",source_Gov)) %>% 
  select(id ,source_Gov) %>%
  mutate(source_Gov_1yes0no =
           ifelse(source_Gov==0,0,
                  ifelse(source_Gov=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join( source_gov_supply ) %>% 
  ggplot()+ geom_sf(aes(fill = source_Gov), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()







# first make use ----
first_yr <- rmtl_srvy22 %>% select(hh_id,contains("mw1a")) %>% mutate(first_yr_use=ifelse(is.na(mw1a),2000,mw1a))%>% mutate(mw1a=as.integer(mw1a) )
  

shapfile_rmtl_2022 %>% left_join(first_yr) %>%
  ggplot()+ geom_sf(aes(fill = first_yr_use), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()


cor.test(irrigation.HH.22$irri01,first_yr$mw1a) # 👎🏽






# cropping pattern ----

a_plots_crop %>% filter(season == "kha") %>% count(crop_common)
# Chillies Greengram Toor Maize Onions   Sunflower Oilseeds Pearl.millet_bajra                       

a_plots_crop %>% filter(season == "rabi") %>% count(crop_common)
# Bengal_gram Oilseeds Sorghum_jowar Sugarcane Wheat                         

#| OUT: Cereals Groundnut            ;Horticulture   



map_crop %>% filter(season == "kha") %>% count(crop_common)
map_crop %>% filter(season == "rabi") %>% count(crop_common)

map_crop <- 
  a_plots_crop %>% left_join(shp_index22) %>% 
  filter(season !="KHA22", plot_crop =="01_1"  ) %>% 
  select(season ,id,plot_crop,crop_common) %>% distinct() %>% 
  group_by(id, season) %>% slice(1)

map_kha_crop <- map_crop %>% filter(season =="kha")
map_rabi_crop <- map_crop %>% filter(season =="rabi")


shapfile_rmtl_2022 %>% left_join(map_kha_crop ) %>% 
  ggplot()+ 
  geom_sf(aes(fill = crop_common  ), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scico::scale_fill_scico(palette = "bilbao")

  
# https://ggplot2-book.org/scales-colour ----

erupt =
  shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40")+
  theme_minimal() +
  theme(legend.position = "none")

library(viridis)
erupt + scale_fill_viridis_c()
erupt + scale_fill_viridis_c(option = "H") #options A-H

library(RColorBrewer)
display.brewer.all()

erupt + scale_fill_distiller()
erupt + scale_fill_distiller(palette = "RdPu")
erupt + scale_fill_distiller(palette = "YlOrBr")


library(scico)
erupt + scico::scale_fill_scico(palette = "bilbao") # the default
erupt + scico::scale_fill_scico(palette = "vik")
erupt + scico::scale_fill_scico(palette = "lajolla")


