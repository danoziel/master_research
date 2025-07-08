library(dplyr)
library(haven)
library(tidyr)
library("stringr") #"str_replace"

attr(rmtl_baseline2016$D24_1_Crop_1_0, "labels")
which(colnames(rmtl_baseline2016) == "D4_3")

# index_shpfile ----

library(sf)
map_file <- read_sf("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages","Ramthal_East")
bd_data0  <- read_sf("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

shp_rmtl <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")

#check the imports worked
head(shp_rmtl)
class(shp_rmtl)
attr(shp_rmtl, "sf_column")


(shp_rmtl_geom <- st_geometry(shp_rmtl)) # class = "sfc_POLYGON" "sfc" IS IT FORTIFY????????
shp_rmtl_geom[[1]]

######### fixing -999 
list_shape_code %>% filter(survey==-999)

bl_plot_SrvyHis %>% filter(hh_id==100030)
a_plots_size %>% filter(hh_id==100030)

# Check if there is a matching id polygon?
shp_rmtl %>% filter(id==160302)
list_shape_code %>% filter(id==160302)

# No polygon - rm 100030 !

list_shape_code2 # [hh_id=1702 | id=1,499 ]

###
n1 <- list_shape_code2 %>% group_by(id) %>% mutate(n=n())%>% filter(n==1)
   # 1,365 HH  |  1,365 id polygon 
n1A= list_shape_code2 %>% left_join(n1[,c(1,8)]) %>% filter(is.na(n) )
   #  336 HH | 133 id polygon 

# Now, select one HH for each id polygon from n1A
# Priority A: 258HH that irrigate 
priorityA <- a_irri_rain_method %>% mutate(irri=as.numeric(irri_method_num)) %>% group_by(hh_id)  %>% summarise( irri=sum(irri,na.rm = T)) %>% filter(irri>0)
n2  <- n1A %>% inner_join(priorityA) %>% group_by(id) %>% sample_n(1) 
     # 35 HH | 35 id polygon 
n2A <- n1A %>% left_join(n2[,c(5,9)] ) %>% filter(is.na(irri) ) 
     # 223HH | 98 id polygon

# Priority B: HH that use water, HH infrastructure
priorityB <- rmtl_srvy22 %>% select(hh_id,mm5)%>% filter(!is.na(mm5))
n3  <- n2A %>% inner_join(priorityB) %>% group_by(id) %>% sample_n(1)  
     # 52HH | 52 id polygon
n3A <- n2A %>% left_join(n3[,c(5,10)] ) %>% filter(is.na(mm5) ) 
      # 105HH | 46 id polygon

# Priority c: random
# Group by id and randomly select one hh_id for each group
n4 <- n3A %>% group_by(id) %>% sample_n(1)
#      46HH | 46 id polygon

n_1 = n1%>% select (hh_id,a6,bl_d4_0 ,survey,id)
n_2 = n2%>% select (hh_id,a6,bl_d4_0 ,survey,id)
n_3 = n3%>% select (hh_id,a6,bl_d4_0 ,survey,id)
n_4 = n4%>% select (hh_id,a6,bl_d4_0 ,survey,id)

index_shp <- 
  rbind(n_1, n_2, n_3, n_4) 

######### fixing NAs
library(tidyr)
# index_shp NAs 
index_shp_NAs <- rbind(n_1, n_2, n_3, n_4) %>% filter(is.na(survey))
                 # 52HH | 52 id polygon

shp_NA <- 
  index_shp_NAs[,1] %>% left_join(a_plots_size[,c(1,3)]) %>% 
  separate(plotSrvy, into = c("srvy", "hiss"), sep = "-") %>% 
  separate(srvy, into = c("srvy", "hiss"), sep = "/")%>% 
  group_by(hh_id) %>% mutate(n=n())
shp_NA$srvy <- gsub("[^0-9]", "", shp_NA$srvy)
shp_NA$srvy <- ifelse( shp_NA$hh_id==101759, "13",shp_NA$srvy ) 
shp_NA$srvy <- ifelse( shp_NA$hh_id==108984, "21",shp_NA$srvy ) 

shp_NA1 <-  shp_NA %>% group_by(hh_id) %>% slice(1) %>% filter(srvy>0)

# 3 types of hh in shp_NA1 :
# A. hh whose polygon is not in shp_rmtl
# B. hh whose polygon is in shp_rmtl and has a hh in index_shp
# C. hh whose polygon is in shp_rmtl but DOES NOT contain a hh  index_shp
### A and B to rm, C to save and add to index_shp

# generating id polygon
xc=index_shp %>% right_join(shp_NA1)
xc$srvy <- as.numeric(xc$srvy)
xc$id <- xc$id+xc$srvy
xc

Ids_shp_rmtl = shp_rmtl %>% as.data.frame() %>% select(id) %>% distinct()
new_ids <- 
  Ids_shp_rmtl %>% inner_join(xc) %>%  # rm A
  select(hh_id,id) %>%rename(HH=hh_id) %>%
  left_join(index_shp) %>% #if [hh_id = NA] means its id is "taken" therefore it should be rm
  filter(is.na(hh_id)) %>%   # rm B
  select(HH,id) %>% rename(hh_id=HH,fixed_id=id)

#### Adding new id's to the index

index_shpfile <- 
  index_shp %>% left_join(new_ids) %>% 
  mutate(id=ifelse(is.na(fixed_id),id,fixed_id))%>% 
  select(hh_id,a6, bl_d4_0, survey,id)








# DF ----
baseline_RMTL[1:20,4679:4695]
baseline_RMTL[1:20,4732:4739]
which(colnames(rmtl_baseline2016) == "D4_3")

baseline_RMTL %>% select("hh_id","in1_out0","in_out_intersect","inner_plots","layer","distance_km","around_boundary","south1_north0")
mid2018_RMTL[ ,c("id","in1_out0","layer","distance_km","around_boundary","south1_north0")]


HH_2016_baseline =baseline_RMTL %>% select(hh_id,in1_out0,in_out_intersect) %>% 
  mutate(sample_2016=in_out_intersect )
HH_2016_baseline$sample_2016[is.na(HH_2016_baseline$sample_2016)] <- 10
# 10= HH who are not 0 or 1 or 2 
HH_2016_baseline %>% count(in1_out0)
HH_2016_baseline %>% count(sample_2016)
HH_2016_baseline %>% count(in_out_intersect)

HH_2018 = mid2018_RMTL %>% select (id,in1_out0) %>% mutate( sample_2018=in1_out0) %>% rename(hh_id=id)
HH_2018 %>% count(sample_2018)
full_join(HH_2016_baseline[,c(1,4)],HH_2018[,c(1,3)]) %>% filter(sample_2016 ==10 )
  #NAs/10 in 2016 are same  NAs in 2018
HH_2018$sample_2018[is.na(HH_2018$sample_2018)] <- 10

HH1618 %>% count(sample_2016)
HH1618 %>% count(sample_2018)


HH_2022=a_rmtl_srvy22 %>% select(hh_id,mm2_1) %>% left_join(HH_2018[,c(1,3)]) %>% 
  mutate(sample_2022= ifelse(sample_2018 %in% c(0,1) ,sample_2018, mm2_1 ) ) %>% 
  select("hh_id", "sample_2022")

HH_2022=a_rmtl_srvy22 %>% select(hh_id,mm2_1) %>% left_join(HH_2018[,c(1,3)]) %>% 
  mutate(sample_2022= ifelse(sample_2018 %in% c(0,1) ,sample_2018, mm2_1 ) ) %>% 
  select("hh_id", "sample_2022")

HH_2022 %>% count(sample_2018)
HH_2022 %>% count(sample_2016)
HH_2022 %>% count(sample_2022)


sample_2016_2028_2022= #
  full_join(HH_2016_baseline[,c(1,4)],HH_2018[,c(1,3)]) %>% 
  full_join(HH_2022)
sample_2016_2028_2022 %>% count(sample_2016)
sample_2016_2028_2022 %>% count(sample_2018)# NAs are HH who didnt surveyed in 2018
sample_2016_2028_2022 %>% count(sample_2022)#  NAs are HH who didnt surveyed in 2020



#| 🟡BASELINE 2016 
rmtl_baseline2016 = baseline_RMTL
rmtl_baseline2016 = rmtl_baseline2016 %>% mutate(farmers_hh= in1_out0)
rmtl_baseline2016$farmers_hh[rmtl_baseline2016$farmers_hh==1] <- "inside_ramthal"
rmtl_baseline2016$farmers_hh[rmtl_baseline2016$farmers_hh==0] <- "outside_ramthal" 


#| 🟠MIDELINE 2018
rmtl_midline2018 = mid2018_RMTL %>% mutate(farmers_hh= in1_out0)
rmtl_midline2018 = rmtl_midline2018 %>% mutate(farmers_hh= in1_out0)
rmtl_midline2018$farmers_hh[rmtl_midline2018$farmers_hh==1] <- "inside_ramthal"
rmtl_midline2018$farmers_hh[rmtl_midline2018$farmers_hh==0] <- "outside_ramthal" 


  mutate(farmers_hh= ifelse(in1_out0==1, "inside_ramthal" ,"outside_ramthal" )) %>% 
  

#| 🟣MIDELINE 2022
rmtl_srvy22 = a_rmtl_srvy22 %>% left_join(HH_2022) %>% 
  mutate(farmers_hh= ifelse(sample_2022==1, "inside_ramthal" ,"outside_ramthal" ))
  
# plot ----
bl6_plotAcre=full_join(bl_plot_SrvyHis ,bl6_plotAcre)

P1a=rmtl_baseline2016 %>% select(hh_id,D2,D3,D61)%>%mutate_at(2:3,round,2) %>% 
  rename(Q_acre16=D2,ttl_p16=D3 ,Leased_land16=D61)
P1=bl6_plotAcre %>% group_by(hh_id) %>% summarise(sum_acre16=sum(plot_acre,na.rm = T)) %>% left_join(P1a)
#
P2= ml18_plots_size %>% group_by(hh_id) %>% summarise(sum_acre18=sum(acres,na.rm = T),ttl_p18=n())
#
P3=a_plots_size %>% group_by(hh_id) %>% summarise(sum_acre22=sum(acres,na.rm = T),ttl_p22=n())

P_a= left_join (P1[,1:2], P2[,1:2] ) %>% left_join(P3[,1:2])
  
P_= 
  left_join(P1,P2) %>% left_join(P3) %>%
  mutate(gap_acr= abs((sum_acre16+sum_acre18)/2-sum_acre22),gap_plt=(ttl_p22+ttl_p18)/2-ttl_p16) %>% 
  mutate_at(10:11,round,2) %>% 
  mutate(el=ifelse(gap_acr>0 & gap_plt>0,1,0)) %>% mutate( prcnt10=sum_acre16*0.1 ) %>%  
  filter(!is.na(gap_acr)) %>% filter(gap_acr > .15, el==0) %>% 
  mutate(el=ifelse(prcnt10 > gap_acr ,1 ,0)) %>% 
  filter(el==0)


bl6_plotAcre  %>% filter(hh_id==100019)
ml18_plots_size %>% filter(hh_id==100019)
a_plots_size %>% filter(hh_id==100019)

p16=bl6_plotAcre %>% rename(acre_BL=plot_acre,plotID = plot_num)
p18=ml18_plots_size %>% rename(acre_MID=acres,plotID = plotId   )
p22=a_plots_size %>% rename(acre_END=acres)%>% select(plotSrvy, plotSrvyHissa, plotStatus, plotVillage, hh_id, plotID,acre_END  )
plot_size= p22 %>% left_join(p16)%>% left_join(p18) %>% 
  select(plotSrvy:plotID ,acre_BL, acre_MID,acre_END )


plot_BL_dt <-full_join(bl_plot_SrvyHis, bl6_plotAcre)
plot_BL_dt %>% filter(is.na(plot_acre))

ml18_plots_size







# village_code ----
village_code <- a_rmtl_srvy22 %>%
  mutate(village_code=ifelse(a5 %in% c("Amaravati", "amaravati",'Amaravathi',"AMARAVATHI"),"01",a5)) %>%
  mutate(village_code=ifelse(a5 %in% c("Bekamaladinni","bekamaladinni" ,"Bekamaldinni","BEKAMALADINNI"),"02",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Binjawadgi" ,"binjawadagi","Binjawadagi","BINJAWADAGI"),"03",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Chikkabadwadgi","Chikkabadavadagi","Chikkabadawadagi","Chikkabadawadgi","chikkabadawadagi"),"04",village_code)) %>% 
  mutate(village_code=ifelse(village_code %in% c("Chinnapur", "chinnapur", "chinnapur","Chinnapur S T","Chinnapur ST","Chunnapur ST"), "05",village_code)) %>% 
  mutate(village_code=ifelse(village_code %in% c( "Chintakamaladinni","chintakamaladinni","Chintakamaladini","Chintakamldinni" ), "06",village_code)) %>% 
  mutate(village_code=ifelse(village_code %in% c("Chittawadagi", "chittavadagi","Chittawadgi","Chittavadagi" ),"07" ,village_code)) %>%   
  mutate(village_code=ifelse(village_code %in% c("Ghattignur","ghattiganur" ,"Gattiganur","Ghattiganur" ,"GHATTIGANUR"),"08" ,village_code)) %>%  
  mutate(village_code=ifelse(a5 %in% c("Gopasani","gopasani"),"09" ,village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Gorabal","gorabal" ),"10" ,village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Hachanur","hachanur"),"11",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c( "Havaragi","havaragi"),"12",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c( "Hagedal","hegedal","Hegedal","HEGEDAL" ),"13",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c( "Hemawadagi","hemavadagi","Hemavadagi"),"14",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c( "Herur","herur","HERUR"),"15",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c( "Hulgera","hulgera"),"16",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c( "Hungund","hungund"),"17",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c( "Ingalagi","ingalagi"),"18",village_code)) %>%  
  mutate(village_code=ifelse(a5 %in% c( "Jalakamaladini" ,"jalakamaladini","Jalakamaladinni"),"19",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c( "Kadiwal","kadiwal","Kadiwal inam" ,"Kadiwal Inam"),"20",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Kesarbhavi", "kesarabhavi","Kesarabhavi"),"22",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Konnur","konnur"),"23",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Marol","marol","MAROL"),"24",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Nagur","nagur"),"25",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Ramawadagi", "ramawadagi","Ramavadagi","RAMAWADAGI","Ramawadgi" ),"26",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Revadihal","revadihal"),"27",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Turamari","turamari"),"28",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Veerapur","virapur","Virapur"),"29",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Yadahalli","yadahalli"),"30",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Bannihatti"),"31",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Budihal","budihal"),"32",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Chatnihal","chatnihal"),"33",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Gadisunkapur","gadisunkapur"),"34",village_code)) %>% 
  mutate(village_code=ifelse(a5 %in% c("Hirebadawadgi" ,"hirebadawadagi","Hirebadawadagi","HIrebadawadagi"),"35",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Hirehunakunti" ,"hirehunkunti","Hirehunkunti"),"36",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Hullalli","hullalli"),"37",village_code)) %>%         
  mutate(village_code=ifelse(a5 %in% c("Jambaladinni","jambaladinni"),"38",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Koppa","koppa"),"39",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Malagihal","malagihal"),"40",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Nidasanur","nidasanur","Nidasanoor"),"41",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("Tumba","thumba","Thumba"),"42",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("ILKAL","Ilakal","Ilkak","Ilkal","Ilkal mahantesh  cinema theater","ilkal"),"500Ilkal",village_code)) %>%
  mutate(village_code=ifelse(a5 %in% c("-888--Other Specify / Other Specify","Dadabal","Islaampur","Other Specify","Palathi","Palthi", "St chinnapura","Taariwaal"),"500Others",village_code)) 

a_rmtl_srvy22 = village_code %>% full_join(list_villages)
rm(village_code)

# list villages and code
list_villages <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/villages_list.csv")
list_villages$village_code <- sprintf("%02d", list_villages$village_code)


# rmtl_groups ----
YR_Ramthal_Data_Entry_2_stata13 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata13.dta")
list_groups_rmtl = YR_Ramthal_Data_Entry_2_stata13[ ,c("id","in1_out0","layer","distance_km","around_boundary","south1_north0")]
rm(YR_Ramthal_Data_Entry_2_stata13) #heavy file- better to remove it

 # hh_2022 ----
hh_2022= rmtl_srvy22 %>%  select(hh_id,farmers_hh)
  
irrigation_HH

#GIS shape_code 
list_shape_code <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/shape_code.csv")
list_shape_code <- list_shape_code %>% 
  mutate(
    survey = as.character (survey),
    shp_code = as.character(shp_code))

#[survey 2022] MM2_1Is your land coming under such a government project?
MM2_1 <- select(a_rmtl_srvy22,(c(id,mm2_1)))

x1_rmtl_groups <- full_join(list_groups_rmtl,MM2_1) %>% 
  mutate(in1_out0=ifelse(is.na(in1_out0),mm2_1,in1_out0)) %>% 
  mutate(sample_mm2_1= ifelse(in1_out0 == 1 | mm2_1== 1 , 1,0))%>% 
  mutate(list16_mm2.1_correlation=ifelse(in1_out0 == mm2_1, 1,0))

list_groups_rmtl=a_rmtl_srvy22[,1] %>% left_join(x1_rmtl_groups)
rm(x1_rmtl_groups,MM2_1)

a_sample= list_groups_rmtl %>% 
  select(id,in1_out0,layer,distance_km,around_boundary,south1_north0) %>% 
  left_join(list_shape_code ) %>% 
  rename(hh_id=id, layer_elevation=layer,village_code =a6) %>%
  mutate(village_code = sprintf("%02d", village_code)) %>% 
  full_join(list_villages)

a_sample$farmers_hh=ifelse(a_sample$in1_out0 == 1, "inside_ramthal","outside_ramthal")  
a_sample$distance_up_to_1km=ifelse(a_sample$distance_km==1,1,0)
a_sample$distance_up_to_1km=ifelse(is.na(a_sample$distance_km),0,a_sample$distance_up_to_1km)
a_sample$distance_up_to_1.5km=ifelse(!is.na(a_sample$distance_km),1.5,0)





