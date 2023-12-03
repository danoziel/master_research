library(dplyr)
library(tidyverse)
library(haven)

# survey 2022 dataset ----
Ramthal_Karnataka_Cleaned_Data <- read_dta("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/Ramthal_Karnataka_submissions/Ramthal_Karnataka_Cleaned_Data.dta")
a_rmtl_srvy22 <- Ramthal_Karnataka_Cleaned_Data %>% rename(hh_id=id)
rm(Ramthal_Karnataka_Cleaned_Data)

# updating villages names and code
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





# ----







