library(tidyverse)
library(readxl)
library(haven)
library(kableExtra)

# df_ifmr_wider & df_ifmr_long ----
# combine all DBs to One
prefill_LN <- read_excel("~/master_research/DATAs/ramthal_data/ifmr_ml_2022_01112022.xlsx",
                                    sheet = "prefill_LN")

List_DSF <- read_excel("~/master_research/DATAs/ramthal_data/ifmr_ml_2022_01112022.xlsx",
                       sheet = "List_DSF")

roster_2016_2018 <- read_excel("~/master_research/DATAs/ramthal_data/ifmr_ml_2022_01112022.xlsx",
                       sheet = "roster_2016_2018")


DSF  <- List_DSF[,c(1,2,5, 3,6, 4,7)]

DSF[DSF=="NA"] <- NA
DSF[DSF== 0 ] <- NA
DSF[DSF== -666] <- NA
DSF[DSF== -777] <- NA
DSF[DSF== -888] <- NA
DSF[DSF== -999] <- NA
DSF[DSF== -444] <- NA

#door ----
DSF_door <- DSF %>% 
  mutate(
    door_Num = case_when(
      door_Num_bl16 == door_Num_ml18 ~ door_Num_bl16,
      is.na(door_Num_bl16) ~ door_Num_ml18,
      is.na(door_Num_ml18) ~ door_Num_bl16,
      door_Num_bl16 != door_Num_ml18 ~ door_Num_bl16,
      TRUE                      ~ door_Num_bl16
    )) %>% 
  select(-c(door_Num_bl16,door_Num_ml18))
DSF_door$door_Num[DSF_door$door_Num == "]]]]]" ] <- NA

#father ----
DSF_father <- DSF_door %>%
  mutate(
    father_name = case_when(
      father_name_bl16 == father_name_ml18 ~ father_name_bl16,
      is.na(father_name_bl16) ~ father_name_ml18,
      is.na(father_name_ml18) ~ father_name_bl16,
      father_name_bl16 != father_name_ml18 ~ father_name_bl16,
      TRUE                      ~ father_name_bl16
    )) %>% 
  select(-c(father_name_bl16,father_name_ml18))

#spouse_name ----
DSF_spouse <- DSF_father %>% 
  mutate(
    spouse_name=case_when(
      spouse_name_bl16 == spouse_name_ml18 ~ spouse_name_bl16,
      is.na(spouse_name_bl16) ~ spouse_name_ml18,
      is.na(spouse_name_ml18) ~ spouse_name_bl16,
      spouse_name_bl16 != spouse_name_ml18 ~ spouse_name_bl16,
      TRUE                      ~ spouse_name_bl16)) %>% 
  select(Id,father_name, spouse_name,door_Num)

df_DSF <- DSF_spouse

rm(DSF_father, DSF_spouse, DSF_door)

## df_ifmr_long ----
df_ifmr_a <- 
  roster_2016_2018 %>% 
  mutate(gender=ifelse(gender==0,2,gender)  ) %>% 
  group_by(Id) %>% arrange(relationship_code) %>% 
  arrange(Id) %>% 
  group_by(Id) %>% arrange(year) %>% 
  mutate(numbering = row_number()) %>% 
  mutate(numbering=str_pad(numbering, 2, pad = "0")) %>% 
  mutate (Id_m=paste(Id, numbering, sep="_"))

df_ifmr_long <- 
  df_ifmr_a %>% 
  full_join(df_DSF) %>% 
  full_join(prefill_LN) %>% 
  select(Id, Id_m, village, everything()) %>% 
  select(-14)

phone18 <- ifmr_mid_2018 %>% select(id,a10) %>% rename(Id=id)

df_ifmr_Sachin <- df_ifmr_long %>%  full_join(phone18)

df_ifmr_Sachin %>% kable() %>% kable_classic()

## df_ifmr_wider  ----
df_ifmr_wider <-  # missing: gender, age, relationship_code
  df_ifmr_a %>%
  select("Id","HH_name_2016" ,"member_name","numbering") %>%
  mutate (mmr="member_name",m_n =paste(mmr, numbering, sep="_")) %>% 
  select("Id","HH_name_2016" ,"member_name","m_n") %>% 
  spread(key=m_n, value=member_name) %>% 
  full_join(df_DSF) %>% 
  full_join(prefill_LN) 


rm(prefill_LN,df_DSF,roster_2016_2018,df_ifmr_a)



#----




shape_code <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/shape_code.dta")


IDs_18 <- ifmr_mid_2018 %>% select(id,a5,a6,a10,a13)

Jain_crop_17_22 %>% group_by(season) %>% count()

village_list4 <- read_excel("~/master_research/DATAs/ramthal_data/village_list4.xlsx")
village_list <- village_list4[,c(1,2,5)]
  
village_list <- village_list %>% 
  mutate(village=ifelse(a5 == "binjawadagi", "Binjawadagi",village)) %>% 
  mutate(village=ifelse(village == "Ghattignur", "Ghattignur",village)) #%>% 

IDs_ji <- Jain_crop_17_22 %>%
  filter(season=="rabi_20_21") %>% 
  select (farmer_name,village,survey_hissa,survey_number) %>%
  full_join(village_list4)

  
IDs_jiV <- IDs_ji%>% select (village,survey_number) %>% distinct()
IDs_jiV2 <- IDs_ji%>% select (village) %>% distinct()

Ghattignur
Ghattignur


IDs_ji$survey_number <- gsub("(^\\d+)([\a-zA-Z0-9]*)", "\\1", IDs_ji$survey_number)





Malakajappa Tumbagi S/O Shivarudrappa - Ashok S Hosamani





# id_yoav----

Jain_crop_17_22

jain_789F <- 
  jain_789E %>% filter(survey_plot>0,a6>0)

# Adding Leading digits to the Elements of a Vector
library(stringr)
jain_789F$a6 <- str_pad(jain_789F$a6, 2, pad = "0")
jain_789F$a6 <- str_pad(jain_789F$a6, 3, pad = "1")
jain_789F$survey_plot <- str_pad(jain_789F$survey_plot, 3, pad = "0")
jain_789F$id_yoav <- str_c(jain_789F$a6,jain_789F$survey_plot)

rm(jain_789A,jain_789B,jain_789C,jain_789D,jain_789E)



# village ----

AA <- Jain_crop_17_22 [,c(1,5)] %>% arrange(village)


AB <- AA %>%
  mutate(village=ifelse(village %in% c("Amaravati","amaravati",'Amaravathi',"AMARAVATHI"),"01",village)) %>% 
  mutate(village=ifelse(village %in% c("Bekamaladinni","bekamaladinni" ,"Bekamaldinni","BEKAMALADINNI"),"02",village)) %>% 
  mutate(village=ifelse(village %in% c("Binjawadgi" ,"binjawadagi","Binjawadagi","BINJAWADAGI"),"03",village)) %>% 
  mutate(village=ifelse(village %in% c("Chikkabadwadgi","Chikkabadavadagi","Chikkabadawadagi","Chikkabadawadgi","chikkabadawadagi"),"04",village)) %>% 
  mutate(village=ifelse(village %in% c("Chinnapur", "chinnapur", "chinnapur","Chinnapur S T","Chinnapur ST","Chunnapur ST"), "05",village)) %>% 
  mutate(village=ifelse(village %in% c( "Chintakamaladinni","chintakamaladinni","Chintakamaladini","Chintakamldinni" ), "06",village)) %>% 
  mutate(village=ifelse(village %in% c("Chittawadagi", "chittavadagi","Chittawadgi","Chittavadagi" ),"07" ,village)) %>%   
  mutate(village=ifelse(village %in% c("Ghattignur","ghattiganur" ,"Gattiganur","Ghattiganur" ,"GHATTIGANUR"),"08" ,village)) %>%  
  mutate(village=ifelse(village %in% c("Gopasani","gopasani"),"09" ,village)) %>% 
  mutate(village=ifelse(village %in% c("Gorabal","gorabal" ),"10" ,village)) %>%
  mutate(village=ifelse(village %in% c("Hachanur","hachanur"),"11",village)) %>%
  mutate(village=ifelse(village %in% c( "Havaragi","havaragi"),"12",village)) %>%
  mutate(village=ifelse(village %in% c( "Hagedal","hegedal","Hegedal","HEGEDAL" ),"13",village)) %>% 
  mutate(village=ifelse(village %in% c( "Hemawadagi","hemavadagi","Hemavadagi"),"14",village)) %>%
  mutate(village=ifelse(village %in% c( "Herur","herur","HERUR"),"15",village)) %>%
  mutate(village=ifelse(village %in% c( "Hulgera","hulgera"),"16",village)) %>% 
  mutate(village=ifelse(village %in% c( "Hungund","hungund"),"17",village)) %>% 
  mutate(village=ifelse(village %in% c( "Ingalagi","ingalagi"),"18",village)) %>%  
  mutate(village=ifelse(village %in% c( "Jalakamaladini" ,"jalakamaladini","Jalakamaladinni"),"19",village)) %>%
  mutate(village=ifelse(village %in% c( "Kadiwal","kadiwal","Kadiwal inam" ,"Kadiwal Inam"),"20",village)) %>% 
  mutate(village=ifelse(village %in% c("Kesarbhavi", "kesarabhavi","Kesarabhavi"),"22",village)) %>%
  mutate(village=ifelse(village %in% c("Konnur","konnur"),"23",village)) %>%
  mutate(village=ifelse(village %in% c("Marol","marol","MAROL"),"24",village)) %>%
  mutate(village=ifelse(village %in% c("Nagur","nagur"),"25",village)) %>%
  mutate(village=ifelse(village %in% c("Ramawadagi", "ramawadagi","Ramavadagi","RAMAWADAGI","Ramawadgi" ),"26",village)) %>%
  mutate(village=ifelse(village %in% c("Revadihal","revadihal"),"27",village)) %>%
  mutate(village=ifelse(village %in% c("Turamari","turamari"),"28",village)) %>%
  mutate(village=ifelse(village %in% c("Veerapur","virapur","Virapur"),"29",village)) %>%
  mutate(village=ifelse(village %in% c("Yadahalli","yadahalli"),"30",village)) %>%
  mutate(village=ifelse(village %in% c("Bannihatti"),"31",village)) %>%
  mutate(village=ifelse(village %in% c("Budihal","budihal"),"32",village)) %>% 
  mutate(village=ifelse(village %in% c("Chatnihal","chatnihal"),"33",village)) %>% 
  mutate(village=ifelse(village %in% c("Gadisunkapur","gadisunkapur"),"34",village)) %>% 
  mutate(village=ifelse(village %in% c("Hirebadawadgi" ,"hirebadawadagi","Hirebadawadagi","HIrebadawadagi"),"35",village)) %>%
  mutate(village=ifelse(village %in% c("Hirehunakunti" ,"hirehunkunti","Hirehunkunti"),"36",village)) %>%
  mutate(village=ifelse(village %in% c("Hullalli","hullalli"),"37",village)) %>%         
  mutate(village=ifelse(village %in% c("Jambaladinni","jambaladinni"),"38",village)) %>%
  mutate(village=ifelse(village %in% c("Koppa","koppa"),"39",village)) %>%
  mutate(village=ifelse(village %in% c("Malagihal","malagihal"),"40",village)) %>%
  mutate(village=ifelse(village %in% c("Nidasanur","nidasanur","Nidasanoor"),"41",village)) %>%
  mutate(village=ifelse(village %in% c("Tumba","thumba","Thumba"),"42",village)) %>%
   mutate(n())
 
 table(AB$village)
 
 