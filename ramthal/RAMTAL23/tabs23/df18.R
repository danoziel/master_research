#| THIS R SCRIPT [df18.R] is data-frames/sets/bases of "2018 midline survey"
#| 🟠MIDELINE 2018  | rmtl_midline2018 #= mid2018_RMTL


#| df16.R # scrip of data-frames/sets/bases of "2016 baseline survey"
#🟡BASELINE 2016| rmtl_baseline2016 #= baseline_RMTL

#| DF_22.R # scrip of data-frames/sets/bases of "2022 midline survey"
#🟣MIDELINE 2022| rmtl_srvy22 #= a_rmtl_srvy22 


# MIDATA----

Irrigation_Midline_Clean_20180713 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180713.dta")
# 1621  20,033

Irrigation_Midline_Clean_20180709 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180709.dta")
# 1557  20,972

Irrigation_Midline_Clean_20180619 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180619.dta")
#  1,088 × 10,441

Irrigation_Midline_Clean_20180613<- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/roster18/Irrigation_Midline_Clean_20180613.dta")
# 965  10,079


Irrigation_Cultivation2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Irrigation_Cultivation Section_Long Form_20180622.dta")
# [1200 HH] 3122 1904


YR_Ramthal_Data_Entry <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry.dta")
# A tibble: 1,811 × 279    # 8/5/2018

YR_Ramthal_Data_Entry_2_Copy<- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2 - Copy.dta")
# A tibble: 737 × 8,039    # 7/6/2018

YR_Ramthal_Data_Entry_2_stata12 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata12.dta")
# A tibble: 126 × 4,630    # 11/5/2018

# Irrigation_Midline_Clean_with Callbacks  former "ifmr_mid_2018"   
Irrigation_Midline_Clean_with_Callbacks<- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Clean Data/Irrigation_Midline_Clean_with Callbacks.dta")
# A tibble: 1,702 × 7,990  # 1/10/2018

YR_Ramthal_Data_Entry_2 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2.dta")
# A tibble: 1,702 × 7,853  # 8/4/2019

YR_Ramthal_Data_Entry_2_stata13 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata13.dta")
# A tibble: 1,702 × 7,853  # 8/4/2019

rmtl_midline2018 = YR_Ramthal_Data_Entry_2_stata13
rm(YR_Ramthal_Data_Entry_2_stata13)
# rmtl_midline2018 ----

a_sample=a_sample %>% rename(id=hh_id)

#🟩  IRRI  HH FREQ | d29 d13A D13_ [HH wise]                             ----

######### last 2 years 
#| [D29]  # Has this plot been irrigated at least once during the last 2 years?

d29 <- rmtl_midline2018 %>% select(in1_out0,id, starts_with("d29"),-c("d29_share","d29_lease","d29_number"))
d29[d29==2] <- 0
d29[is.na(d29)] <- 0
d29 <- d29 %>% mutate(d_ = rowSums(.[names(.)[3:11]], na.rm = T))%>%
  mutate(D29=ifelse(d_>0,1,0 ))

d29 %>% count(in1_out0,D29) %>%group_by(in1_out0) %>%  mutate(n/sum(n))  


######## past year
#| [D13_A]	# Has this plot been irrigated at least once in the past year?

d13A <- rmtl_midline2018 %>% select(in1_out0 ,id, starts_with("d13_a_")) 
d13A[d13A==2] <- 0
d13A[is.na(d13A)] <- 0
d13A <- d13A %>% 
  mutate(d_ = rowSums(.[names(.)[3:12]], na.rm = T)) %>% 
  mutate(cb = rowSums(.[names(.)[13:15]], na.rm = T)) %>% 
  mutate(dcb = d_+cb )

D13_<- d13A %>% select(in1_out0 ,id,d_,cb,dcb) %>%
  mutate(D13a=ifelse(d_>0,1,0 )) %>% 
  mutate(D13A=ifelse(dcb>0,1,0 ))

D13_ %>% count(in1_out0,D13a) %>%group_by(in1_out0) %>%  mutate(n/sum(n))  
#D13_ %>% count(in1_out0,D13A) %>%group_by(in1_out0) %>%  mutate(n/sum(n))  


#🟩  IRRI SOURCES |   ml18_source_irrigate      ----
#| D14_A	"What sources? (Mark all)"
#1 Canal  #2	Tank  #3	Open well  #4	River(/Pond/Lake)  #5	Bore well  # -888	Other, specify

d14A <- rmtl_midline2018 %>% select(id, starts_with("d14_a_"))

d14a <- rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d14_a_"), -c("d14_a_1_cb","d14_a_2_cb","d14_a_3_cb" )) %>% mutate(irri1=100)
d14a$d14_a_1[d14a$d14_a_1=="3,5"] <-"5"
d14a <- d14a %>% mutate(irri1= ifelse(d14_a_1 %in% c("1", "2", "3","4","5" ),d14_a_1, d14_a_os_1))

d14a$irri1[d14a$irri1 %in% c( "KALUVE", "KOLAVE BHAVI")] <- "1"
d14a$irri1[d14a$irri1 %in% c("BOREWELL", "BORVELL")] <- "5"
d14a$irri1[d14a$irri1 %in% c("RIVER","DRIP", "DRIP IRRIGATION","PAIPA  MULAKA","PIPLINE")] <- "4"
d14a$irri1[!d14a$irri1 %in% c("1", "2", "3","4","5")] <- "0"
#d14a %>% count(irri1)

#d14a %>% count(d14_a_2)
d14a <- d14a %>% mutate(irri1= ifelse(irri1 =="0",d14_a_2, irri1))
#d14a %>% filter(is.na(d14_a_2),is.na(irri1))%>% count(d14_a_os_2)
d14a$irri1[is.na(d14a$irri1) & d14a$d14_a_os_2 %in% c("DRIP", "DRIP RAMTAL")] <- "4"
d14a$irri1[is.na(d14a$irri1) & d14a$d14_a_os_2 %in% c("BOREWELL")] <- "5"
d14a$irri1[is.na(d14a$irri1) & d14a$d14_a_os_2 %in% c("3")] <- "3"

#d14a %>% filter(is.na(irri1))%>% count(d14_a_3)
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_3, irri1))
#d14a %>% filter(is.na(d14_a_3),is.na(irri1))%>% count(d14_a_os_3)
d14a$irri1[is.na(d14a$irri1) & d14a$d14_a_os_3 %in% c("3")] <- "3"

#d14a %>% count(d14_a_4)
d14a$d14_a_4[d14a$d14_a_4=="3,1"] <- "1"
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_4, irri1))
d14a$irri1[!d14a$irri1 %in% c("1", "2", "3","4","5")] <- NA
# d14_a_4=d14a %>% filter(is.na(irri1))
# d14_a_4 %>% count(d14_a_os_4)
d14a$irri1[is.na(d14a$irri1) & d14a$d14_a_os_4 %in% c("DRIP")] <- "4"

d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_5, irri1))
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_6, irri1))
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_7, irri1))
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_8, irri1))
d14a <- d14a %>% mutate(irri1= ifelse(is.na(irri1),d14_a_10, irri1))
d14a$irri1[is.na(d14a$irri1)] <- 0

ml18_source_irrigate <- d14a %>% mutate(irri_source_num=irri1) %>% rename(irri_source=irri1)

#1 Canal  #2	Tank  #3	Open well  #4	River(/Pond/Lake)  #5	Bore well  # -888	Other, specify

ml18_source_irrigate$irri_source[ml18_source_irrigate$irri_source_num %in% c("1", "2") ] <- "Canal"
ml18_source_irrigate$irri_source[ml18_source_irrigate$irri_source_num=="3" ] <- "Openwell"
ml18_source_irrigate$irri_source[ml18_source_irrigate$irri_source_num=="4" ] <- "River(/pond)"
ml18_source_irrigate$irri_source[ml18_source_irrigate$irri_source_num=="5" ] <- "Borewell"





#🟩  IRRI METHODS | d15A |ml18_irri_methods  [HH wise]    ----
# D15_A	What methods?   DF: ml18_irri_methods
"d15_a_:  1 Flood | 2 Furrows | 3 Drip | 4 Sprinkler | 5 Manual | 6 Hose | -888 Other (specify)"

d15A <- rmtl_midline2018 %>% select(in1_out0,id, starts_with("d15_a_")) %>% mutate(irri=100)
d15A=d15A[,c(1:4,25,5:24)] %>% mutate(irri=ifelse( d15_a_1>0,d15_a_1,NA ))

d15A$irri[d15A$d15_a_os_1 == "DRIP IRRIGATION"] <- 3
d15A$irri[d15A$d15_a_os_1 == "PIPELINE" ] <- 3
d15A$irri[d15A$d15_a_os_1 == "1" & d15A$d15_a_1==-888 ] <- 1
d15A$irri[d15A$d15_a_os_1 == "BOREWELL" ] <- 2

d15A %>% count(irri)
d15A_2=d15A %>% filter (is.na(irri))
d15A_2 %>% count(d15_a_2)

d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_2>0, d15A$d15_a_2 ,d15A$irri)
d15A$irri[d15A$d15_a_os_2 == "DRIP" & d15A$d15_a_2==-888] <- 3
d15A$irri[d15A$d15_a_os_2 == "BOREWELL" & d15A$d15_a_2==-888] <- 2

d15A_2=d15A %>% filter (is.na(irri))
d15A_2 %>% count(d15_a_3)

d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_3>0, d15A$d15_a_3 ,d15A$irri)
d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_4>0, d15A$d15_a_4 ,d15A$irri)
d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_5>0, d15A$d15_a_5 ,d15A$irri)
d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_6>0, d15A$d15_a_6 ,d15A$irri)
d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_7>0, d15A$d15_a_7 ,d15A$irri)
d15A$irri=ifelse(is.na(d15A$irri) & d15A$d15_a_10>0, d15A$d15_a_10 ,d15A$irri)

d15A$irri[d15A$irri == "2,4" ] <- 2
d15A$irri[d15A$irri == "3,2" ] <- 3

d15A$irri[d15A$d15_a_2 == "3" ] <- 3
d15A$irri[d15A$d15_a_5 == "3" ] <- 3
d15A$irri[d15A$d15_a_6 == "3" ] <- 3
# NO drip in d15_a_3 d15_a_4 d15_a_7 d15_a_10

d15A$irri[is.na(d15A$irri) ] <- 0

ml18_irri_methods = d15A %>% select(in1_out0, id, irri,farmers_hh) %>% 
  mutate(irri_method =irri)

ml18_irri_methods %>% count(in1_out0 , irri) %>% 
  group_by(in1_out0) %>% mutate(grp=n/sum(n)) %>% mutate_at(4,round,3)

ml18_irri_methods = ml18_irri_methods %>% 
  mutate(irri_method =irri)

ml18_irri_methods$irri_method[ml18_irri_methods$irri_method==1] <- "flood"
ml18_irri_methods$irri_method[ml18_irri_methods$irri_method==2] <- "furrows"
ml18_irri_methods$irri_method[ml18_irri_methods$irri_method==3] <- "drip"
ml18_irri_methods$irri_method[ml18_irri_methods$irri_method==4] <- "sprinkler"
ml18_irri_methods$irri_method[ml18_irri_methods$irri_method %in% c(5,6) ] <- "hose"
ml18_irri_methods$irri_method[ml18_irri_methods$irri_method==0] <- "rain"





#🟩  IRRI land acre [HH-season] ----

d100= 
  rmtl_midline2018 %>% select(farmers_hh,id,
                              d3, # currently own?
                              d4,   # own in April 2016?
                              d5 ,  # since April 2016
                              d6  #currently lease?
  )

#  currently OWNED
d3_ <-  # Survey/hissa number
  rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d3_" ))
d15 <- # Area of plot
  rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d15_acre_" ),starts_with("d15_guntas_"))

# currently LEASED IN
d23 <-  # Survey/hissa number
  rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d23" ))
d25 <- # Area of plot
  rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d25" ))


d <- rmtl_midline2018 %>% select(farmers_hh,id, starts_with("D13_A" ))

d <- rmtl_midline2018 %>% select(farmers_hh,id, starts_with("d16" ))


#|🟦 ----
#|🟦  I16	"Is your land taking part in the Ramthal project? " (
  #|Large Pipes installed in the field)

#|🟦  I17 "Has any drip irrigation infrastructure been installed on your land?" 
#|(Small black pipes laid over the ground and used for drip irrigation)

#🟦 I15.a	Were you offered irrigation water -Y/N
#🟦 I15.b	If yes, Did you use water offered
#🟦 I15.c	If yes, When did you start using the water




