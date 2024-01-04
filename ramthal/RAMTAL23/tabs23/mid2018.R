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

mid2018_RMTL = YR_Ramthal_Data_Entry_2_stata13
rm(YR_Ramthal_Data_Entry_2_stata13)
# mid2018_RMTL ----

a_sample=a_sample %>% rename(id=hh_id)

#|🟩 cult


###### 29	Has this plot been irrigated at least once during the last 2 years? ----
d29 <- mid2018_RMTL %>% select(in1_out0,id, starts_with("d29"),-c("d29_share","d29_lease","d29_number"))
d29[d29==2] <- 0
d29[is.na(d29)] <- 0
d29 <- d29 %>% mutate(d_ = rowSums(.[names(.)[3:11]], na.rm = T))%>%
  mutate(D29=ifelse(d_>0,1,0 ))

d29 %>% count(in1_out0,D29) %>%group_by(in1_out0) %>%  mutate(n/sum(n))  

d_3 <-d29 %>%t_test(D29 ~ in1_out0 , detailed = T)

###### D13_A	Has this plot been irrigated at least once in the past year? ----
d13A <- mid2018_RMTL %>% select(in1_out0 ,id, starts_with("d13_a_")) 
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

d_1 <-D13_ %>%t_test(D13A ~ in1_out0 , detailed = T)
#d_2 <-D13_ %>%t_test(D13a ~ in1_out0 , detailed = T)



# D14_A	What sources? (Mark all)
d14 <- mid2018_RMTL %>% select(id, starts_with("d14_a_"))


# methods DF: ml18_irri_methods ----
# D15_A	What methods?   DF: ml18_irri_methods
"d15_a_:  1 Flood | 2 Furrows | 3 Drip | 4 Sprinkler | 5 Manual | 6 Hose | -888 Other (specify)"

d15A <- mid2018_RMTL %>% select(in1_out0,id, starts_with("d15_a_")) %>% mutate(irri=100)
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

ml18_irri_methods = d15A

ml18_irri_methods %>%filter(!is.na(in1_out0))  %>% count(in1_out0 , irri) %>% 
  group_by(in1_out0) %>% mutate(grp=n/sum(n)) %>% mutate_at(4,round,3)












#|🟦 ----
#|🟦  I16	"Is your land taking part in the Ramthal project? " (
#|Large Pipes installed in the field)

#|🟦  I17 "Has any drip irrigation infrastructure been installed on your land?" 
#|(Small black pipes laid over the ground and used for drip irrigation)

#🟦 I15.a	Were you offered irrigation water -Y/N
#🟦 I15.b	If yes, Did you use water offered
#🟦 I15.c	If yes, When did you start using the water




