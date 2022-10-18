library(tidyverse)

library(haven)
Irrigation_Cultivation_Section_Long_Form_20180622 <- read_dta("C:/Users/Dan/master_research/DATAs/ramthal_data/Ramthal Midline/Irrigation_Cultivation Section_Long Form_20180622.dta")

# Databases created in this Rscript ----

# plot list 12.10.2022 (crop, size, survey,hissa)
PLOT_list_ifmr_2018_2016 <- read.csv("~/master_research/DATAs/ramthal_data/PLOT_list_ifmr_2018_2016.csv")

# Midline18 plot list
PLOT_list_ifmr_2018<- read.csv("~/master_research/DATAs/ramthal_data/PLOT_list_ifmr_2018_2016.csv")

# baseline PLOT who doesn't appear in PLOT_list_ifmr_2018
PLOT_b16a<- read.csv("~/master_research/DATAs/ramthal_data/PLOT_b16a.csv")

#first plot list (survey,hissa)
ifmr_hissa_2016_2018<- read.csv("~/master_research/DATAs/ramthal_data/ifmr_hissa_2016_2018.csv")


#==== ifmr_mid_2018 =====

plot_ifmr_2018a <- 
  ifmr_mid_2018 %>% 
  select("id","a5","a13","d3",                         
         starts_with("D3_"),                                       # srvy+hissa 4318
         matches("d41_"),                                          # crop       3721
         starts_with("d15_acre_"),starts_with ("d15_guntas_")) %>% # plot size  4007
  rename(Id=id,village=a5,farmer_name=a13,total_plots=d3) 
  

## srvy/hisa No. ------

#d3_1_hissa
plot_ifmr_2018b <- plot_ifmr_2018a%>% 
  select( -starts_with(c("d3_1_hissa_nu_yesno_","d3_1_status_","d3_1_hissa_nu_correct_")),
         -ends_with("cb")) %>% 
  select(1:14) %>% 
  gather("key1", "hissa_srvy_no",5:14)
plot_ifmr_2018b$plot <- 
  c(rep("plot_1",1702),rep("plot_2",1702),rep("plot_3",1702), rep("plot_4",1702),
    rep("plot_5",1702), rep("plot_6",1702),rep("plot_7",1702),rep("plot_8",1702),
    rep("plot_9",1702), rep("plot_10",1702))
                          
#d3_1_hissa_nu_correct_
plot_ifmr_2018c <- plot_ifmr_2018a%>% 
  select(Id,village,farmer_name,total_plots,
         starts_with("d3_1_hissa_nu_correct_")) %>%
  add_column(d3_1_hissa_nu_correct_9="",d3_1_hissa_nu_correct_10="") %>% 
  gather("key2", "hissa_srvy_correct",5:14) 

plot_ifmr_2018c$plot <- 
  c(rep("plot_1",1702),rep("plot_2",1702),rep("plot_3",1702), rep("plot_4",1702),
    rep("plot_5",1702), rep("plot_6",1702),rep("plot_7",1702),rep("plot_8",1702),
    rep("plot_9",1702), rep("plot_10",1702))

plot_ifmr_2018cd <- inner_join (plot_ifmr_2018b,plot_ifmr_2018c) 

plot_ifmr_2018cd[plot_ifmr_2018cd==""] <- NA

plot_ifmr_2018cd <-
  plot_ifmr_2018cd %>%  
  mutate(srvy_hissa=ifelse(is.na (hissa_srvy_correct),hissa_srvy_no,hissa_srvy_correct) ) %>% 
  mutate(baseline_incorrect_plot_number=ifelse(hissa_srvy_correct==srvy_hissa,hissa_srvy_no,"") ) %>% 
  select(-starts_with(c("hissa","key"))) # %>% filter(!is.na(srvy_hissa)) 4318


rm(plot_ifmr_2018b,plot_ifmr_2018c,plot_ifmr_2018d)

40309301


## plot size ----
# d15_acre_
plot_ifmr_2018e <- plot_ifmr_2018a %>% 
  select(Id,village,farmer_name,total_plots,                                          
         starts_with("d15_acre_")) %>% 
  select(-ends_with("cb")) %>% 
  gather("key1", "plot_size_acre",5:14)
plot_ifmr_2018e$plot <- 
  c(rep("plot_1",1702),rep("plot_2",1702),rep("plot_3",1702), rep("plot_4",1702),
    rep("plot_5",1702), rep("plot_6",1702),rep("plot_7",1702),rep("plot_8",1702),
    rep("plot_9",1702), rep("plot_10",1702))

# d15_guntas_
plot_ifmr_2018f <- plot_ifmr_2018a %>% 
  select(Id,village,farmer_name,total_plots,
         starts_with ("d15_guntas_")) %>% 
  select(-ends_with("cb"))%>% 
  gather("key2", "plot_size_guntas",5:14) 
plot_ifmr_2018f$plot <- 
  c(rep("plot_1",1702),rep("plot_2",1702),rep("plot_3",1702), rep("plot_4",1702),
    rep("plot_5",1702), rep("plot_6",1702),rep("plot_7",1702),rep("plot_8",1702),
    rep("plot_9",1702), rep("plot_10",1702))

plot_ifmr_2018ef <- inner_join (plot_ifmr_2018e[,-5] ,plot_ifmr_2018f[,-5] ) 

# plot_ifmr_2018ef <- plot_ifmr_2018ef %>% filter(plot_size_guntas>=0) 4007

rm(plot_ifmr_2018e,plot_ifmr_2018f)

# crop plot_1-plot_5 ----

plot_ifmr_2018g <- plot_ifmr_2018a %>% 
  select(Id,village,farmer_name,total_plots,  matches("d41_")) 
plot_ifmr_2018g[plot_ifmr_2018g=="-666"] <- NA
plot_ifmr_2018g[plot_ifmr_2018g=="-333"] <- NA

plot_1_ifmr_2018 <- plot_ifmr_2018g %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE) %>% 
  unite("S3_crop", 15:18, na.rm = TRUE, remove = FALSE) 
plot_2_ifmr_2018 <- plot_1_ifmr_2018 %>%   select(1:4,20:last_col())
plot_1_ifmr_2018 <- plot_1_ifmr_2018 %>% select(1:5,10,15) %>% mutate(plot="plot_1")

plot_2_ifmr_2018 <- plot_2_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE) %>% 
  unite("S3_crop", 15:18, na.rm = TRUE, remove = FALSE) 
plot_3_ifmr_2018 <- plot_2_ifmr_2018 %>%   select(1:4,20:last_col())
plot_2_ifmr_2018 <- plot_2_ifmr_2018 %>% select(1:5,10,15) %>% mutate(plot="plot_2")

plot_3_ifmr_2018 <- plot_3_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE) %>% 
  unite("S3_crop", 15:18, na.rm = TRUE, remove = FALSE) 
plot_4_ifmr_2018 <- plot_3_ifmr_2018 %>%   select(1:4,20:last_col())
plot_3_ifmr_2018 <- plot_3_ifmr_2018 %>% select(1:5,10,15) %>% mutate(plot="plot_3")

plot_4_ifmr_2018 <- plot_4_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE) %>% 
  unite("S3_crop", 15:18, na.rm = TRUE, remove = FALSE) 
plot_5_ifmr_2018 <- plot_4_ifmr_2018 %>%   select(1:4,20:last_col())
plot_4_ifmr_2018 <- plot_4_ifmr_2018 %>% select(1:5,10,15) %>% mutate(plot="plot_4")

plot_5_ifmr_2018 <- plot_5_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE) %>% 
  unite("S3_crop", 15:18, na.rm = TRUE, remove = FALSE) 
plot_6_ifmr_2018 <- plot_5_ifmr_2018 %>%   select(1:4,20:last_col())
plot_5_ifmr_2018 <- plot_5_ifmr_2018 %>% select(1:5,10,15) %>% mutate(plot="plot_5")
  
# crop 6-10 ----

plot_06_ifmr_2018 <- plot_6_ifmr_2018

plot_6_ifmr_2018 <- plot_6_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE)
plot_7_ifmr_2018 <- plot_6_ifmr_2018    %>% select(1:4,15:last_col())
plot_6_ifmr_2018 <- plot_6_ifmr_2018 %>% mutate(S3_crop="") %>% select(1:5,S3_crop,10) %>% mutate(plot="plot_6")

plot_7_ifmr_2018 <- plot_7_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE)
plot_8_ifmr_2018 <- plot_7_ifmr_2018    %>% select(1:4,15:last_col())
plot_7_ifmr_2018 <- plot_7_ifmr_2018 %>% mutate(S3_crop="") %>% select(1:5,S3_crop,10) %>% mutate(plot="plot_7")

plot_8_ifmr_2018 <- plot_8_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE)
plot_9_ifmr_2018 <- plot_8_ifmr_2018    %>% select(1:4,15:last_col())
plot_8_ifmr_2018 <- plot_8_ifmr_2018 %>% mutate(S3_crop="") %>% select(1:5,S3_crop,10) %>% mutate(plot="plot_8")

plot_9_ifmr_2018 <- plot_9_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE)
plot_10_ifmr_2018 <- plot_9_ifmr_2018    %>% select(1:4,15:last_col())
plot_9_ifmr_2018 <- plot_9_ifmr_2018 %>% mutate(S3_crop="") %>% select(1:5,S3_crop,10) %>% mutate(plot="plot_9")

plot_10_ifmr_2018 <- plot_10_ifmr_2018 %>% 
  unite("S1_crop", 5:8, na.rm = TRUE, remove = FALSE) %>% 
  unite("S2_crop", 10:13, na.rm = TRUE, remove = FALSE)
plot_10_ifmr_2018 <- plot_10_ifmr_2018 %>% mutate(S3_crop="") %>% select(1:5,S3_crop,10) %>% mutate(plot="plot_10")

# ----plot_1_10_ifmr_2018----

GH <-rbind (plot_1_ifmr_2018,plot_2_ifmr_2018,plot_3_ifmr_2018,plot_4_ifmr_2018,
         plot_5_ifmr_2018,plot_6_ifmr_2018,plot_7_ifmr_2018,plot_8_ifmr_2018,
         plot_9_ifmr_2018,plot_10_ifmr_2018) # %>% filter(Id=="100036") 
# filter(GH,S1_crop>0 | S2_crop>0 | S3_crop>0) # crop  3721

EF <- plot_ifmr_2018ef%>%    # filter(Id=="100036") # plot size  4007
CD <- plot_ifmr_2018cd %>%   #filter(Id=="100036")  # srvy+hissa 4318

# PLOT_list <- inner_join (CD,EF) %>%  full_join(GH)   # 17020
PLOT_list <- full_join (CD,EF) %>%  full_join(GH) # 17270

PLOT_list <- PLOT_list %>% mutate(delete=ifelse(plot == "plot_1","stay1",
                                                ifelse(srvy_hissa >-1000 ,"stay2"))) %>% 
  filter(!is.na(delete)) %>% 
  select(Id,village,farmer_name,total_plots,plot,S1_crop,S2_crop,S3_crop,
         plot_size_acre,plot_size_guntas,srvy_hissa,baseline_incorrect_plot_number) %>% 
  rename(rabi_crop=S1_crop,kharif_crop=S2_crop, summer_crop=S3_crop)

4686
PLOT_b16a <- 
  ifmr_hissa_2016_2018 %>% filter(is.na(HH_name_2018)| HH_name_2018=="") %>% 
  kable() %>% kable_classic()

PLOT_list_ifmr_2018 <- PLOT_list
write.csv(PLOT_list_ifmr_2018,"~/master_research/DATAs/ramthal_data/PLOT_list_ifmr_2018.csv", row.names = FALSE)

#===== ifmr_base_2016 ======

AA <- 
  ifmr_base_2016 %>% 
  select("Id",A17,"A9","D3",                         
         starts_with("D4_"),                         # srvy+hissa 
         starts_with("D24_"),                        # crop       
         starts_with("D27_")) %>%                    # plot size
  rename(village=A9,farmer_name=A17,total_plots=D3) %>%
  right_join(PLOT_b16a) 

D24_<- AA %>% select(Id,farmer_name,hissa_srvy,starts_with("D24_"),-matches("D24_3"))

write.csv(D24_, file = "C:/Users/Dan/Documents/master_research/DATAs/D24_.csv", row.names=FALSE)
library(readr)
D24_fo <- read_csv("~/master_research/DATAs/D24_fo.csv")

D27_<- AA %>% select(Id,farmer_name,hissa_srvy,village,total_plots,D27_1_acer_1_1 ,D27_1_guntas_1_1) %>% 
  cbind (D24_fo[,3:5] ) %>% 
  rename(plot_size_acre=D27_1_acer_1_1,
         plot_size_guntas=D27_1_guntas_1_1,
         srvy_hissa=hissa_srvy ) %>% 
  mutate(summer_crop="",
         baseline_incorrect_plot_number="") %>% 
  select("Id","village","farmer_name","total_plots","plot",
         "rabi_crop","kharif_crop","summer_crop", 
         "plot_size_acre","plot_size_guntas" ,
         "srvy_hissa","baseline_incorrect_plot_number")


PLOT_list_ifmr_2018_2016 <- rbind(PLOT_list,D27_) %>% 
  arrange(Id)

write.csv(PLOT_list_ifmr_2018_2016, file = "C:/Users/Dan/Documents/master_research/DATAs/PLOT_list_ifmr_2018_2016.csv", row.names=FALSE)

# budihal,gadisunkapur,gorabal,hachanur,hulgera,hullalli,jalakamaladini,konnur,nagur


write.csv(FILE,"C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/FILE.csv", row.names=FALSE)
FILE <- read.csv("~/master_research/DATAs/ramthal_data/FILE.csv")
