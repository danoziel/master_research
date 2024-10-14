library(dplyr)
library(tidyverse)
library(haven)
library("stringr") #"str_replace"

library(rstatix) # ttest "add_significance"
library(rempsyc) # ttest # nice_table
library(kableExtra )



# July 23 VISIT farmers_interviews_july23----

library(readxl)
visit_july23 <- 
  read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/field visit july 2023/List of farmers for interviews.xlsx", 
             sheet = "interviws")

names(visit_july23)

farmers_july23 <-   
  visit_july23 %>% 
  rename (farmer_mentioned_ValveProximity = `תזת הקירבה לשיבר`)
filter(!UID == "out of list") %>% 
  mutate(hh_id = as.numeric (UID)) %>% 
  left_join(frmrs_b4u) 

farmers_july23$mm10[farmers_july23$mm10  == 1] <- "1 never happened"
farmers_july23$mm10[farmers_july23$mm10  == 2] <- "2 once a season"
farmers_july23$mm10[farmers_july23$mm10  == 3] <- "3 several times a season"
# Replacing 4 with 5 from the survey, to create a logical value scale
farmers_july23$mm10[farmers_july23$mm10  == 4] <- "5 constant basis"
farmers_july23$mm10[farmers_july23$mm10  == 5] <- "4 Some seasons"

--------------------------------------------------------------------------------
  # 20/02/2024 ----
names(farmers_july23)

fj=
  farmers_july23 %>% 
  select(farmers_hh,hh_id, farmer_mentioned_ValveProximity,10 ,useYN,mm9,mm10 ,irri_source ,hh_6methods,water_usage) %>% 
  mutate(mm9=ifelse(mm9==-999,10,mm9)) %>% 
  filter(farmer_mentioned_ValveProximity==1)

--------------------------------------------------------------------------------
  
  
  farmers_july23_01 <- farmers_july23%>% select(id, infrastructureYN,mm10,mm9,useYN,mm5)

add0 <- data.frame( water_usage= c("use","never_use","use","never_use","never_use"),
                    mm9= c(3,4,9,10,18),
                    n= c(0,0,0,0,0))

farmers_july23_01 %>%
  filter(mm9>0) %>% 
  mutate(water_usage= ifelse(mm5==1, "use","never_use")) %>% 
  count(water_usage ,mm9) %>% 
  bind_rows(add0) %>% 
  
  # mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=mm9, y=n, fill=water_usage)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c( "#E69F00","skyblue")) +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9,10,18,20))+
  theme_linedraw()+
  labs(title = "Usage water / Location on the pipeline",x ="No. farmers before you", y = "Total No. of farmers")+
  # labs(subtitle = "Sample:Infrastructure installed", caption = "n=667")+ #Infrastructure
  labs(subtitle = "July 2023 visit sample:",  caption = "n=18")  #Ramthal


# June 24 VISIT farmers_interviews_june23----


farmers_interviews_june24=
  rmtl_baseline2016 %>% select(hh_id, A9,A14,A17,A18,A19) 
names(farmers_interviews_june24) <- c('hh_id', 'village', 'phone', 'frmer_name','spouse','father')

water_usage=
  rmtl_InOut_groups %>% select(hh_id, in1_out0 ,infrstr_17_21 , waterIR_17_21, hh_irri_yrs_17_21, hh_drip_yrs_17_21 )

list_farmer_visit_june24=
  right_join(farmers_interviews_june24,water_usage )

write.csv(list_farmer_visit_june24, file ="C:/Users/Dan/OneDrive - mail.tau.ac.il/list_farmer_visit_june24.csv", row.names=FALSE)






