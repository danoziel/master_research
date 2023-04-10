#  Midline 2022 databases, new survey - started on December 19, 2022\
library(dplyr)
library(haven)
library(readr)
library(tidyverse)
library(kableExtra)
remotes::install_github("haozhu233/kableExtra")

Adt3
Adt4
Adt1101
Adt1401
Adt1801
Adt2101

# ramtal_groups ----
# "id","layer","distance_km","around_boundary","in_out","south_north" 

YR_Ramthal_Data_Entry_2_stata13 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/YR_Ramthal_Data_Entry_2_stata13.dta")

ramtal_groups=YR_Ramthal_Data_Entry_2_stata13[ ,c("id","survey","in1_out0","layer","distance_km","around_boundary","south1_north0")]
rm(YR_Ramthal_Data_Entry_2_stata13) #heavy file- better to remove it

ramtal_groups$in_out = ifelse(ramtal_groups$in1_out0 ==1,"Inside","Outside")
ramtal_groups$south_north = ifelse(ramtal_groups$south1_north0==1,"Southern","Northern")

# Adt  ----

# file: Anonymized_Dataset_raw_27_Dec_2022
# contain  Adt_191222  <- Anonymized_Dataset_raw_19 Dec 2022.dta
#          Adt_211222  <- Anonymized_Dataset_raw_21 Dec 2022.dta
#          Adt_24_12_22 <-Anonymized_Dataset_raw_24 Dec 2022.dta

Adt_27_12_22<- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_27 Dec 2022.dta")
Adt_4_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_04 Jan 2023.dta")
Atd_7_1_23 <- Anonymized_Dataset_raw_07_Jan_2023
Adt_11_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_11 Jan 2023.dta")
Adt_14_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_14 Jan 2023.dta")
Adt_18_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_18 Jan 2023.dta")
Adt_21_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_21 Jan 2023.dta")
Adt_25_1_23  <- read_dta("~/master_research/DATAs/ramthal_data/MIDLINE2022/Anonymized_Dataset_raw_25 Jan 2023.dta")

Adt3 <- Adt_27_12_22 %>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)
Adt4 <- Adt_4_1_23%>%mutate(survy2022=ifelse(grepl('1',mm2), "Inside", "Outside") )%>% left_join (ramtal_groups) %>% rename(list2016=in_out)
Adt1101  <- Adt_11_1_23%>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)
Adt1401  <- Adt_14_1_23%>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)
Adt1801  <- Adt_18_1_23%>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)
Adt2101  <- Adt_21_1_23%>% mutate(survy2022=ifelse(grepl('1', mm2),  "Inside", "Outside")) %>% left_join (ramtal_groups)%>% rename(list2016=in_out)

## crosstab ----
crosstab1 <- Adt3 %>% select(id,list2016,survy2022)
crosstab2 <- Adt4 %>% select(id,list2016,survy2022)
crosstab3<- Adt1101 %>% select(id,list2016,survy2022)
crosstab4<- Adt1401 %>% select(id,list2016,survy2022)
crosstab5<- Adt1801 %>% select(id,list2016,survy2022)
crosstab6<- Adt2101 %>% select(id,list2016,survy2022)

crosstab <- rbind(crosstab1,crosstab2,crosstab3,crosstab4,crosstab5,crosstab6) %>% 
  mutate(list2016=case_when(is.na(list2016) ~ survy2022,TRUE ~ list2016)) %>% 
  mutate(COUNT=1)
rm(crosstab1,crosstab2,crosstab3,crosstab4,crosstab5,crosstab6)
library(sjPlot)
sjPlot::tab_xtab(var.row = crosstab$list2016 , 
                 var.col = crosstab$survy2022, 
                 title = "HH inside & outside the project\n reality(list 2016) Vs. self-reporting (survey2022)", 
                 show.row.prc = TRUE)









# ---- CULTIVATION_short separate tab---- 

		

# ---- ROSTER_short      ---- 

# R1	How many household members live in this house?	
A <- Adt3 %>% select (id,starts_with("r1_") ) # Not found

# R2	What is the household member's name?
A <- Adt3 %>% select (id,starts_with("r2_") )   

# R3	What is the household member status ?
A <- Adt3 %>% select (id,starts_with("r3_") )   

# R4	What is their gender?
A <- Adt3 %>% select (id,starts_with("r4_") )   

# R5	What is their age? (Years)    
A <- Adt3 %>% select (id,starts_with("r5_") )   

# R6	What is their relationship to the head of household?
A <- Adt3 %>% select (id,starts_with("r6_") )   %>% 
  filter(is.na(r6_1))

# R7 Are they literate?
A <- Adt3 %>% select (id,starts_with("r7_") )   

# R8  What is their educational level?
A <- Adt3 %>% select (id,starts_with("r8_") )   

# R9 Are they currently enrolled in school?
A <- Adt3 %>% select (id,starts_with("r9_") )   

# R10 In what grade are they currently enrolled?
A <- Adt3 %>% select (id,starts_with("r10_") )   

# R11 Is the institution public or private?
A <- Adt3 %>% select (id,starts_with("r11_") )   

# R12 What are the annual tuition fees?
A <- Adt3 %>% select (id,starts_with("r12_") )   

# R13	What is their current marital status?
A <- Adt3 %>% select (id,starts_with("r13_") )   

#RC1	What are their main income activities most of the time in the last 2 years?
A <- Adt3 %>% select (id,starts_with("rc1_") )   

#RC2	Where do they reside most of the time in the last 2 years?
A <- Adt3 %>% select (id,starts_with("rc2_") )   

#RC3  Where do they work most of the time in the last 2 years?
A <- Adt3 %>% select (id,starts_with("rc3_") )   

# R26 Since 2016, has [member's name] migrated from the village for work for a period of 6 months or more?
A <- Adt3 %>% select (id,starts_with("r26_") )   

# R27	How long they left between 2016 and now? 
A <- Adt3 %>% select (id,starts_with("r27_") )   %>% 
  select(where(~!all(is.na(.x))))

# R28 Does the [member's name] send money back to the household?
A <- Adt3 %>% select (id,starts_with("r28_") )   %>% 
  select(where(~!all(is.na(.x))))

# R30 Why did they come back?

A <- Adt3 %>% select (id,starts_with("r30_") )   %>% 
  select(-starts_with("r30_other")) %>% 
  select( -starts_with("r30_back")) 

# R31		When did they come back ? (YYYY)
A <- Adt3 %>% select (id,starts_with("r31_") )   %>% 
  
#   PREVIOUS HOUSEHOLD MEMBERS		
#R32 How many people, who previously lived in the household in the past 10 years now live elsewhere?
A <- Adt3 %>% select (id,starts_with("r32_") ) #NOT FOUND
  
#R33 What is their name?
A <- Adt3 %>% select (id,starts_with("r33_") ) #NOT FOUND

#R34 What is their gender?
A <- Adt3 %>% select (id,starts_with("r34_gender") )

#R35 What is their age? (Years)
A <- Adt3 %>% select (id,starts_with("r35_") )

#R36 What is their mobile number?

#R37 What is their relationship to the head of household?
AA <- Adt3 %>% select (id,starts_with("r37_") )

#R38 Where did they move to?
AA <- Adt3 %>% select (id,starts_with("r38_") )

#R39 Why did they move away?
AA <- Adt3 %>% select (id,starts_with("r39_") )

# New household members 		
# R47	 What is the new household member's name?
AA <- Adt3 %>% select (id,starts_with("r47_") ) #NOT FOUND

# R48	 Are they household members who left before 2016?
AA <- Adt3 %>% select (id,starts_with("r48_") )

# R49	 Why did they come back? and if they have never lived in a household before, why are they joining?
AA <- Adt3 %>% select (id,starts_with("r49_") )

# R50	 What is their relationship to the head of household?
AA <- Adt3 %>% select (id,starts_with("r50_") )

# R51	 What is their gender?
AA <- Adt3 %>% select (id,starts_with("r51_") )

# R52	 What is their age? (Years)
A <- Adt3 %>% select (id,starts_with("r52_") )

# R53	 Are they literate?
AA <- Adt3 %>% select (id,starts_with("r53_") )

# RN1 What are their main income activities most of the time in the last 2 years?
AA <- Adt3 %>% select (id,starts_with("rn1_") )

# RN2 Where do they reside most of the time in the last 2 years?
AA <- Adt3 %>% select (id,starts_with("rn2_1") )

# RN3 Where do they work most of the time in the last 2 years?
A <- Adt3 %>% select (id,starts_with("rn3_") )

# ---- ASSETS            ---- 
# ---- INCOME            ---- 
# ---- SOCIAL CAPITAL    ---- 
# ---- WATER separate tab---- 



# VARS ----
RG <- ramtal_groups[,c(1,7)]

Adt4 %>%select(id,mm5,mm9,mm10, mw1b,mw1c, mw4,mw5,mw6) %>% 
  kbl() %>% kable_paper()

dt <- Adt3 %>%select(id,Grp,mm5,mm9,mm10, mw1b,mw1c, mw4,mw5,mw6)
dt_dt <- Adt %>%select(id,Grp,mm5,mm9,mm10, mw1b,mw1c, mw4,mw5,mw6)%>%
  bind_rows(dt) %>% bind_rows(dt4)

dtt <- left_join (dt_dt,RG)
dtt <- inner_join (dt_dt,RG) %>% 
  mutate(TF=ifelse(Grp=="treatment" & in_out=="Inside", TRUE,
                   ifelse(Grp=="control" & in_out=="Outside", TRUE,
                         FALSE )))%>%
  select(id,Grp,in_out,everything())

