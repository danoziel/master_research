library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(kableExtra)
library(broom)

baseline_cefr_mooc_A.21.22 <- read_excel("~/master_research/DATAs/k2p_2022/CEFR_course/baseline_CEFR_course_21.22_semesterA.xlsx",sheet = "clean_data")
endline_cefr_mooc_A.21.22 <- read_excel("~/master_research/DATAs/k2p_2022/CEFR_course/endline_CEFR_course_21.22_semesterA.xlsx",sheet = "endline_clean_data")
distribution_CEFR_course <- read_excel("~/master_research/DATAs/k2p_2022/CEFR_course/endline_CEFR_course_21.22_semesterA.xlsx", sheet = "distribution")
#distribution_CEFR_course ----
df2 <- 
  distribution_CEFR_course %>% filter(database=="Baseline", var != "qa4_thinking", var != "qa6_initiative") %>%
  mutate(across(is.numeric, round, 2))   #%>% mutate(value = paste0(round(100 * value, 0), "%")) 

ggplot(data=df2, aes(x = var, y = value, fill = level)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=value), vjust=1.2, color="black",
            position = position_dodge(.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +
  labs(x = "Baseline")

df22 <- 
  distribution_CEFR_course %>% filter(database=="Endline", var != "qa4_thinking", var != "qa6_initiative") %>%
  mutate(across(is.numeric, round, 2))   #%>% mutate(value = paste0(round(100 * value, 0), "%")) 


  ggplot(data=df22, aes(x = var, y = value, fill = level)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=value), vjust=1.2, color="black",
              position = position_dodge(.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal() +
    labs(x = "Endline")




#ability_base_end_a2122 ----

baseline_a2122 <- baseline_cefr_mooc_A.21.22 %>% 
  select(gender,age,sector,status,childrens,course_typ,
         qa1_interaction,qa2_tech,qa3_knowledge,qa4_thinking,qa5_tools,qa6_initiative) %>% 
    add_column(database="Baseline")       

endline_a2122<- endline_cefr_mooc_A.21.22 %>%
  select(gender,age,sector,status,childrens,course_typ,
         qa1_interaction,qa2_tech,qa3_knowledge,qa4_thinking,qa5_tools,qa6_initiative) %>%
  add_column(database="Endline")

ability_7_13_a2122 <- bind_rows(baseline_a2122[,7:13],endline_a2122[,7:13])
ability_7_13_a2122[ability_7_13_a2122==1] <- NA
ability_7_13_a2122[ability_7_13_a2122==2] <- 1
ability_7_13_a2122[ability_7_13_a2122==3] <- 2
ability_7_13_a2122[ability_7_13_a2122==4] <- 3
ability_7_13_a2122[ability_7_13_a2122==5] <- 4
ability_7_13_a2122[ability_7_13_a2122==6] <- 5

ability_1_6_a2122 <- bind_rows(baseline_a2122[,1:6],endline_a2122[,1:6])
ability_base_end_a2122 <-bind_cols(ability_1_6_a2122,ability_7_13_a2122)

rm(baseline_cefr_mooc_A.21.22,#endline_cefr_mooc_A.21.22,
   baseline_a2122,endline_a2122,ability_7_13_a2122,ability_1_6_a2122)

# Cronbach's alpha ----

library(ltm) 

data <- ability_base_end_a2122 %>% filter(database=="Baseline") %>% dplyr::select(7:12) %>% na.omit()
data <- ability_base_end_a2122 %>% dplyr::select(7:12) %>% na.omit()

cronbach.alpha(data)
cronbach.alpha(data, CI=TRUE)

# ability_avsd ----

ability_av <- 
  ability_base_end_a2122 %>% group_by(database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  gather( "Ability", "Mean",-database)

ability_sd <-
  ability_base_end_a2122 %>% group_by(database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), sd, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2))%>% 
  gather( "Ability", "SD",-database)

# t.test ability 

attach(ability_base_end_a2122)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
#t4 <- t.test(qa4_thinking[database=="Baseline"],qa4_thinking[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])
#t6 <- t.test(qa6_initiative[database=="Baseline"],qa6_initiative[database=="Endline"])

ttest01 <- map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools"))

ability_avsd <-
  inner_join(ability_av,ability_sd) %>% select(2,everything()) %>% 
  inner_join(ttest01)

rm(ability_av,ability_sd,t1, t2, t3, t5, ttest01)

kbl(ability_avsd) %>%kable_classic() %>%
  column_spec(c(3,6), bold = T) %>%
  row_spec(c(3:4,7:8), background = "aliceblue")


###   ability Mean sector ----
ability_base_end_a2122 %>%filter(sector %in% c( "jr","am","js")) %>%
  group_by(sector,database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), mean, na.rm = TRUE) %>% 
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  row_spec(c(3:4), background = "aliceblue")

###   ability_jr     ----
ability_jr <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(sector == "jr")

attach(ability_jr)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Religious jewish students")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

###   ability_am     ----


ability_am <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(sector == "am") # %>% group_by(sector,database) %>% summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE)

attach(ability_am)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Muslim Arab students")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

###   ability_js     ----
ability_js <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(sector == "js") # %>% group_by(sector,database) %>% summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE)

attach(ability_js)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Secular Jewish students")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

# ------
ability_js %>% count(database) %>% filter(database =="Baseline")
ability_am %>% count(database)%>% filter(database =="Baseline")
ability_jr %>% count(database)%>% filter(database =="Baseline")

###   ability Mean course_typ ----
ability_base_end_a2122 %>%filter(course_typ != "course") %>% filter(!is.na(course_typ)) %>% 
  group_by(course_typ,database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), mean, na.rm = TRUE) %>% 
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  row_spec(c(3:4), background = "aliceblue")





###   ability_pre_basic     ----
ability_pre_basic <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(course_typ == "pre_basic") # %>% group_by(sector,database) %>% summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE)

attach(ability_pre_basic)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Pre-asic course ")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

###   ability_basic    ----
ability_basic <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(course_typ == "basic") # %>% group_by(sector,database) %>% summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE)

attach(ability_basic)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Basic course")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

###   ability_advanced_01     ----
ability_advanced_01 <- 
  ability_base_end_a2122 %>%
  select(-c("qa6_initiative","qa4_thinking")) %>% 
  filter(course_typ == "advanced_01") # %>% group_by(sector,database) %>% summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE)

attach(ability_advanced_01)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Advanced 1 course")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

# ------
ability_pre_basic%>% count(database)
ability_basic%>% count(database)
ability_advanced_01%>% count(database)





###   ability Mean gender ----
ability_base_end_a2122 %>% filter(!is.na(gender)) %>% 
  mutate(gender=ifelse(gender== 1,"Men","Women")) %>% 
  group_by(gender,database) %>%
  summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  kable_paper( full_width = F) %>% 
  row_spec(c(3:4), background = "aliceblue")


###   ability_Female     ----
ability_Female <-
  ability_base_end_a2122 %>% filter(!is.na(gender)) %>% 
  mutate(gender=ifelse(gender== 1,"Men","Women")) %>% 
  filter(gender== "Women")

attach(ability_Female)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Women students")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 

###   ability_Men     ----
ability_Men <-
  ability_base_end_a2122 %>% filter(!is.na(gender)) %>% 
  mutate(gender=ifelse(gender== 1,"Men","Women")) %>% 
  filter(gender== "Men")

attach(ability_Men)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Men students")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 




# ------




###   ability Mean age ----z <- 
ability_base_end_a2122 %>%  filter(!is.na(age)) %>% 
  mutate(ud25=ifelse(age %in% c("18_20","20_25"),"Generation_Z","over_25")) %>% 
  group_by(ud25,database) %>%
  summarise_at(vars(c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")), sd, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  kable_paper( full_width = F) %>% 
  row_spec(c(1:2), background = "aliceblue")

###   ability_Generation_Z     ----
ability_Generation_Z <-
  ability_base_end_a2122 %>%  filter(!is.na(age)) %>% 
  mutate(ud25=ifelse(age %in% c("18_20","20_25"),"Generation_Z","over_25")) %>%
  filter(ud25 == "Generation_Z")

attach(ability_Generation_Z)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Generation Z")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 



###   ability_over_25     ----
ability_over_25 <-
  ability_base_end_a2122 %>%  filter(!is.na(age)) %>% 
  mutate(ud25=ifelse(age %in% c("18_20","20_25"),"Generation_Z","over_25")) %>%
  filter(ud25 == "over_25")

attach(ability_Generation_Z)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students over 25")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 




###   ability_18_20     ----
ability_18_20 <-
  ability_base_end_a2122 %>% filter(!is.na(age)) %>% 
  filter(age == "18_20")
filter(age == "20_25")
filter(age == "25_30")
filter(age == "30_35")
filter(age == "35_99")

attach(ability_18_20)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students ages 18-20")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 


###   ability_20_25     ----
ability_20_25 <-
  ability_base_end_a2122 %>% filter(!is.na(age)) %>% 
  filter(age == "20_25")

attach(ability_20_25)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students ages 20-25")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 


###   ability_25_30     ----
ability_25_30 <-
  ability_base_end_a2122 %>% filter(!is.na(age)) %>% 
  filter(age == "25_30")

attach(ability_25_30)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students ages 25-30")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 


###   ability_30_35     ----
ability_30_35 <-
  ability_base_end_a2122 %>% filter(!is.na(age)) %>% 
  filter(age == "30_35")

attach(ability_30_35)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students ages 30-35")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 


###   ability_35_99     ----
ability_35_99 <-
  ability_base_end_a2122 %>% filter(!is.na(age)) %>% 
  filter(age == "35_99")

attach(ability_35_99)
t1 <- t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t2 <- t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t3 <- t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t5 <- t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])

map_df(list(t1, t2, t3,t5), tidy) %>% 
  select(statistic, p.value, conf.low, conf.high) %>% 
  round(3) %>% rename(t=statistic,Sig.= p.value) %>% 
  mutate(Ability=c("qa1_interaction","qa2_tech","qa3_knowledge","qa5_tools")) %>% 
  select(5,everything()) %>% 
  kbl(caption = "Self-efficacy - Students ages 35+")%>%kable_classic() %>%
  kable_paper( full_width = F) %>% column_spec(3, bold = T) 


# ---------





#`satisfaction with the course` n=144 ----
endline_cefr_mooc_A.21.22 %>%
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 3,1,`satisfaction with the course`)) %>% 
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 1,"yes","no")) %>% 
  count(`satisfaction with the course`) %>% na.omit() %>% 
  mutate(prt=n/sum(n)) %>% mutate_at(3, round, 2) %>% 
  kbl(align =c("r","c","c")) %>% kable_paper( full_width = F)

#Sector
sts_sector <- 
endline_cefr_mooc_A.21.22 %>%
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 3,1,`satisfaction with the course`)) %>% 
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 1,"yes","no")) %>% 
  group_by(sector) %>% 
  count(`satisfaction with the course`) %>% na.omit() %>%   
  mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/103,var="Sector") %>% mutate_at(4:5, round, 2) %>%
  filter(`satisfaction with the course`=="yes",n>4) %>% select(var,everything()) %>% rename("sub."=sector)

#course_typ
sts_course_typ <- 
endline_cefr_mooc_A.21.22 %>%
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 3,1,`satisfaction with the course`)) %>% 
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 1,"yes","no")) %>% 
  group_by(course_typ) %>% 
  count(`satisfaction with the course`) %>% na.omit() %>%   
  mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/103,var="course_typ") %>% mutate_at(4:5, round, 2) %>%
  filter(`satisfaction with the course`=="yes",n>4) %>% select(var,everything()) %>% rename("sub."=course_typ)

#gender
sts_gender <- 
endline_cefr_mooc_A.21.22 %>%
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 3,1,`satisfaction with the course`)) %>% 
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 1,"yes","no")) %>% 
  group_by(gender) %>% 
  count(`satisfaction with the course`) %>% na.omit() %>%   
  mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/103,var="gender") %>% mutate_at(4:5, round, 2) %>%
  filter(`satisfaction with the course`=="yes",n>4) %>% select(var,everything()) %>%  
  mutate(gender=ifelse(gender == 1,"Men","Women")) %>% rename("sub."=gender)

#age
sts_age <- 
endline_cefr_mooc_A.21.22 %>%
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 3,1,`satisfaction with the course`)) %>% 
  mutate(`satisfaction with the course`=ifelse(`satisfaction with the course` == 1,"yes","no")) %>% 
  group_by(age) %>% 
  count(`satisfaction with the course`) %>% na.omit() %>%   
  mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/103,var="age") %>% mutate_at(4:5, round, 2) %>%
  filter(`satisfaction with the course`=="yes",n>4) %>% select(var,everything()) %>% rename("sub."=age)

sts <- rbind(sts_course_typ,sts_sector,sts_age,sts_gender) 


  kbl(sts) %>% kable_paper( full_width = F) %>% 
#  column_spec(c(3,6), bold = T) %>%
  row_spec(c(4:6,12:13), background = "aliceblue")

  
###   time_flexibility ----
endline_cefr_mooc_A.21.22 %>%
  group_by(time_flexibility) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents)) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
  mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c")) %>% kable_paper( full_width = F)

###  `frontal lesson`  ----
endline_cefr_mooc_A.21.22 %>%
  group_by(`frontal lesson`) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=43` = Respondents / 43) %>% 
  mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents)) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
  mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
  mutate(`n=43` = paste0(round(100 * `n=43`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c","c")) %>% kable_paper( full_width = F)

###   zoom             ----
endline_cefr_mooc_A.21.22 %>%
  group_by(zoom) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=25` = Respondents / 25) %>% 
  mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents))%>% 
  mutate(across(is.numeric, round, 2))  %>% 
  mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
  mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
  mutate(`n=25` = paste0(round(100 * `n=25`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c","c")) %>% kable_paper( full_width = F)

# meaningful_experiences n=95 ----
endline_cefr_mooc_A.21.22 %>%
  group_by(meaningful_experiences) %>%
  summarise(n = n()) %>% na.omit() %>% 
    mutate(meaningful_experiences=ifelse(meaningful_experiences == 1,"yes","no")) %>% 
  mutate(`prt` = n / 95) %>% 
  mutate(across(is.numeric, round, 2))   %>% 
  mutate(`prt` = paste0(round(100 * `prt`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c","c")) %>% kable_paper( full_width = F)

#course_typ
  mng_course_typ <- 
    endline_cefr_mooc_A.21.22 %>%
    mutate(meaningful_experiences=ifelse(meaningful_experiences == 1,"yes","no")) %>% 
    group_by(course_typ) %>% 
    count(meaningful_experiences) %>% na.omit() %>%   
    mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/67,var="course_typ") %>% mutate_at(4:5, round, 2) %>%
    filter(meaningful_experiences=="yes") %>% select(var,everything()) %>% rename("sub."=course_typ)

#Sector
  mng_sector <- 
    endline_cefr_mooc_A.21.22 %>%
    mutate(meaningful_experiences=ifelse(meaningful_experiences == 1,"yes","no")) %>% 
    group_by(sector) %>% 
    count(meaningful_experiences) %>% na.omit() %>%   
    mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/67,var="Sector") %>% mutate_at(4:5, round, 2) %>%
    filter(meaningful_experiences=="yes",n>4) %>% select(var,everything()) %>% rename("sub."=sector)

  #gender
  mng_gender <- 
    endline_cefr_mooc_A.21.22 %>%
    mutate(meaningful_experiences=ifelse(meaningful_experiences == 1,"yes","no")) %>% 
    group_by(gender) %>% 
    count(meaningful_experiences) %>% na.omit() %>%   
    mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/67,var="gender") %>% mutate_at(4:5, round, 2) %>%
    filter( meaningful_experiences =="yes") %>% select(var,everything()) %>%  
    mutate(gender=ifelse(gender == 1,"Men","Women")) %>% rename("sub."=gender)
# age
  mng_age <- 
    endline_cefr_mooc_A.21.22 %>%
    mutate(meaningful_experiences=ifelse(meaningful_experiences == 1,"yes","no")) %>% 
    group_by(age) %>% 
    count(meaningful_experiences) %>% na.omit() %>%   
    mutate(prt_of_ver=n/sum(n),prt_of_YESs=n/67,var="age") %>% mutate_at(4:5, round, 2) %>%
    filter( meaningful_experiences =="yes") %>% select(var,everything()) %>% rename("sub."=age)
  
  mng <- rbind(mng_course_typ,mng_sector,mng_age,mng_gender) 
  
  
  kbl(mng) %>% kable_paper( full_width = F) %>% 
    #  column_spec(c(3,6), bold = T) %>%
    row_spec(c(4:6,12:13), background = "aliceblue")
  
  
##   speaks_english_better_is_meaningfu  ----
 endline_cefr_mooc_A.21.22 %>%
  group_by(speaks_english_better_is_meaningfu) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=95` = Respondents / 95) %>% 
   mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents))%>% 
  mutate(across(is.numeric, round, 2))   %>% 
   mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
   mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
   mutate(`n=95` = paste0(round(100 * `n=95`, 0), "%")) %>% 
   kbl(align =c("r","c","c","c","c")) %>% kable_paper( full_width = F)
 
# Correlations ability ----
 
base_6 <-  ability_baes_end %>% filter(database=="Endline") %>% drop_na() %>% select(3:8)

 res <- cor(base_6)
 round(res, 2)
 
 library(corrplot)
 corrplot(res, type = "upper", order = "hclust", 
          tl.col = "black", tl.srt = 45)
 
 library(PerformanceAnalytics)
 chart.Correlation(res)
 
 
# TOOLS ----
 c(learning_tools,podcast_n_practice+video_n_practice_speaking+vocabulary+articles_reading_n_practicing+
     interactive_activity+skills_videos, learning_tools_usage,learning_tools_sum)
 
endline_cefr_mooc_A.21.22 <- read_excel("~/master_research/DATAs/k2p_2022/CEFR_course/endline_CEFR_course_21.22_semesterA.xlsx",sheet = "endline_clean_data")
names(endline_cefr_mooc_A.21.22)

tools_endline <- 
  endline_cefr_mooc_A.21.22 %>%
  select(date,gender,age,sector,course_typ,learning_tools:learning_tools_sum) %>% 
  filter(!is.na(learning_tools)) %>% mutate(count = n()) %>% 
  mutate(age25=ifelse(age %in% c("18_20","20_25"),"Generation_Z","over_25")) 

sum_tools <-
  tools_endline %>% summarise_at(vars (podcast_n_practice:learning_tools_usage),sum)%>% 
  gather("key","n",1:7 ) %>% mutate_at(2, round, 2) 

prc_tools <-
  tools_endline %>% summarise_at(vars (podcast_n_practice:learning_tools_usage),mean,rm.na=T) %>% 
  gather("key","prc",1:7 ) %>% mutate_at(2, round, 2)


inner_join(sum_tools,prc_tools) %>% kable() %>% kable_minimal()

prop_tools <- 
  tools_endline %>% 
  mutate(total_learning_tools=ifelse(learning_tools_sum %in% c(4,5,6),"4-6",learning_tools_sum )) %>% 
  count(total_learning_tools) %>%
  mutate(prop = prop.table(n))%>%
  mutate_at(3, round, 2)%>%
  kable() %>% kable_classic()

tools_endline %>% 
  group_by(sector) %>% filter(sector %in% c( "jr","am","js")) %>% 
  summarise_at(vars (podcast_n_practice:learning_tools_usage),mean) %>% 
  gather("key","value",2:8 ) %>% mutate_at(3, round, 2) %>% arrange(sector)%>%
  kable() %>% kable_classic()

tools_endline %>% 
  group_by(age25) %>% 
  summarise_at(vars (podcast_n_practice:learning_tools_usage),mean) %>% 
  gather("key","value",2:8 ) %>% mutate_at(3, round, 2) %>% arrange(age25)%>%
  kable() %>% kable_classic()


tools_endline %>% 
  group_by(course_typ) %>% 
  summarise_at(vars (podcast_n_practice:learning_tools_usage),mean) %>% 
  gather("key","value",2:8 ) %>% mutate_at(3, round, 2) %>% arrange(desc(course_typ))%>%
  kable() %>% kable_classic()





