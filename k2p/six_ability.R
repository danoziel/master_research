library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(kableExtra)

baseline_cefr_mooc_A.21.22 <- read_excel("~/master_research/DATAs/CEFR_course/baseline_CEFR_course_21.22_semesterA.xlsx",sheet = "clean_data")
endline_cefr_mooc_A.21.22 <- read_excel("~/master_research/DATAs/CEFR_course/endline_CEFR_course_21.22_semesterA.xlsx",sheet = "endline_clean_data")

#ability_base_end_a2122 ----

baseline_a2122 <- baseline_cefr_mooc_A.21.22 %>% 
  select(sector,course_typ,qa1_interaction,qa2_tech,qa3_knowledge,qa4_thinking,qa5_tools,qa6_initiative) %>% 
    add_column(database="Baseline")

endline_a2122<- endline_cefr_mooc_A.21.22 %>%
  select(sector,course_typ,qa1_interaction,qa2_tech,qa3_knowledge,qa4_thinking,qa5_tools,qa6_initiative) %>%
  add_column(database="Endline")

ability_base_end_a2122 <- bind_rows(baseline_a2122,endline_a2122)
rm(baseline_a2122,endline_a2122)

ability_base_end_a2122[ability_base_end_a2122==1] <- NA
ability_base_end_a2122[ability_base_end_a2122==2] <- 1
ability_base_end_a2122[ability_base_end_a2122==3] <- 2
ability_base_end_a2122[ability_base_end_a2122==4] <- 3
ability_base_end_a2122[ability_base_end_a2122==5] <- 4
ability_base_end_a2122[ability_base_end_a2122==6] <- 5

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

ability_avsd <-
  inner_join(ability_av,ability_sd) %>% 
  mutate(t=c(" ","4.1942"," ","2.7688"," ", "7.8629", " ","6.9733", " ","6.122", " ","5.0964")) %>% 
  mutate(df=c(" ","385.14"," ", "497.08"," ", "416.82"," ","410.01"," ","404.36"," ","446.88")) %>% 
  mutate(`p-value`=c(" ","3.402e-05", " ","0.005", " ", "3.245e-14"," ","1.246e-11"," ","2.188e-09", " ","5.119e-07"))
rm(ability_av,ability_sd)

kbl(ability_avsd) %>%kable_classic() %>%
  column_spec(c(3,7), bold = T) %>%
  row_spec(c(3:4,7:8,11:12), background = "aliceblue")


###   ability sector     ----
ability_base_end_a2122 %>% filter(sector %in% c("jr","js","am")) %>% group_by(sector,database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  #  kable_paper( full_width = F) %>% 
  #column_spec(c(3,7), bold = T) %>%
  row_spec(c(3:4), background = "aliceblue")

###   ability course_typ ----
ability_base_end_a2122 %>%filter(course_typ != "course") %>% filter(!is.na(course_typ)) %>% 
  group_by(course_typ,database) %>%
  summarise_at(vars(qa1_interaction:qa6_initiative), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  kbl() %>%kable_classic() %>%
  #  kable_paper( full_width = F) %>% 
  #column_spec(c(3,7), bold = T) %>%
  row_spec(c(3:4), background = "aliceblue")

# t.test ability ----

attach(ability_base_end_a2122)
t.test(qa1_interaction[database=="Baseline"],qa1_interaction[database=="Endline"])
t.test(qa2_tech[database=="Baseline"],qa2_tech[database=="Endline"])
t.test(qa3_knowledge[database=="Baseline"],qa3_knowledge[database=="Endline"])
t.test(qa4_thinking[database=="Baseline"],qa4_thinking[database=="Endline"])
t.test(qa5_tools[database=="Baseline"],qa5_tools[database=="Endline"])
t.test(qa6_initiative[database=="Baseline"],qa6_initiative[database=="Endline"])

#`satisfaction with the course` n=144 ----

endline_cefr_mooc_A.21.22 %>%
  group_by(`satisfaction with the course`) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents)) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
  mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c")) %>% kable_paper( full_width = F)
  
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

# meaningful_experiences ----
endline_cefr_mooc_A.21.22 %>%
  group_by(meaningful_experiences) %>%
  summarise(`Respondents` = n()) %>%
  mutate(`n=95` = Respondents / 95) %>% 
  mutate(`n=144` = Respondents / 144) %>% 
  mutate(`Total` = Respondents / sum(Respondents))%>% 
  mutate(across(is.numeric, round, 2))   %>% 
  mutate(Total = paste0(round(100 * Total, 0), "%")) %>% 
  mutate(`n=144` = paste0(round(100 * `n=144`, 0), "%")) %>% 
  mutate(`n=95` = paste0(round(100 * `n=95`, 0), "%")) %>% 
  kbl(align =c("r","c","c","c","c")) %>% kable_paper( full_width = F)

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
 
 
 

 