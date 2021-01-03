library(tidyverse)
library(haven)
Broockman_Butler_AJPS_2015_Study1_data <-
  read_dta("~/master_research/DATAs/data_yotam/m2/Broockman_Butler_AJPS_2015_Study1_data.dta")

approve_LegA_post_binary <-Broockman_Butler_AJPS_2015_Study1_data %>% 
  select(serial,q1_post,q2_post,q3_post,policy_letter_treat) %>% 
  mutate(LegA_post_binary = ifelse(q1_post %in% c("3","4"),"0",
                          ifelse(q2_post%in% c("1","2"),"1",
                                 ifelse(q3_post%in% c("1","2"),"0",
                                        NA))))

approve_LegA_post_binary <- approve_LegA_post_binary%>% drop_na()

approve_LegA_post_binary%>% 
  drop_na() %>% 
  count(policy_letter_treat,LegA_post_binary) %>% 
  mutate(prop = prop.table(n))

approve_LegA_post_binary <-approve_LegA_post_binary %>% 
  mutate(control_approve=case_when(
    LegA_post_binary == 1 & policy_letter_treat == 0~1,TRUE ~ as.numeric (0))) %>% 
  mutate(treatment_approve=case_when(
  LegA_post_binary == 1 & policy_letter_treat == 1~1,TRUE ~ as.numeric (0)))
  
attach(approve_LegA_post_binary)

t.test(treatment_approve,control_approve)
