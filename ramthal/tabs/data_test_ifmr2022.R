library(tidyverse)
library(readxl)
library(haven)
library(kableExtra)
# library(epiDisplay)
cul_tab_2018 <- read_dta("~/master_research/DATAs/ramthal_data/Ramthal Midline/Irrigation_Cultivation Section_Long Form_20180622.dta")

# crops plot 1 ----
l40 <- 
  Anony_19_11_ %>% select (starts_with("l4") )

# Checking distributions for the **ramthal_module** ----
ramthal_module <- Anony_19_11_%>%
  select(id,starts_with("m")) %>% 
  select(-max_plot)

ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  count(group,mm1) %>%
  group_by(group) %>%         
  mutate(prop = prop.table(n)) %>% 
  mutate(prop = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(group)) %>% 
  kable() %>% kable_classic()

# M1 households to rely on ----
M1 <- ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  count(group,m1) %>%
  group_by(group) %>%         
  mutate(prop = prop.table(n)) %>% 
  filter(m1>0) %>% 
  arrange(desc(group)) 

#ggplot his ..density.. ----
ggplot(M1, aes(x = m1, y = ..density..)) + 
  geom_histogram(bins = 20, colour = "#80593D", fill = "#9FC29F", boundary = 0) +
  geom_density(color = "#3D6480") + 
  facet_wrap(~group)

ggplot(M1, aes(m1, fill = group)) +
  geom_histogram(aes(y = after_stat(density * width)),
                 position = "identity", alpha = 0.5)

#ggplot his count  ----

750/400
ggplot(M1, aes(m2)) +
  geom_histogram(binwidth = 10,fill = "lightBlue")+ 
  facet_wrap(~group)+
  ggtitle("m1:	How many households in the village would you consider to be \nhouseholds that you can rely on in time of need?") +
  labs(x = "No. of households to rely on", y = "No. of respondents") +
  theme_bw()+
  theme(text=element_text(size=16,  family="serif"))


# M3
M3 <- ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  select(group,m3_1:m3__999) %>% 
  rename(Increased_yields=m3_1,
         Water_saving=m3_2,
         Fertilizer_saving=m3_3,
         Less_weeds=m3_4,
         Less_labor_requirements=m3_5,
         Dont_know=m3__999,
         Other=m3__888) %>% 
  gather(key=answer,value = value,2:8) %>% 
  filter(value>0) %>% 
  group_by(group,answer) %>% 
  summarise(n=sum(value)) %>% 
  mutate(prop = prop.table(n)) %>% 
  mutate(perc = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  filter(perc != "0%")



ggplot(M3, aes(x = "", y = prop, fill = answer)) +
  geom_col() +
  geom_text(aes(label = perc),position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer()+
  facet_grid(~group)+
  theme_void()+
  theme(legend.position="bottom")+
  ggtitle("M3:	Are you aware of Are you aware of any advantages of drip irrigation over other irrigation methods?any advantages of drip irrigation over other irrigation methods?") +
  theme(text=element_text(size=16,  family="serif"))

#M4
M4 <- ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  select(group,m4_1:m4__999) %>% 
  rename(Government=m4_1,
         The_Company=m4_2,
         Dont_know=m4__999,
         Other=m4__888) %>% 
  gather(key=answer,value = value,2:5) %>% 
  filter(value>0) %>% 
  group_by(group,answer) %>% 
  summarise(n=sum(value)) %>% 
  mutate(prop = prop.table(n)) %>% 
  mutate(perc = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  filter(perc != "0%")

ggplot(M4, aes(x = "", y = prop, fill = answer)) +
  geom_col() +
  geom_text(aes(label = perc),position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer()+
  facet_grid(~group)+
  theme_void()+
  theme(legend.position="bottom")+
  ggtitle("M4: Who is deciding which land and farmers are \npart of this government drip irrigation project?") +
  theme(text=element_text(size=16,  family="serif"))

# M5
M5 <- ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  select(group,m5_1:m5__999) %>% 
  rename(Elevation=m5_1,
         Political=m5_2,
         Wealth=m5_3,
         Farming_skill=m5_4,
         Location=m5_5,
         Dont_know=m5__999,
         Other=m5__888) %>% 
  gather(key=answer,value = value,2:5) %>% 
  filter(value>0) %>% 
  group_by(group,answer) %>% 
  summarise(n=sum(value)) %>% 
  mutate(prop = prop.table(n)) %>% 
  mutate(perc = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  filter(perc != "0%")

ggplot(M5, aes(x = "", y = prop, fill = answer)) +
  geom_col() +
  geom_text(aes(label = perc),position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer()+
  facet_grid(~group)+
  theme_void()+
  theme(legend.position="bottom")+
  ggtitle("M5: On what basis are they deciding what/who is \nincluded or not in the project?") +
  theme(text=element_text(size=16,  family="serif"))

# estimate_land_value----
### non-ramthal farmer
# m12	If you were to sell any of your plot, how much would it be worth? (Rs. per Acre)
# m13	Can you estimate land  inside 
# 
### ramthal farmer
# m17	If you were to sell any of your plot, how much would it be worth? (Rs. per Acre)	
# m18	Can you estimate land  outside 

estimate_land_value <- 
  ramthal_module %>% 
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>% rename(group=mm2_logic) %>% 
  select(id,group,m12,m13,m17,m18) 

estimate_land_value[estimate_land_value==-999] <- NA
estimate_land_value[estimate_land_value==0] <- NA

est_land_value <- 
  estimate_land_value %>%
  mutate(land_value = case_when(
    m17>m18~'higher_inside',
    m17<m18~'higher_outside',
    m17==m18~'same',
    m12>m13~'higher_outside',
    m12<m13~'higher_inside',
    TRUE ~ 'same')) %>% 
  count(group,land_value) %>%
  group_by(group) %>%         
  mutate(prop = prop.table(n)) %>% 
  arrange(desc(group)) %>% 
  mutate(prop = paste0(round(100 * n/sum(n), 0), "%")) 

# R28+R29 HH member send/bring money ----

# C25	R28		Does the [member's name] send money back to the household?
# C26	R29		Does the [member's name] bring money with him/her when visiting the household?
  
  
A <- Anony_19_11_ %>%
  mutate(mm2_logic=ifelse(mm2_logic==1,"ramthal farmer","non-ramthal farmer")) %>%
  rename(group=mm2_logic) %>% 
  select (id ,cal_village_updated,starts_with("r28"),starts_with("r29")  )  


