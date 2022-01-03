# Q11 12 13 sip_use months/days/hours----
# On an average, out of 12 months in a year, for how many months do you use your SIP? 
# SURVEY 2017-2019 data

df36_usage <- 
  water01_SEASONs %>% 
  rename(household_questionnaire_id=HH) %>% 
  mutate(household_questionnaire_id=ifelse(household_questionnaire_id=="A0110402001","A110402001",household_questionnaire_id)) %>% 
  inner_join(list_36_farmers) %>% rename(HH=household_questionnaire_id) %>% 
  filter(Hours>0) %>% 
  group_by(HH,date) %>% summarise(hrs=sum(Hours)) %>% 
  mutate(
    year = as.numeric(format(date, format = "%Y")),
    month = as.numeric(format(date, format = "%m"))) %>% 
  as.data.frame(df36_usage) 

df36_hours_a_day <- 
  df36_usage %>% summarise(Mean=mean(hrs)) %>%
  mutate(usaeg="Hours a Day")%>% mutate_at(1,round,2)


df36_days_in_month <- 
  df36_usage%>%
  group_by(HH,year,month) %>% count() %>% 
  as.data.frame(df36_usage) %>% 
  filter(n>1) %>%
  rename(days_in_month=n) %>% summarise(Mean=mean(days_in_month)) %>% 
  mutate(usaeg="Days a Month") %>% mutate_at(1,round,2)

# df36_days_in_each_month ----

df36_days_month_season <- 
  df36_usage%>%
  mutate(seasons=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon\n2017",date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter\n2017",date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer\n2017",
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon\n2018",date >= "2018-10-01" & date <= "2019-01-31" ~ "Winter\n2018",date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer\n2018",
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon\n2019",date >= "2019-10-01" & date <= "2019-12-16" ~ "Winter\n2019")) %>% 
  mutate(season=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon",date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter",date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer",
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon",date >= "2018-10-01" & date <= "2019-01-31" ~ "Winter",date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer",
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon",date >= "2019-10-01" & date <= "2019-12-16" ~ "Winter"))

df36_days_season08 <- 
  df36_days_month_season %>% group_by(HH,seasons,month) %>% count() %>% 
  filter(n>1) %>%
  group_by(HH,seasons) %>%  summarise(Mean=mean(n)) %>% group_by(seasons) %>%  summarise(days_in_month=mean(Mean)) %>% mutate_at(2,round) 

df36_days_season03 <- 
  df36_days_month_season %>% group_by(HH,year,season,month) %>% count() %>% 
  filter(n>1) %>%
  group_by(HH,season) %>% summarise(Mean=mean(n)) %>% group_by(season) %>%  summarise(days_in_month=mean(Mean))%>% mutate_at(2,round) 

700/250
df36_days_season08 %>%  add_row(seasons = "Month\n2017-19", days_in_month = 14) %>% 
  add_row(seasons = "Month\n2021", days_in_month = 23) %>%
  add_row(seasons = "|", days_in_month = NA) %>%
  
  ggplot(aes(seasons,days_in_month, fill=seasons))+
  geom_col(position="dodge",width= 0.75) +
  geom_text(aes(label = days_in_month), position = position_dodge(0.70), vjust= 1.25, color= "white", size = 4)+
  theme_minimal() +
  labs(x = " ",y="Days")+
  theme(text = element_text(size = 25),axis.title = element_text(size = 17.5),legend.position = "none",axis.text = element_text(size = 10)) +
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
  
  scale_x_discrete(limits = c("Month\n2021","Month\n2017-19","|",
                              "Monsoon\n2017","Monsoon\n2018","Monsoon\n2019", "|" ,
                              "Winter\n2017","Winter\n2018","Winter\n2019","|",
                              "Summer\n2017","Summer\n2018"))+
  scale_fill_manual(values=c("black","dimgrey","dimgrey", "dimgrey","sienna2","#339900",
                             "darkolivegreen4", "darkolivegreen4",
                             "dodgerblue4", "dodgerblue4","dodgerblue4"))


# add rain days variable

  
diary_44 <- diary_4 %>% filter(district=="Saptari") %>% mutate(year = as.numeric(format(date, format = "%Y")),month = as.numeric(format(date, format = "%m"))) %>% select(-c(district,ALLSKY_SFC_SW_DWN))%>%
  mutate(seasons=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon\n2017",date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter\n2017",date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer\n2017",
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon\n2018",date >= "2018-10-01" & date <= "2019-01-31" ~ "Winter\n2018",date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer\n2018",
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon\n2019",date >= "2019-10-01" & date <= "2019-12-16" ~ "Winter\n2019")) %>% 
  mutate(season=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon",date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter",date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer",
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon",date >= "2018-10-01" & date <= "2019-01-31" ~ "Winter",date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer",
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon",date >= "2019-10-01" & date <= "2019-12-16" ~ "Winter"))

diary_rain_5 <-   diary_44 %>% filter(PRECTOT<5) %>% group_by(year,seasons,season,month) %>% count()
x2 <- diary_rain_5 %>%as.data.frame() %>%  summarise(mean(n))
x1 <- diary_rain_5 %>% group_by(season) %>% summarise(mean(n))
x <- diary_rain_5 %>% group_by(seasons) %>% summarise(m=mean(n))

  
  

df36_days_month_season %>% group_by(year,season,month) %>% count() %>% 
  filter(n>5) %>%
  group_by(HH,season) %>% summarise(Mean=mean(n)) %>% group_by(season) %>%  summarise(days_in_month=mean(Mean))%>% mutate_at(2,round) 

View(diary_44)
View(diary_rain_2)







# ----
df_cultivation_expansion <- read.csv("~/master_research/DATAs/cultivation_expansion.csv")

# remove from "cultivation_expansion" in thesis----

#  filter(!HH %in% c("A104507035","E0104705010", "E104705010","A110402001","A0110402001")) %>%  # HH pilot
#  filter(!HH %in% c("T303007001", "T300901091","T210701004","T300901113")) %>%    ## missing a year
#  filter(!HH %in% c("T300608033","T308707002"))#Removed due to conflicting pond size data -lands_I_18_19/aquaculture




df36_cultivation_expansion <- 
  df_cultivation_expansion %>% 
  mutate(HH=ifelse(HH == "A0110402001","A110402001",HH)) %>% 
  inner_join(list_36_farmers)



# df36_cultivation_expansion ----
df36_cultivation_expansion <- 
  df36_cultivation_expansion %>% 
  mutate(district=ifelse(district == "Rautahat_Bara_Sarlahi", "Rautahat, Bara & Sarlahi",district)) %>% 
  filter(Source !="Irrigated land \nMonitoring") %>% 
  group_by(district,x,Source) %>% summarise(y=mean(y,na.rm = T)) %>% 
  mutate_at(4,round,2)

# PLOT 
df36_cultivation_expansion <- 
  df36_cultivation_expansion %>% 
  mutate(y=ifelse(district=="Rautahat, Bara & Sarlahi" & x=="monsoon_2018_2019" & Source=="Irrigated land \nSurvey",3.66,y)) 

# 

df36_cultivation_expansion %>%
  ggplot(aes(x,y, fill=Source))+
  geom_col(position="dodge",width= 0.75,alpha=.65) +
  geom_text(aes(label = y), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
  facet_grid(. ~ district) +
  
  theme_minimal() +
  labs(x = " ",y="Area size (in ha)")+
  theme(text = element_text(size = 25),
        axis.title = element_text(size = 17.5),
        axis.text = element_text(size = 10)) +
  theme(panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
  
  scale_x_discrete(limits = c("Total land holding", "monsoon_2017_2018", "winter_2017_2018","summer_2017_2018",
                              "monsoon_2018_2019", "winter_2018_2019","summer_2018_2019"))






# croping pattern YES/NO ----
list_36_farmers <- read.csv("~/master_research/DATAs/list_36_farmers.csv")

list_36_farmers <- list_36_farmers %>% rename(household_questionnaire_id=HH)  
  
# aqua
aqua_sap_2012 <- 
  lands_I_17_18_19 %>%
  filter(TC==1) %>% 
  select (household_questionnaire_id,year,land_for_aquaculture_ponds) %>% 
  mutate(name_of_crop = "Fish Farming") %>% 
  rename(cult_area_under_crop = land_for_aquaculture_ponds) %>% 
  mutate(cult_area_under_crop=cult_area_under_crop*0.0339) %>%
  inner_join(list_36_farmers,by="household_questionnaire_id") 
  
aqua_rbs_2021 <- 
  lands_I_18_19 %>%
  filter(TC==1) %>% 
#  filter(!household_questionnaire_id %in% c("T300608033","T309708020")) %>% 
  select (household_questionnaire_id,year,land_for_aquaculture_ponds) %>% 
  mutate(name_of_crop = "Fish Farming") %>% 
  rename(cult_area_under_crop = land_for_aquaculture_ponds) %>% 
  mutate(cult_area_under_crop=cult_area_under_crop*0.0339) %>% 
  inner_join(list_36_farmers,by="household_questionnaire_id") 

aq3_2021 <- aqua_rbs_2021 %>% filter(cult_area_under_crop != 0) %>% count(name_of_crop,year)

Ag3_2021 <- Agriculture_18_19 %>% filter(TC==1) %>% 
  select(household_questionnaire_id ,name_of_crop, year) %>% 
  inner_join(list_36_farmers,by="household_questionnaire_id") %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(name_of_crop) %>% bind_rows(aq3_2021) %>% 
  spread(year,n) %>% 
  rename("18" = `2018`,"19."=`2019`)

aq3_2021 <- aqua_sap_2012 %>% filter(cult_area_under_crop != 0) %>% count(name_of_crop,year)
Ag1_2021 <- Agriculture_17_18_19 %>% filter(TC==1) %>% 
  select(household_questionnaire_id ,name_of_crop, year) %>% 
  inner_join(list_36_farmers,by="household_questionnaire_id") %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(name_of_crop) %>% bind_rows(aq3_2021) %>% spread(year,n)

full_join(Ag1_2021,Ag3_2021) %>% 
  filter(!name_of_crop %in% c("Cereals","Others")) %>% 
  kbl(align = c("l","c","c", "c", "c", "c"),
      col.names=c("Crop","2017","2018", "2019", "2018", "2019")) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Saptari" = 3, "RBS" = 2))


