# librarys ----
library(tidyverse)
library(tableone)

library(kableExtra)
library(formattable)
library(gridExtra)
library(scales)
library(extrafont)
library(sjPlot)

library(readxl)


# organize data ----

post_survey <- Phone_Survey_21_24_Jun_2021_SPIP_ICIMOD_

# Removal of those who did not respond to the survey
post_survey_01 <- 
  post_survey %>% 
  filter(!is.na(name_SIP_owner)) %>% 
  rename(District=district) %>% 
  mutate(district  = ifelse(District %in% c("Rautahat", "Bara","Sarlahi"),
                            "Rautahat_Bara_Sarlahi","Saptari"))
rm(post_survey)

post_survey_01 <- 
  post_survey_01 %>% 
  mutate(size_all_land_own_ha=as.numeric(size_all_land_own_ha),
         size_irrigated_land_ha=as.numeric(size_irrigated_land_ha),
         why_not_irrigated_entire_land=as.numeric(why_not_irrigated_entire_land))

list_36_farmers <- 
  post_survey_01 %>% select(HH,district)

post_survey_01 %>%count(district)
# rain data----
POWER_Point_Daily <- read.csv("~/master_research/DATAs/data_icimod_post_survey/POWER_Point_Daily_20200601_20210531_026d6065N_086d6353E_LST.csv")

POWER_Point_Daily %>%filter(PRECTOTCORR<=5.27) %>% 
  group_by(MO) %>% count() %>% as.data.frame() %>%  summarise(m=mean(n))

# nasa----
library(nasapower)

rain_saptari_21 <- get_power(
  community = "SSE",
  lonlat = c(86.63533, 26.60646),
  pars = c("PRECTOT"),
  dates =c( "2020-06-01","2021-05-31"),
  temporal_average = "DAILY") %>% 
  select(7,8)

rain_rbs_21 <- get_power(
  community = "SSE",
  lonlat = c(85.16984, 27.00147),
  pars = c("PRECTOT"),
  dates =c( "2020-06-01","2021-05-31"),
  temporal_average = "DAILY")%>% 
  select(7,8)

HH_rain_4 <-
  rain_rbs %>%
  right_join(rain_saptari,by="YYYYMMDD") %>% 
  rename(date=YYYYMMDD,rbs_mm=PRECTOT.x,saptari_mm=PRECTOT.y ) %>% 
  rename(Rautahat_Bara_Sarlahi=rbs_mm,Saptari=saptari_mm ) %>% 
  gather( "district", "rain_mm",-date) 
rm(rain_saptari,rain_rbs)





# color----

scale_fill_manual(values=c("sienna","sienna1","sienna2","sienna3", "sienna4"))+
  c("steelblue","steelblue")

mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))
mutate(freq =(round(n/sum(n), 2)))

# Q ----
#Q7 what_pumps_use_for_irri                        ----
#  Do you use any of the following pumps for crop irrigation? 

post_survey_01 %>%
  mutate(pump_usege= ifelse(what_pumps_use_for_irri %in% c("Both", "Diesel"),
                                               "Diesel","Electric")) %>% 
  group_by(district,pump_usege) %>% 
  summarise(n=n())%>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

post_survey_01 %>%
  mutate(pump_usege= ifelse(what_pumps_use_for_irri %in% c("Both", "Diesel"),
                            "Diesel","Electric")) %>% 
  group_by(pump_usege) %>% 
  summarise(n=n())%>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

#Q8 9 size_all_land_own_ha size_irrigated_land_ha  ----
 #Q8 What is the size of all the land you own? 
 #Q9 What is the size of the cultivated land that is irrigated? 

# BY DISTRICT
size_land <- 
  post_survey_01 %>%filter(!HH %in% c("A110402001","T210101002","T210101002")) %>% 
  group_by(district) %>% 
  summarise(size_all_land_own_ha=mean(size_all_land_own_ha,na.rm = TRUE),
            size_irrigated_land_ha=mean(size_irrigated_land_ha,na.rm = TRUE)) %>% 
  gather(x,y,-1) %>% 
  mutate_at(3,round,2) %>% 
  rename(Source=x) %>% 
  mutate(district=ifelse(district == "Rautahat_Bara_Sarlahi", "Rautahat, Bara & Sarlahi",district),
         x=c("Annual_2021","Annual_2021","annual_2021","annual_2021")) %>% 
  select(district,x,Source,y)
  
# combinthe surveys data

survey2017_2021 <- rbind(df36_cultivation_expansion,size_land) 

# 800/500 crop_df

survey2017_2021 %>%
  filter(Source !="size_all_land_own_ha") %>% 
  ggplot(aes(x,y, fill=x))+
  geom_col(position="dodge",width= 0.75) +
  geom_text(aes(label = y), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
  facet_grid(. ~ district) +
  
  theme_minimal() +
  labs(x = " ",y=" ")+
  theme(text = element_text(size = 25),
        axis.title = element_text(size = 17.5),
        axis.text = element_text(size = 10)) +
  theme(panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(angle = 90),legend.position = "none")+
  scale_x_discrete(limits = c("Total land holding", "monsoon_2017_2018", "winter_2017_2018","summer_2017_2018",
                              "monsoon_2018_2019", "winter_2018_2019","summer_2018_2019","annual_2021"))+
  scale_fill_manual(values=c("steelblue1","steelblue1","steelblue1","steelblue1",
                             "steelblue1","steelblue4","steelblue1","steelblue1"))


new


#Q10 why_not_irrigated_entire_land                ----
 #If you do not irrigated the entire  land that you own, what is the reason? 

pQ10 <- 
  post_survey_01 %>% count(why_not_irrigated_entire_land) %>% 
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  kbl() %>%
  kable_classic()

Multiple_choice_survey2021_sipICIMODE[c(5,6,9),2] %>% 
  rename(why_not_irrigated_entire_land=gender_who_applied_SIP) %>% 
  mutate(n=0,freq=0)


#PLOT
ggplot(pQ10, aes(x="", y=freq, fill=as.factor(why_not_irrigated_entire_land))) + 
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size=4)+
  labs(x = NULL, y = NULL, fill = NULL, title = "why_not_irrigated_entire_land")+
  theme_void()+
  scale_fill_manual(values=c("sienna","sienna1","sienna2","sienna3", "sienna4"))+
  theme(legend.position = "none")
  
# NO NA's
pQ10_2 <- 
  post_survey_01 %>% 
  filter(!is.na(why_not_irrigated_entire_land)) %>% 
  count(why_not_irrigated_entire_land) %>% 
  mutate(freq =(round(n/sum(n), 2)))
#  mutate(freq = round(n/sum(n),4)*100,lab.pos = cumsum(freq)-.5*freq)

  ggplot(pQ10_2, aes(x="", y=freq, fill=as.factor(why_not_irrigated_entire_land))) + 
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size=4)+
  labs(x = NULL, y = NULL, fill = NULL, title = "why_not_irrigated_entire_land")+
  theme_void()+
  scale_fill_manual(values=c("sienna1","sienna2","sienna3", "sienna"))+
  theme(legend.position = "none")






                                                                                                          




#Q11 12 13 sip_use_ ----
 #On an average, out of 12 months in a year, for how many months do you use your SIP? 
sip_use_ <- post_survey_01 %>% summarise(
    sip_use_months_in_year=mean(sip_use_months_in_year,na.rm = T),
    sip_use_days_in_month=mean(sip_use_days_in_month,na.rm = T),
    sip_use_hours_in_day=mean(sip_use_hours_in_day,na.rm = T))%>% gather(key,value) 

#Q17 18 19 diesel_use_ -----
  #On an average, out of 12 months in a year, for how many months do you use your diesel pump ? 
diesel_use_ <- post_survey_01 %>% summarise(
    diesel_use_months_in_year=mean(diesel_use_months_in_year,na.rm = T),
    diesel_use_days_in_month=mean(diesel_use_days_in_month,na.rm = T),
    diesel_use_hours_in_day=mean(diesel_use_hours_in_day,na.rm = T)) %>% gather(key,value)

sip_diesel_use <- 
  rbind(sip_use_,diesel_use_) %>% add_row(key="Monitoring\n2017-2019",value=5.88) %>% 
  add_row(key="Mean sunshine\nduration hours\nper day",value=6.61) %>% mutate_at(2,round,2) %>% 
  add_row(key=" | ",value=NA)

sip_diesel_use[1:6,] %>% ggplot(aes(key,value, fill=key))+
  geom_col(position="dodge",width= 0.75) +
  geom_text(aes(label = value), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
  theme_minimal() +labs(x = " ",y=" ")+
  theme(text = element_text(size = 25),axis.title = element_text(size = 17.5),axis.text = element_text(size = 10)) +
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"),axis.text.x = element_text(angle = 90),legend.position = "none")+
  scale_x_discrete(limits = c("sip_use_months_in_year","diesel_use_months_in_year"," | ","sip_use_days_in_month","diesel_use_days_in_month"," | ","sip_use_hours_in_day","diesel_use_hours_in_day"))+
  scale_fill_manual(values=c("#FF9900","#FF9900","#FF9900","#339900","#339900","#339900"))

sip_diesel_use[c(3,7:8),]%>% mutate(key=ifelse(key=="sip_use_hours_in_day","Survey\n2021",key)) %>% 
  ggplot(aes(key,value, fill=key))+geom_col(position="dodge",width= 0.75) +
  geom_text(aes(label = value), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
  theme_minimal() +labs(x = " ",y=" ")+
  theme(text = element_text(size = 25),axis.title = element_text(size = 17.5),axis.text = element_text(size = 10),legend.position = "none")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  scale_x_discrete(limits = c("Survey\n2021","Monitoring\n2017-2019"," | ","Mean sunshine\nduration hours\nper day"))+
  scale_fill_manual(values=c("#FFCC33","#99CC33","#339900"))


  
  
  
  
  
  
  
  
  
  
  


#Q15 pct_SIPirrigated_of_TOTALirrigated ----
#How much of your total irrigated  area is irrigated by the SIP (choose one option)? 
post_survey_01 %>%
  count(pct_SIPirrigated_of_TOTALirrigated_area) %>% 
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

#Q16 why_not_use_SIP_max_0 ----
 #If the farmer does not use the pump 7 days a week or 12 months a year- why does he/she not use SIP more than that ? (Choose Multiple Answers) 
     # first choise ----
post_survey_01 %>% 
  count(why_not_use_SIP_max_01) %>% 
  drop_na() %>% 
  mutate(freq =(round(n/sum(n), 2))) %>% 
  #PLOT
  ggplot(aes(x="", y=freq, fill=as.factor(why_not_use_SIP_max_01))) + 
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size=7)+
  labs(x = NULL, y = NULL, fill = NULL, title = "why_not_use_SIP_max_01")+
  theme_void()+
  scale_fill_manual(values=c("sienna2","sienna3","sienna1","sienna"))+
  theme(plot.title = element_text(size = 15,hjust = 0.5))


     # all choises -----

why_not_use_SIP <- 
  post_survey_01 %>% select(why_not_use_SIP_max_01:why_not_use_SIP_max_04) %>% 
  drop_na(why_not_use_SIP_max_01) %>% gather(why,reason) %>% count(reason) %>% drop_na() %>% 
  mutate(gray_bar=24-n, freq =n/24)

SurveyQA <- read_excel("~/master_research/DATAs/Phone_Survey_21.24_Jun_2021_SPIP_ICIMOD .xlsx",sheet = "QA")

600/250
SurveyQA[19:28,2:3] %>% 
  rename(reason=No.) %>% 
  inner_join(why_not_use_SIP) %>% select(A,n,gray_bar) %>% 
  gather(key,value,2:3) %>% 
  
  ggplot(aes(x=A, y=value, fill=key)) + 
  geom_bar(stat="identity",width=0.8)+coord_flip()+
  theme_blank()+labs(x=" ", y="")+
 # geom_text(aes(label=value), vjust=1.5, colour="white", size=3.5)+
  theme(panel.grid.major.x = element_blank(),text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"black"))+
  theme(axis.text.y = element_text(size=14))




#Q20 Croping Pattern crop_irrigate_total_01 ----
 #What are the main crops that you irrigate using electric, diesel or solar pumps? ? (list maximum10 major crops by name) 
post_survey_01%>% 
  select(24:33) %>% 
  gather(crop_irrigate,crop) %>% 
  drop_na() %>% 
  count(crop)

crop_data_Phone_Survey <- read_excel("~/master_research/DATAs/Phone_Survey_21.24_Jun_2021_SPIP_ICIMOD .xlsx",sheet = "crop_data")
crop_df <- 
  crop_data_Phone_Survey %>% 
  filter(!is.na(name_SIP_owner)) %>% 
  rename(District=district) %>% 
  mutate(district  = ifelse(District %in% c("Rautahat", "Bara","Sarlahi"),
                            "Rautahat_Bara_Sarlahi","Saptari")) %>% 
  select(1,24:33) %>% 
  gather(crop_irrigate,crop,-1) %>% 
  select(-2) %>% distinct() %>% drop_na() %>% 
  count(crop) %>%  kbl() %>%
  kable_classic()

crop_df <- 
  crop_data_Phone_Survey %>% 
  filter(!is.na(name_SIP_owner)) %>% 
  rename(District=district) %>% 
  mutate(district  = ifelse(District %in% c("Rautahat", "Bara","Sarlahi"),
                            "Rautahat_Bara_Sarlahi","Saptari")) %>% 
  select(district,1,24:33) %>% 
  gather(crop_irrigate,crop,3:12) %>% 
  select(-3) %>% distinct() %>% drop_na() %>% 
  group_by(district) %>% 
  count(crop) %>%  kbl() %>%
  kable_classic()


# POLT 
crop_table <- read_excel("~/master_research/DATAs/Phone_Survey_21.24_Jun_2021_SPIP_ICIMOD .xlsx",sheet = "crop_table")
crop_table$year <- as.character(crop_table$year)
crop_table$year <- factor(crop_table$year,levels = c("2021", "2019", "2018", "2017"))


450/300
crop_table_sap <- crop_table %>% filter(district=="Saptari") %>% 
  ggplot(aes(x = crop, y=HH, fill=year)) + 
  geom_bar(stat="identity", color="black",position =position_dodge())+coord_flip()+
  theme(panel.grid.major.y = element_blank(),text = element_text(family = "Times New Roman"))+
  scale_fill_manual(values=c("dodgerblue4","dodgerblue","dodgerblue" ,"dodgerblue"))+
  theme(axis.text.y = element_text(size=12))+labs(x=" ", y="",title = "Sapteri")+
  scale_y_continuous(breaks=c(2,5,7,10,12,15,17))

crop_table_rbs <- crop_table %>% filter(district!="Saptari") %>% 
  ggplot(aes(x = crop, y=HH, fill=year)) + 
  geom_bar(stat="identity", color="black",position =position_dodge())+coord_flip()+
  #geom_text(aes(label=HH), vjust=1.5, colour="black", size=3.5)+
  theme(panel.grid.major.y = element_blank(),panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
  scale_fill_manual(values=c("dodgerblue4","dodgerblue","dodgerblue" ,"dodgerblue"))+
  theme(axis.text.y = element_text(size=12))+labs(title = "Rautahat Bara Sarlahi", x=" ", y="")+
  scale_y_continuous(breaks=c(1,3,5,7,10,12,15))


500/700
grid.arrange(crop_table_sap,crop_table_rbs)


# Q26 27 28 what_pumps_use_for_fishfarming ----
pQ28 <- 
  post_survey_01 %>% 
  count(what_pumps_use_for_fishfarming) %>% 
  drop_na() %>% filter(what_pumps_use_for_fishfarming!="none") %>%
  mutate(freq =(round(n/sum(n), 2))) %>% 
  mutate(punp_type=c("Solar","Diesel & Electric","Diesel & Electric","Solar","Solar","Solar" ),
         what_pumps_use_for_fishfarming=ifelse(what_pumps_use_for_fishfarming == "both","3_pumps",what_pumps_use_for_fishfarming))%>% 
  arrange(punp_type,desc())


#type  steelblue
ggplot(pQ28, aes(x="", y=freq, fill=as.factor(what_pumps_use_for_fishfarming))) + 
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size=7)+
  labs(x = NULL, y = NULL, fill = NULL, title = "punp_type")+
  theme_void()+
  scale_fill_manual(values=c("steelblue4", "sienna3","sienna2"
                             ,"steelblue3", "steelblue1", "steelblue2"
                             ))
  theme(legend.position = "none")


#Q29 benefits from SIPs ----
# Did you benefit from SIPs in any of the following ways? ONLY TO BE ASKED TO THOSE WHO GOT SIPs 
# https://docs.google.com/spreadsheets/d/1X181S9_fcz8URKP1eIrwMswUjZR-TSLV/edit#gid=2108834600

# Q30.2 do_pumps_owners_sell_water_for_irri_OPINION ----
# Are there any other pump owners (SIP/diesel/electric) who sells water to farmers for irrigation ?
post_survey_01 %>% 
  count(do_pumps_owners_sell_water_for_irri_OPINION) %>% 
  drop_na() %>%  
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

#Q31 distance_in_meters_for_selling_water ----
#How far (in meters), is it possible to transfer water via lined or unlined channels, or plastic pipes genrally for those who sell water (answer even if you don't sell water yourself)
mean(post_survey_01$distance_in_meters_for_selling_water,na.rm = T)

#Q32 if_no_selling_water_why_not----
#If the farmer did not sell water to other neighboring farmers, Why didn’t he?
post_survey_01 %>% 
  count(if_no_selling_water_why_not) %>% 
#  drop_na() %>%  
    mutate(freq =(round(n/sum(n), 2))) %>% 
    #PLOT
    ggplot(aes(x="", y=freq, fill=as.factor(if_no_selling_water_why_not))) + 
    geom_bar(stat="identity", width=1)+
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(round(freq*100), "%")),
              position = position_stack(vjust = 0.5),
              color = "white", size=7)+
    labs(x = NULL, y = NULL, fill = NULL, title = "why_not_use_SIP_max_01")+
    theme_void()+
    scale_fill_manual(values=c("sienna3",
                               "sienna4",
                               "sienna1",
                               "sienna3",
                               "sienna",
                               "sienna2"))+
    theme(plot.title = element_text(size = 15,hjust = 0.5))
Multiple_choice_survey2021_sipICIMODE[c(30,31,34,35),]

#Q35 repair the pump ----
#How long did it take to repair the pump when it broke down the last time? 
  count 17
  mean 170
  mean without outlear (730) =  134
   -365 * 3farmer = 81 days
  
  
  
  sip  <-tribble(~"D", ~"Value",
                 "Mean SPIP usage days", 17, 
                 "Mean day per month", 22, 
                 "Mean day in dry month", 25, 
                 "Mean day in monsoon month" ,12 )  
  
  
  ggplot(sip, 
         aes(x=D, y=Value, fill=D)) + 
    geom_bar(stat="identity",width=0.8)+
    geom_text(aes(label = Value), position = position_dodge(0.70), vjust= 1.25, color= "black", size = 4)+
    theme_minimal()+
    ggtitle("SPIP Usage - Days a Month") +
    xlab("Rainless Days") +ylab("Days")+
    theme(text = element_text(size = 25),
          axis.title = element_text(size = 17.5),
          axis.text = element_text(size = 10),legend.position = "none") +
    theme(panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
    scale_x_discrete(limits = c("Mean SPIP usage days", "Mean day per month",
                                "Mean day in dry month"	,"Mean day in monsoon month"))+
    scale_fill_manual(values=c("khaki1","khaki3","khaki2","steelblue1"))
  
    
    
    
    geom_text(aes(label=Value), vjust=-0.3, size=2.5) %>% 
    theme(text = element_text(size = 25),
          axis.title = element_text(size = 17.5),
          axis.text = element_text(size = 10)) +
    theme(panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman")) +
    scale_fill_manual(values=c("steelblue1","steelblue1","steelblue1","steelblue1",
                               "steelblue1","steelblue4","steelblue1","steelblue1"))
  


  