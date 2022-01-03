# librarys      ----
library(tidyverse)
library(tableone)

library(kableExtra)
library(formattable)
library(gridExtra)
library(scales)
library(extrafont)

library(nasapower)
library(sjPlot)
library(data.table)
library(lubridate)


library(readxl)
# datasets      ------

Supplant_data_final <- read.csv("~/master_research/DATAs/data_talya_supplant/Supplant_data_final - supplant_data.csv")

supplant <- Supplant_data_final

supplant <- supplant %>% mutate(irr_follow=ifelse(irr_follow =="no","No",ifelse(irr_follow =="yes","Yes",irr_follow)))

supplant <- supplant %>% 
  select(farmer_name,week,
         daily_water_demand_m3_Field_RM,   daily_water_hrs_demand_RM,
         daily_water_reported_m3_Field_RM, daily_water_hrs_eported_RM)

irrigation_splnt <- supplant %>% 
  gather(key,value,3:6) %>% 
  mutate(irrigation=ifelse(key%in% c("daily_water_demand_m3_Field_RM",
                                    "daily_water_reported_m3_Field_RM" ),"Volume","Hours")) 

irrigation_splnt_cat <- 
  irrigation_splnt %>% 
  mutate(key=ifelse(key%in% c("daily_water_demand_m3_Field_RM",
                                     "daily_water_hrs_demand_RM" ),"recomnd","report")) %>% 
  group_by(week,irrigation,key) %>% 
  summarise(value=mean(value,na.rm = T)) 
  
  
  
  
# rain_mm ----
rain_mm_splnt <- 
  get_power(
    community = "SSE",
    lonlat = c(78.73847, 13.88456),
    pars = c("PRECTOT"),
    dates =c( "2020-01-05","2020-05-30"),
    temporal_average = "DAILY") 

rain_mm_splnt <- rain_mm_splnt %>% 
  mutate(week=rep(2:22,each=7)) %>% 
  group_by(week) %>% 
  summarise(mean_rain=mean(PRECTOT)) %>% 
  mutate_at(2:3,round,2)

# reported Vs. demand - vol & hrs ----
#    general ----
general <- 
  irrigation_splnt %>% 
  group_by(irrigation,key) %>% 
  summarise(Mean=mean(value,na.rm = TRUE)) %>%
  add_column(value = c(4.707537,2.402059,5.087987,4.426003)) %>% mutate_at(4,round,2) %>% 
  mutate(key=ifelse(key%in% c("daily_water_demand_m3_Field_RM",
                              "daily_water_hrs_demand_RM" ),"Recommendations \nof SupPlant","Report of \nfarmers"))

plothr <- general %>% filter(irrigation=="Hours") %>% 
  ggplot(aes(x=irrigation, y=value, fill=key)) + 
  geom_bar(stat="identity",width=0.8)+
  labs(x=" ", y="Hours")+ geom_text(aes(label=value), vjust=1.5, colour="white", size=3.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"darkturquoise"))+expand_limits(y=c(0, 9.6))

plotvol <- general %>% filter(irrigation=="Volume") %>% 
  ggplot(aes(x=irrigation, y=value, fill=key)) + 
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label=value), vjust=1.5, colour="white", size=3.5)+
  theme_minimal()+labs(x=" ", y="Volume in mᶟ")+
  theme(panel.grid.major.x = element_blank(),text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"goldenrod1"))+  expand_limits(y=c(0, 9.6))

500/400
grid.arrange(plothr, plotvol, ncol=3)

legend <- 
  general %>% 
  ggplot(aes(x=irrigation, y=value, fill=key)) + geom_bar(stat="identity",width=0.8)+geom_text(aes(label=value), vjust=1.5, colour="white", size=3.5)+
  theme_minimal()+labs(x=" ", y="Volume in mᶟ")+theme(text = element_text(family = "Times New Roman"),legend.text = element_text(size=15)) 

  
#    farmer_level <- ----
irrigation_splnt %>% 
  filter(farmer_name!="N.Gopinath reddy") %>% 
  group_by(irrigation,farmer_name,key) %>% 
  summarise(value=mean(value,na.rm = TRUE)) %>% 
  mutate(key=ifelse(key%in% c("daily_water_demand_m3_Field_RM",
                              "daily_water_hrs_demand_RM" ),"recomnd","report"))

farmer_recomnd <- farmer_level %>% filter(key =="recomnd") %>% mutate(recomnd=value) %>% select(farmer_name,irrigation,recomnd)
farmer <- farmer_level %>% filter(key =="report") %>% mutate(report=value)%>% select(farmer_name,irrigation,report) %>% 
  inner_join(farmer_recomnd) %>% mutate(back=recomnd-report) %>% select(-recomnd) %>% gather(key,value,3:4) %>%# filter(week!=2) %>% 

farmer_hr <- 
  farmer %>% filter(irrigation=="Hours") %>% 
  ggplot(aes(x=farmer_name, y=value, fill=key)) + 
  geom_bar(stat="identity")+
  theme_minimal()+   labs(x=" ", y="Hours")+ylim(-1,17)+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"darkturquoise"))

farmer_vol <- 
  farmer %>% filter(irrigation=="Volume") %>% 
  mutate(value=ifelse(value<0,-0.978983,value)) %>% 
  
  ggplot(aes(x=farmer_name, y=value, fill=key)) + 
  geom_bar(stat="identity")+
  theme_minimal()+   labs(x=" ", y="Volume in mᶟ")+ylim(-1,17)+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"goldenrod1"))

600/350
grid.arrange(farmer_hr, farmer_vol, nrow=2)






#    weekly <- ----
weekly <-
irrigation_splnt %>% 
  mutate(key=ifelse(key%in% c("daily_water_demand_m3_Field_RM",
                              "daily_water_hrs_demand_RM" ),"recomnd","report")) %>% 
  group_by(week,irrigation,key) %>% 
  summarise(value=mean(value,na.rm = T))

week_recomnd <- weekly %>% filter(key =="recomnd") %>% mutate(recomnd=value) %>% select(week,irrigation,recomnd)
weekly_irri <- weekly %>% filter(key =="report") %>% mutate(report=value)%>% select(week,irrigation,report) %>% 
  inner_join(week_recomnd) %>% mutate(back=recomnd-report) %>% select(-recomnd) %>% gather(key,value,3:4) %>% filter(week!=2) %>% 
  mutate(value2=ifelse(week %in% c(13,14,15),NA,value)) %>%
  inner_join(rain_mm_splnt)
rm(week_recomnd,weekly)

plot_hr <- 
  weekly_irri %>% filter(irrigation=="Hours") %>% 
  ggplot(aes(x=week, y=value2, fill=key)) + 
  geom_bar(stat="identity")+
  geom_line(aes(x=week, y=mean_rain),stat="identity",color="black",size = 1)+
  #  geom_line(aes(x=week, y=sum_rain),stat="identity",color="blue",size = 1)+
  theme_minimal()+   labs(x=" ", y="Hours")+
  scale_x_continuous(breaks = seq(3, 21, 1))+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_y_continuous(sec.axis=sec_axis(~.*10,name="mm"))+
  scale_fill_manual(values=c("gray92" ,"darkturquoise"))

plot_vol <- 
  weekly_irri %>% filter(irrigation=="Volume") %>% 
  ggplot(aes(x=week, y=value2, fill=key)) + 
  geom_bar(stat="identity")+
  geom_line(aes(x=week, y=mean_rain),stat="identity",color="black",size = 1)+
  #  geom_line(aes(x=week, y=sum_rain),stat="identity",color="blue",size = 1)+
  theme_minimal()+   labs(x=" ", y="Volume in mᶟ")+
  scale_x_continuous(breaks = seq(3, 21, 1))+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_y_continuous(sec.axis=sec_axis(~.*10,name="mm"))+
  scale_fill_manual(values=c("gray92" ,"goldenrod1"))

600/350
grid.arrange(plot_hr, plot_vol, nrow=2)
rm(plot_hr,plot_vol)

  
# "Volume","Hours" coralletion ----

# bar plot
irrigation_splnt %>% 
  filter(week!= 2, key %in% c("daily_water_reported_m3_Field_RM","daily_water_hrs_eported_RM")) %>% 
  group_by(irrigation,week) %>% 
  summarise(Mean=mean(value,na.rm = TRUE)) %>% 
  mutate(Mean=ifelse(week %in% c(13,14,15),NA ,Mean)) %>% 
  ggplot(aes(x=week, y=Mean, fill=irrigation)) + 
  geom_bar(stat="identity", position=position_dodge())+labs(x="Week")+ theme_minimal()+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("darkturquoise" ,"goldenrod1"))


#  scatter plot
ggplot(supplant, aes(x=daily_water_reported_m3_Field_RM, y=daily_water_hrs_eported_RM)) + 
  geom_point(size = 2,color="gray52")+geom_smooth(method=lm,color="steelblue4",size=1.25)+
  labs(x="Volume", y="Hours")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman")  )

# LM
co <- supplant %>% drop_na(daily_water_reported_m3_Field_RM) %>% drop_na(daily_water_hrs_eported_RM) 
cor.test(co$daily_water_reported_m3_Field_RM,co$daily_water_hrs_eported_RM)

supplant.lm <- lm(daily_water_reported_m3_Field_RM ~ daily_water_hrs_eported_RM, data = supplant)

summary(supplant.lm)

tab_model(supplant.lm,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf.Int(95%)",
          dv.labels = c("Daily Water Reported: Volum(m3) ~ Hours"),
          pred.labels = c("(Intercept)", "Houres reported"))




# recomandations ----

300/350
recomandations_01 <- 
supplant %>% 
  count(irr_follow) %>%drop_na(irr_follow) %>% 
  mutate(prop = prop.table(n)) %>% mutate(rec="recomandations") %>%  
  
  ggplot(aes(x=rec, y=n, fill=irr_follow)) + 
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label=n), vjust=1.5, colour="white", size=3.5)+
  theme_minimal()+labs(x=" ", y="Total recomandations")+
  theme(panel.grid.major.x = element_blank(),text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"black"))+
  scale_y_continuous(breaks=c(0, 46, 160))


supplant %>% 
  count(farmer_name,irr_follow) %>%
  drop_na(irr_follow) %>% 
  group_by(farmer_name) %>%        
  mutate(prop = prop.table(n)) %>% 
  group_by(irr_follow) %>% 
  summarise(mean(prop))
  

recomandations_02 <- 
  supplant %>% 
  count(farmer_name,irr_follow) %>%
  drop_na(irr_follow) %>% 
  group_by(farmer_name) %>%        
  mutate(prop = prop.table(n)) %>% 
  
  ggplot(aes(x=farmer_name, y=n, fill=irr_follow)) + 
  geom_bar(stat="identity",width=0.8)+
  theme_minimal()+labs(x=" ", y="Total recomandations")+
  geom_text(aes(label=n), vjust=1.5, colour="white", size=3.5)+
  theme(panel.grid.major.x = element_blank(),text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("gray92" ,"black"))+
  theme(axis.text.x = element_text(angle=90))
  


# cumulative volume ----

cumulative <-
  Supplant_data_final %>% #filter(farmer_name %in% c( "B. Ramana reddy", "T.Bayyareddy")) %>% 
  rename(recomnd=cumulative_daily_water_demand_m3_Field_RM,report=cumulative_m3_weekly_reported_RM) %>% 
  select(farmer_name,week,report,recomnd) %>% 
  gather(key,value,3:4 ) %>% group_by(farmer_name,week,key) %>% summarise(value=mean(value,na.rm = T))

cumulative_recomnd <- cumulative %>% filter(key =="recomnd") %>% mutate(recomnd=value) %>% select(farmer_name,week,recomnd)
cumulative <- cumulative %>% filter(key =="report") %>% mutate(report=value)%>% select(farmer_name,week,report) %>% 
  inner_join(cumulative_recomnd) %>% mutate(x = ifelse(is.na(report), 0, report)) %>% 
  mutate(back=recomnd-x) %>% select(-c(recomnd,x)) %>% gather(key,value,3:4) %>% inner_join(rain_mm_splnt)
rm(cumulative_recomnd)

  cumulative %>% #filter(farmer_name %in% c( "M. Vijay Kumar Reddy")) %>% 
  ggplot(aes(x=week, y=value, fill=key)) + 
  geom_bar(stat="identity")+
 # geom_line(aes(x=week, y=mean_rain),stat="identity",color="black",size = 0.25)+
  facet_wrap(~ farmer_name, ncol = 2)+
  theme_minimal()+   labs(title= "Cubic meter volume accumulates from week to week", x=" ", y="Volume in mᶟ")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"),legend.position = "none")+
  scale_fill_manual(values=c("goldenrod1", "gray92" ))

  


