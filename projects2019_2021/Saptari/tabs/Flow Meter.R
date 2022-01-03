
#  Flow meter reading / Irrigations hours 

# bar plot: Static Water Level ----

title="Static Water Level"
subtitle =  "Average borewell depth per household by districts in meters"

xwater_level <- 
  farmers_spip_details %>%
  rename(water_level=`Static water level(m)`) %>% 
  mutate(District = ifelse(District %in% c("Rautahat", "Bara" , "Sarlahi"),
                           "Rautahat\nBara Sarlahi",
                           ifelse(District == "Saptari (Pilot)","Saptari",District))) %>% 
  select(2,3,5) %>%
  group_by(District) %>% 
  summarise(Mean=mean(water_level,na.rm = T),SD=sd(water_level,na.rm = T)) %>% 
  mutate(across(is.numeric,round,2)) %>% 
  
  ggplot(aes(x=District, y= Mean , fill=District)) +
  geom_bar(stat="identity", position=position_dodge(), width=.5) +
  geom_errorbar(aes(ymin= Mean-SD, ymax= Mean+SD), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(label=Mean), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  labs(x=" ",y="Avg. Static Water Level")+
  theme(plot.title = element_text( size = 9))+
  guides(fill=FALSE)+
  theme_minimal() +
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 10, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))



water01 %>% select(HH,FarmerName,District) %>% 
  distinct() %>% kable() %>% kable_styling()








# HH - DIFFERENCE = 0----
  water01%>% 
  filter(DIFFERENCE == 0) %>% 
  select(1) %>% distinct() %>% count()

filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001",
                  "T302603034","T309708020","T300901113")) %>% 
# general DIFFERENCE,Hours----
f_xx <- 
  water01 %>%
  select(HH,DIFFERENCE,Hours,district,) %>% 
  filter(DIFFERENCE > 0,DIFFERENCE<200)%>%
  rename(District = district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                           "Rautahat\nBara Sarlahi", District)) %>% 
  mutate(cubic_meter_per_hour=DIFFERENCE/Hours)
# table cubic_meter_per_hour---- 
X <- 
  f_xx %>% 
  group_by(District) %>%
  summarise(`Cubic Meter per Hour`=mean(cubic_meter_per_hour),
            SD=sd(cubic_meter_per_hour)) %>%
  mutate(across(is.numeric,round,2)) %>% 
  kable() %>% kable_classic()


# scatter  --            ----

ggplot(f_xx, aes(x = Hours, y=DIFFERENCE, color=District)) +
  geom_point(size = .85) + 
 # geom_smooth(method=lm, se=FALSE, fullrange=TRUE,color="black")+
  labs(x="Irrigations hours", y="Flow meter reading")+
  theme_minimal() +  
  scale_color_manual(values=c("lightsalmon4", "darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia") )

# plot : Hours vrs DIFFERENCE Only for irrigation days that are after 5-10 days without irrigation ----
#dataser and scatterplot in Gap.TAB






#  ---- time_energy ----
# Time = Energy * HP.
# Energy = water * depth.
# Therefore: Time = water * HP * depth
# and Time/water = HP * depth
# HP is the same for everyone.
# so Time/Water should be proportional to depth.

# spip_nepal_time_energy ----
spip_nepal_time_energy<-
  diary_spip_terai %>% 
  rename(flow_meter_start = `Flow Meter Reading Start`,
         flow_meter_end=`Flow Meter Reading End`,
         flow_meter_reading=DIFFERENCE,
         hours_irrigation= Hours) %>%
  select(HH,date,flow_meter_start,flow_meter_end,flow_meter_reading,hours_irrigation,precip,ghi) %>% 
  inner_join(farmers_spip_details ) %>% 
  select(HH,date,flow_meter_start,flow_meter_end,flow_meter_reading,hours_irrigation,
         pump_type_num,pump_size_HP,panel_size_watts,static_water_level_m,district,precip,ghi) %>% 
  mutate_at(6, round, 2) %>% rename(precipitations_mm=precip,solar_radiation_ghi=ghi) %>% 
  #add gap
  group_by(HH) %>% 
  mutate(d1=lag(date),d2=lead(date) ) %>% 
  mutate(start=d1+1) %>%
  mutate(n=date-start) %>%
  mutate(n = ifelse(is.na(d2) | is.na(d1), "1" ,n)) %>%rename(gap=n) %>%
  select(-c(d1,d2,start)) %>% 
  mutate(gap=ifelse(gap %in% -c(219,271,297,343,48,664,735,736,752,778,921) ,NA,gap))
# ------
spip_nepal_time_energy$date <- as.character (spip_nepal_time_energy$date)

library(data.table)
spip_nepal_time_energy$date <- as.IDate(spip_nepal_time_energy$date)

  library(foreign)
write.dta(spip_nepal_time_energy, "spip_nepal_time_energy.dta", convert.dates = TRUE)
write.csv(spip_nepal_time_energy, file = "C:/Users/Dan/Documents/master_research/DATAs/data_saptari/spip_nepal_time_energy.csv", row.names=FALSE)

T308705001  7 days  
T300307001 מד מים מקולקל
-978 גאפ



class(spip_nepal_time_energy$date)

