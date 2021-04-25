
#  Flow meter reading / Irrigations hours 

# bar plot: Static Water Level ----

title="Static Water Level"
subtitle =  "Average borewell depth per household by districts in meters"

xwater_level <- 
  Farmers_list %>%
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

Farmers_pump_details %>% select(2,3)%>% kable() %>% kable_styling()

HH_Farmer_name <- read.csv("~/R/Saptari/data/HH_Farmer_name.csv")
Farmers_pump_details <- Farmers_pump_details %>%
  rename(Farmer.s.Name = `Farmer's Name`)

x3 <- left_join(HH_Farmer_name,Farmers_pump_details)






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
        text = element_text(family = "Georgia")  )

# plot : Hours vrs DIFFERENCE Only for irrigation days that are after 5-10 days without irrigation ----
#dataser and scatterplot in Gap.TAB






#  ----
# Time = Energy * HP.
# Energy = water * depth.
# Therefore: Time = water * HP * depth
# and Time/water = HP * depth
# HP is the same for everyone.
# so Time/Water should be proportional to depth.
 
EN <- water01_SEASONs %>% select(-District) %>%
  filter(DIFFERENCE > 0,DIFFERENCE<700) %>% 
  inner_join(HH_Farmer_name) %>% 
  inner_join(Farmers_pump_details) %>% 
  rename(hp=`Pump size (HP)` ,depth=`Static water level(m)` ) %>% 
  mutate(energy=DIFFERENCE * depth,
         time= DIFFERENCE * depth * hp,
         `Time/Water`=time/DIFFERENCE,
         `HP*depth`=hp*depth,
         `Hours/water`=Hours/DIFFERENCE) %>% 
  filter(hp==1.5)

EN_lm <- lm(`Hours/water` ~  depth, data=EN)
summary(EN_lm)

ggplot(EN, aes( x = depth, y = `Hours/water`)) +
  geom_point() +
  stat_smooth(method = "lm")+ 
  theme_light() + ylim(0,1.5)

pairs(~ water + hp + depth, data=EN)








