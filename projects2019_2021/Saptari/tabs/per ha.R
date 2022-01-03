# HA ----

#  "A104507035","E0104705010", "A110402001", "A0110402001" HH pilot
#   T300406135  returned the pump no data in monitorung
#  "E0104705010" pump broke

#HH with few observations
# T210701004  (18) |  year 2018-2019 missung | Saptari 
# E0104705010 (22) | Summer_2017_2018	         | Sapteri
# A0110402001 (39) | 2017 missing              | Sapteri
# T301911010       | no 2019-2020              | rbs

# T302603034(11) T309708020(14) T300901113(12) | monsoon 18-19 | Paddy | Bara


filter(!HH %in% c("A104507035","E0104705010", "A110402001", "A0110402001")) %>% 
  

HA <- water01_SEASONs %>% 
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>%
# filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  filter(!HH %in% c("A104507035","E0104705010", "A110402001", "A0110402001")) %>% 
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017")) %>% 
  mutate(`Total Area Cultivated`=`Total Area Cultivated`*0.0339) %>% 
  mutate(crop_type=ifelse(crop %in% c("Paddy", "Summer Paddy","paddy"),"Paddy",
                          ifelse(crop == "Wheat","Wheat",
                                 ifelse(crops_category == "Vegetables","Vegetables",
                                        ifelse(crops_category =="Pulses","Pulses",
                                               ifelse(crop %in% c("Fish Farming","Kurli","pond"),"Fish Farming",
                                                      NA)))))) %>% 
  select(district,Seasons,HH,crop,crop_type,DIFFERENCE,Hours,`Total Area Cultivated`)

# HH count----
HAAA <- 
  HA %>% group_by(district,crop_type,HH) %>% 
  summarise(sum(Hours)) %>% count()
# HA_hr  ----
HA_hr <- 
  HA%>% 
  filter(Hours>0) %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(Hours=sum(Hours),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(Hours_per_ha=Hours/`Total Area Cultivated`) %>% 
  group_by(HH,crop_type) %>% 
  summarise(Hours_per_ha=mean(Hours_per_ha))%>% 
  group_by(crop_type) %>% 
  summarise(`Hours per ha`=mean(Hours_per_ha),SD_hr=sd(Hours_per_ha))%>% 
  drop_na() 

# HA_dif----
HA_dif <-
  HA%>% 
  filter(DIFFERENCE>0) %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(DIFFERENCE=sum(DIFFERENCE),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>%
  mutate(diff_per_ha=DIFFERENCE/`Total Area Cultivated`) %>% 
  group_by(HH,crop_type) %>% 
  summarise(diff_per_ha=mean(diff_per_ha))%>% 
  group_by(crop_type) %>% 
  summarise(`Flow Meter vol. per ha`=mean(diff_per_ha),SD_diff=sd(diff_per_ha))%>% 
  drop_na() 

# HA_day----
HA_day <- 
  HA %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(watering_days=n(),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(watering_days_per_ha=watering_days/`Total Area Cultivated`) %>% 
  group_by(HH,crop_type) %>% 
  summarise(watering_days_per_ha=mean(watering_days_per_ha))%>% drop_na() %>% 
  group_by(crop_type) %>% 
  summarise(`watering days per ha`=mean(watering_days_per_ha),SD_day=sd(watering_days_per_ha))



# HA_wa Watered_area ----
HA_wa <- 
  HA %>%
  select(Seasons,HH,crop_type,crop,`Total Area Cultivated`) %>% 
  filter(`Total Area Cultivated`>0) %>% 
  group_by(Seasons,HH,crop_type,crop) %>% 
  summarise(`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(sum_veg=sum(`Total Area Cultivated`)) %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(Watered_area=max(sum_veg)) %>%
  group_by(HH,crop_type) %>% 
  summarise(Watered_area=mean(Watered_area))%>% 
  group_by(crop_type) %>% 
  summarise(`Watered Area in ha`=mean(Watered_area),SD_wa=sd(Watered_area))%>% 
  drop_na() 




# HA_bind----
HA_bind <- 
  inner_join(HA_wa,HA_hr) %>%
  inner_join(HA_dif) %>% inner_join(HA_day) 
# HA_bind plot----

 HA_bind$`Watered Area in ha` SD_wa
g1 <- 
  HA_bind[,1:3] %>% #filter(crop_type != "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Watered Area in ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Watered Area in ha`-SD_wa,
                    ymax=`Watered Area in ha`+SD_wa), width=.2)+
  ggtitle("Watered Area in hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

 HA_bind$`Hours per ha` SD_hr
g2 <- 
  HA_bind[,c(1,4:5)] %>% # filter(crop_type != "Fish Farming") %>% 
  mutate(`Hours per ha`=`Hours per ha`/1000 ,SD_hr= SD_hr/1000) %>% 
  ggplot(aes(x=crop_type,y=`Hours per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Hours per ha` -SD_hr,
                    ymax=`Hours per ha`+SD_hr), width=.2)+
  ggtitle("Hours of use per hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

HA_bind$`Flow Meter vol. per ha` SD_diff
g3 <- 
  HA_bind[,c(1,6:7)] %>% #filter(crop_type != "Fish Farming") %>% 
  mutate(`Flow Meter vol. per ha`=`Flow Meter vol. per ha`/1000 ,SD_diff= SD_diff/1000) %>% 
  ggplot(aes(x=crop_type,y=`Flow Meter vol. per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Flow Meter vol. per ha` -SD_diff,
                    ymax=`Flow Meter vol. per ha`+SD_diff), width=.2)+
  ggtitle("Flow Meter volum per hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))
  
HA_bind$`watering days per ha`  SD_day
g4 <- 
  HA_bind[,c(1,8:9)] %>% #filter(crop_type != "Fish Farming") %>% 
  mutate(`watering days per ha`=`watering days per ha`/1000,
         SD_day=SD_day/1000) %>% 
  ggplot(aes(x=crop_type,y=`watering days per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`watering days per ha` - SD_day,
                    ymax=`watering days per ha` + SD_day, width=.2))+
  ggtitle("Watering days per hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

HA_bind$`Gap Av.` `SD Gap`
g5 <- 
  HA_bind[,c(1,10:11)] %>% #filter(crop_type != "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Gap Av.`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Gap Av.` - `SD Gap`,
                    ymax=`Gap Av.` + `SD Gap`, width=.2))+
  ggtitle("Gap days") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

  
grid.arrange(g2,g3,g4,g5,ncol=2)




#table by district----
# HA_hr  ----
HA_hr <- 
  HA %>% 
  filter(Hours>0) %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(Hours=sum(Hours),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  drop_na() %>%
  mutate(Hours_per_ha=Hours/`Total Area Cultivated`) %>% 
  group_by(district,HH,crop_type) %>% 
  summarise(Hours_per_ha=mean(Hours_per_ha))%>% 
  group_by(crop_type,district) %>% 
  summarise(`Hours per ha`=mean(Hours_per_ha),SD_hr=sd(Hours_per_ha))%>% 
  mutate_at (3:4,round)

# HA_dif----
HA_dif <-
  HA%>% 
  filter(DIFFERENCE>0) %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(DIFFERENCE=sum(DIFFERENCE),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>%
  mutate(diff_per_ha=DIFFERENCE/`Total Area Cultivated`) %>%   drop_na() %>% 
  group_by(district,HH,crop_type) %>% 
  summarise(diff_per_ha=mean(diff_per_ha))%>% 
  
  group_by(crop_type,district) %>% 
  summarise(`Flow Meter vol. per ha`=mean(diff_per_ha),SD_diff=sd(diff_per_ha),n())%>% 
  mutate_at (3:4,round)

# HA_day----
HA_day <- 
  HA %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(watering_days=n(),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(watering_days_per_ha=watering_days/`Total Area Cultivated`) %>% drop_na() %>%
  group_by(district,HH,crop_type) %>% 
  summarise(watering_days_per_ha=mean(watering_days_per_ha))%>% 
  group_by(crop_type,district) %>% 
  summarise(`watering days per ha`=mean(watering_days_per_ha),SD_day=sd(watering_days_per_ha))%>% 
   mutate_at (3:4,round)



# HA_wa Watered_area ----
HA_wa <- 
  HA %>%
  select(district,Seasons,HH,crop_type,crop,`Total Area Cultivated`) %>% 
  filter(`Total Area Cultivated`>0) %>% 
  group_by(district,Seasons,HH,crop_type,crop) %>% 
  summarise(`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(sum_veg=sum(`Total Area Cultivated`)) %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(Watered_area=max(sum_veg)) %>%
  group_by(district,HH,crop_type) %>% 
  summarise(Watered_area=mean(Watered_area))%>% 
  group_by(crop_type,district) %>% 
  summarise(`Watered Area in ha`=mean(Watered_area),SD_wa=sd(Watered_area))%>% 
  drop_na() %>% mutate_at (3:4,round,2)


# HA_bind----

HA_bind <- 
  inner_join(HA_wa,HA_hr) %>%
  inner_join(HA_dif) %>% inner_join(HA_day)

kable(HA_bind) %>% kable_minimal()



grid.arrange(
  plot1_2,  plot1,
  plot2_2,  plot2,
  plot3_2,  plot3,
  plot4_2,  plot4,
  ncol=4,
  top = "Title of the page"
)


# ------------------------------------------------------------------------------------
  full_join(gapNY)  #addin data about gap from Gap.R TAB

H1 <- 
  HA_bind %>% 
  select(crop_type, district,`Watered Area in ha`,`Hours per ha`,`watering days per ha`,`Flow Meter vol. per ha`) %>% 
  gather("per_ha","value",`Watered Area in ha`,`Hours per ha`,`watering days per ha`,`Flow Meter vol. per ha`)
  


H2 <- gather(HA_bind,"key_sd","SD",
             `sd(Hr_ha)`,`sd(Dif_ha)`,`sd(Wd_ha)`,`sd(watered_area)`,sd_gap)

vars_per_ha <- H1[,c(1,7:8)] %>% bind_cols(H2[,8])%>% mutate_at(3:4,round,2)

kable(vars_per_ha) %>% kable_styling()
rm(HA, HA_c,HA_c.ar,HA_c.day,HA_c.dif,HA_c.hr)

----------------------------------------------------------------
  
  
plot1 <- H1 %>% ggplot(aes(x=crop_type,y= value,fill=district)) +
  geom_bar(stat="identity",fill="steelblue") +
  ggtitle(" ") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Times New Roman"))

# 1200/750 ----
ggplot(data = H1,aes(x=crop_type)) +
  geom_bar(aes(y=value,fill=district),stat="identity",position="dodge",alpha=.7) +
  labs(x = " ",y="Area size (in ha)")+
#  geom_text(aes( y = value,label = value), vjust= 1.6, color= "black", size = 4)+
#  facet_grid( ~ per_ha) + 
#  facet_wrap(~per_ha,nrow=2,ncol=2)+
  theme_minimal() +
  theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))

  scale_x_discrete(limits = c("total own land", "monsoon_2017_2018", "winter_2017_2018",
                              "summer_2017_2018",
                              "monsoon_2018_2019", "winter_2018_2019",
                              "summer_2018_2019"))+

    
#----

  
  # plot1_2 for the legend ----
  plot1_2 <- 
    HA_wa %>% filter(crop_type == "Fish Farming") %>% 
    rename(District = district) %>% 
    mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi","Rautahat, Bara & Sarlahi",District)) %>% 
    ggplot(aes(x=crop_type)) +
    geom_bar(width=0.27,aes(y=`Watered Area in ha`,fill=District),stat="identity",position="dodge",alpha=.7) +
    labs(x = " ",y=" ")+
    theme_minimal() +ggtitle("District") +
    theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
    theme(axis.text.x = element_text(angle = 0))+
    theme(legend.text = element_text(size = 17),legend.position="bottom",
          legend.title = element_text(size=20))+
    scale_y_continuous(breaks=c(0.25,0.5,1)) +
    scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))
  
  
  
  # plot2 ----
  plot2 <- 
    HA_hr %>% mutate(`Hours per ha`= `Hours per ha` /1000) %>% 
    mutate(crop_type=ifelse(crop_type == "Fish Farming","Fish \nFarming", crop_type)) %>% 
    ggplot(aes(x=crop_type)) +
    geom_bar(aes(y=`Hours per ha`,fill=district),stat="identity",position="dodge",alpha=.7) +
    labs(x = " ",y=" ")+ggtitle("Hours per ha (In thousands)") +
    theme_minimal() +
    theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
    theme(axis.text.x = element_text(size = (17)),axis.text.y = element_text(size = (15)),
          legend.position="none",plot.title = element_text(hjust = 0.5,size = (25)))+
    scale_y_continuous(breaks=c(1,2,3,4,5)) +
    scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))
  
  # plot3 ----
  plot3 <- 
    HA_dif %>%  mutate(`Flow Meter vol. per ha`=`Flow Meter vol. per ha`/1000) %>% 
    mutate(crop_type=ifelse(crop_type == "Fish Farming","Fish \nFarming", crop_type)) %>% 
    ggplot(aes(x=crop_type)) +
    geom_bar(aes(y=`Flow Meter vol. per ha`,fill=district),stat="identity",position="dodge",alpha=.7) +
    labs(x = " ",y=" ")+ggtitle("Flow Meter vol. per ha (In thousands)") +
    theme_minimal() +
    theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
    theme(axis.text.x = element_text(size = (17)),axis.text.y = element_text(size = (15)),
          legend.position="none",plot.title = element_text(hjust = 0.5,size = (25)))+
    scale_y_continuous(breaks=c(5,10,20,30)) +
    scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))
  
  #plot4 ----
  plot4 <- 
    HA_day %>% mutate(`watering days per ha`= `watering days per ha`/1000) %>% 
    mutate(crop_type=ifelse(crop_type == "Fish Farming","Fish \nFarming", crop_type)) %>% 
    ggplot(aes(x=crop_type)) +
    geom_bar(aes(y=`watering days per ha`,fill=district),stat="identity",position="dodge",alpha=.7) +
    labs(x = " ",y=" ")+ ggtitle("watering days per ha (In thousands)")+
    theme_minimal() +
    theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
    theme(axis.text.x = element_text(size = (17)),axis.text.y = element_text(size = (15)),
          legend.position="none",plot.title = element_text(hjust = 0.5,size = (25)))+
    scale_y_continuous(breaks=c(0.1,0.25,0.50,0.75,1)) +
    scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))
  # grid.arrange2 ----
  
  grid.arrange(plot2,plot3,plot4,
               ncol=3
#    top = "Title of the page"
  )
  
  
# flow metwr only----

  HA_dif <-
    HA%>% 
    filter(DIFFERENCE>0) %>% 
    group_by(district,Seasons,HH,crop_type) %>% 
    summarise(DIFFERENCE=sum(DIFFERENCE),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>%
    mutate(diff_per_ha=DIFFERENCE/`Total Area Cultivated`) %>%   drop_na() %>% 
    group_by(district,HH,crop_type) %>% 
    summarise(diff_per_ha=mean(diff_per_ha))%>%
    mutate(diff_per_ha=diff_per_ha/1000) %>% 
    group_by(district,crop_type) %>% 
    summarise( 
      n=n(),
      mean=mean(diff_per_ha),
      sd=sd(diff_per_ha)
    ) %>%
    mutate( se=sd/sqrt(n))  %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>% 
    mutate(crop_type=ifelse(crop_type == "Fish Farming","Fish \nFarming", crop_type)) %>% 
    mutate_at (4:7,round,2)
  
  
# 850/650
  HA_dif %>% 
    ggplot(aes(x=crop_type,  y=mean ,fill=district)) + 
      geom_bar(stat="identity",position="dodge",alpha=.7) +
      geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=.2,
                    position=position_dodge(.9))+
      labs(x = " ",y=" ")+ggtitle("Flow Meter vol. per ha (In thousands)") +
      theme_minimal() +
      theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
      theme(axis.text.x = element_text(size = (20)),axis.text.y = element_text(size = (17)),legend.position="none",
            plot.title = element_text(hjust = 0.5,size = (30)))+
      scale_y_continuous(breaks=c(5,10,20,30)) +
      scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  