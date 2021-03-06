# HA ----
HA <- water01_SEASONs %>% 
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017")) %>% 
  mutate(`Total Area Cultivated`=`Total Area Cultivated`*0.0339) %>% 
  mutate(crop_type=ifelse(crop %in% c("Paddy", "Summer Paddy","paddy"),"Paddy",
                          ifelse(crop == "Wheat","Wheat",
                                 ifelse(crops_category == "Vegetables","Vegetables",
                                        ifelse(crops_category =="Pulses","Pulses",
                                               ifelse(crop %in% c("Fish Farming","Kurli","pond"),"Fish Farming",
                                                      NA)))))) %>% 
  select(district,Seasons,HH,crop,crop_type,DIFFERENCE,Hours,`Total Area Cultivated`)
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






# HA_gap----  
gaps_irrigation <- water01_SEASONs %>%
  mutate(crop_type=ifelse(crop %in% c("Paddy", "Summer Paddy","paddy"),"Paddy",
                          ifelse(crop == "Wheat","Wheat",
                                 ifelse(crops_category == "Vegetables","Vegetables",
                                        ifelse(crops_category =="Pulses","Pulses",
                                               ifelse(crop %in% c("Fish Farming","Kurli","pond"),"Fish Farming",
                                                      NA)))))) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  select(HH,date,crop_type,district)%>%
  group_by(district,HH,crop_type) %>% 
  mutate(d1=lag(date),d2=lead(date) ) %>% 
  mutate(start=d1+1) %>%
  mutate(n=date-start) %>%
  mutate(n = ifelse(is.na(d2) | is.na(d1), "1" ,n)) %>% 
  filter(n>0) %>% 
  mutate(start.=date,end.=start,end.=lead(end.)) %>% 
  mutate(n.=end.-start.)

#    count .  days without irrigation
Ngaps_irrigation <- gaps_irrigation %>% select(district,HH,start,date,crop_type) %>%
  mutate(Irrigation= "No") %>%  rename(end = date) %>% na.omit()
#    count .  irrigation days 
Ygaps_irrigation <- gaps_irrigation %>% select(district,HH,start.,end.,crop_type) %>% mutate(Irrigation= "Yes") %>% 
  rename(start=start. ,end=end.) %>% na.omit()
# total count and gaps
NYgaps_irrigation <- rbind(Ngaps_irrigation,Ygaps_irrigation) %>%
  arrange(start) %>% mutate(gap=end-start) %>% filter(gap>0)

NYgaps_irrigation$gap <- as.numeric(NYgaps_irrigation$gap)

rm(Ygaps_irrigation,Ngaps_irrigation)

# Avg. gaps - without 30+ days gaps
HA_gap <- NYgaps_irrigation %>%filter(Irrigation=="No",gap<30) %>%
  group_by(HH,crop_type) %>%
  summarise(gap=mean(gap)) %>% 
  group_by(crop_type) %>%
  summarise(`Gap Av.`=mean(gap),`SD Gap`= sd(gap)) 




# HA_bind----
HA_bind <- 
  inner_join(HA_wa,HA_hr) %>%
  inner_join(HA_dif) %>% inner_join(HA_day) %>% inner_join(HA_gap)

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

# only fish farming plot----
HA_bind$`Watered Area in ha` SD_wa
g1 <- 
  HA_bind[,1:3] %>% #filter(crop_type == "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Watered Area in ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Watered Area in ha`-SD_wa,
                    ymax=`Watered Area in ha`+SD_wa), width=.2)+
  ggtitle("Watered Area in hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))


HA_bind$`Hours per ha` SD_hr
g2 <- 
  HA_bind[,c(1,4:5)] %>% filter(crop_type == "Fish Farming") %>% 
  mutate(`Hours per ha`=`Hours per ha`/1000 ,SD_hr= SD_hr/1000) %>% 
  ggplot(aes(x=crop_type,y=`Hours per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Hours per ha` -SD_hr,
                    ymax=`Hours per ha`+SD_hr), width=.2)+
  ggtitle("Hours of use per hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))

HA_bind$`Flow Meter vol. per ha` SD_diff
g3 <- 
  HA_bind[,c(1,6:7)] %>% filter(crop_type == "Fish Farming") %>% 
  mutate(`Flow Meter vol. per ha`=`Flow Meter vol. per ha`/1000 ,SD_diff= SD_diff/1000) %>% 
  ggplot(aes(x=crop_type,y=`Flow Meter vol. per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Flow Meter vol. per ha` -SD_diff,
                    ymax=`Flow Meter vol. per ha`+SD_diff), width=.2)+
  ggtitle("Flow Meter vol. per hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))

HA_bind$`watering days per ha`  SD_day
g4 <- 
  HA_bind[,c(1,8:9)] %>% filter(crop_type == "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`watering days per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`watering days per ha` - SD_day,
                    ymax=`watering days per ha` + SD_day, width=.2))+
  ggtitle("Watering days per hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))
grid.arrange(g2,g3,g4,g1,ncol=2)


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
  mutate(diff_per_ha=DIFFERENCE/`Total Area Cultivated`) %>% 
  group_by(district,HH,crop_type) %>% 
  summarise(diff_per_ha=mean(diff_per_ha))%>% 
  group_by(crop_type,district) %>% 
  summarise(`Flow Meter vol. per ha`=mean(diff_per_ha),SD_diff=sd(diff_per_ha))%>% 
  drop_na() %>% mutate_at (3:4,round)

# HA_day----
HA_day <- 
  HA %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(watering_days=n(),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(watering_days_per_ha=watering_days/`Total Area Cultivated`) %>% 
  group_by(district,HH,crop_type) %>% 
  summarise(watering_days_per_ha=mean(watering_days_per_ha))%>% 
  group_by(crop_type,district) %>% 
  summarise(`watering days per ha`=mean(watering_days_per_ha),SD_day=sd(watering_days_per_ha))%>% 
  drop_na() %>% mutate_at (3:4,round)



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
  inner_join(HA_dif) %>% inner_join(HA_day) %>% 
  kable() %>% kable_minimal()








# ------------------------------------------------------------------------------------
  full_join(gapNY)  #addin data about gap from Gap.R TAB

H1 <- gather(HA_bind,"per_ha","value",
             `Watering Hours per ha`,`Flow Meter vol. per ha`,
             `Watered Days per ha`,`Watered Area (in ha)`,`Gap days between irrigations`)

H2 <- gather(HA_bind,"key_sd","SD",
             `sd(Hr_ha)`,`sd(Dif_ha)`,`sd(Wd_ha)`,`sd(watered_area)`,sd_gap)

vars_per_ha <- H1[,c(1,7:8)] %>% bind_cols(H2[,8])%>% mutate_at(3:4,round,2)

kable(vars_per_ha) %>% kable_styling()
rm(HA, HA_c,HA_c.ar,HA_c.day,HA_c.dif,HA_c.hr)

