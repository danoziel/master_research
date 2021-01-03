# HA ----
HA <- water01_SEASONs %>% 
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010",
                    "A0110402001")) %>% 
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017","Annual 2019-2020",
                         "Monsoon 2019-2020", "Winter 2019-2020")) %>% 
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
  drop_na() %>% mutate_at (2:3,round)

H <- 
  HA%>% 
  filter(Hours>0) %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(Hours=sum(Hours),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  filter(crop_type == "Fish Farming") 
mean(H$`Total Area Cultivated`)
  
  
  
  
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
  drop_na() %>% mutate_at (2:3,round)

#1.4 ----
HA_1_4 <- inner_join(HA_hr,HA_dif)

g1_4A <- 
  HA_1_4[,1:3] %>% #filter(crop_type != "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Hours per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Hours per ha`-SD_hr,
                    ymax=`Hours per ha`+SD_hr), width=.2)+
  ggtitle("Watered Area in hectar") +labs(x = " ",y= " ")+
#  geom_text(aes(label=`Hours per ha`), vjust=-0.3, size=4)+
  theme(text = element_text(family = "Georgia"))

g1_4B <- 
  HA_1_4[,c(1,4:5)] %>% #filter(crop_type != "Fish Farming") %>% 
  mutate(`Flow Meter vol. per ha`=`Flow Meter vol. per ha`/1000 ,SD_diff= SD_diff/1000) %>%
  mutate_at(2:3,round) %>% 
  ggplot(aes(x=crop_type,y=`Flow Meter vol. per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Flow Meter vol. per ha` -SD_diff,
                    ymax=`Flow Meter vol. per ha`+SD_diff), width=.2)+
  ggtitle("Flow Meter volum per hectar (In thousands)") +labs(x = " ",y= " ")+
#  geom_text(aes(label=`Flow Meter vol. per ha`), vjust=-0.7, size=4)+
  theme(text = element_text(family = "Georgia"))

grid.arrange(g1_4A,g1_4B,ncol=2) #850/400


# HA_day----
HA_day <- 
  HA %>% 
  group_by(Seasons,HH,crop_type) %>% 
  summarise(watering_days=n(),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(watering_days_per_ha=watering_days/`Total Area Cultivated`) %>% 
  group_by(HH,crop_type) %>% 
  summarise(watering_days_per_ha=mean(watering_days_per_ha))%>% 
  group_by(crop_type) %>% 
  summarise(`watering days per ha`=mean(watering_days_per_ha),SD_day=sd(watering_days_per_ha))%>% 
  drop_na() %>% mutate_at (2:3,round)

  
    
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
  drop_na() %>% mutate_at (2:3,round,2)

  



# HA_bind----
HA_bind <- inner_join(HA_dif,HA_hr)
  inner_join(HA_wa,HA_hr) %>%
  inner_join(HA_dif) %>% inner_join(HA_day) 

# cultivated area ----
 HA_bind$`Watered Area in ha` SD_wa
g1 <- 
  HA_bind[,1:3] %>% filter(crop_type != "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Watered Area in ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Watered Area in ha`-SD_wa,
                    ymax=`Watered Area in ha`+SD_wa), width=.2)+
  ggtitle("Watered Area \nin hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

 HA_bind$`Hours per ha` SD_hr
g2 <- 
  HA_bind[,c(1,4:5)] %>% filter(crop_type != "Fish Farming") %>% 
  mutate(`Hours per ha`=`Hours per ha`/1000 ,SD_hr= SD_hr/1000) %>% 
  ggplot(aes(x=crop_type,y=`Hours per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Hours per ha` -SD_hr,
                    ymax=`Hours per ha`+SD_hr), width=.2)+
  ggtitle("Hours of use \nper hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))

HA_bind$`Flow Meter vol. per ha` SD_diff
g3 <- 
  HA_bind[,c(1,6:7)] %>% filter(crop_type != "Fish Farming") %>% 
  mutate(`Flow Meter vol. per ha`=`Flow Meter vol. per ha`/1000 ,SD_diff= SD_diff/1000) %>% 
  ggplot(aes(x=crop_type,y=`Flow Meter vol. per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Flow Meter vol. per ha` -SD_diff,
                    ymax=`Flow Meter vol. per ha`+SD_diff), width=.2)+
  ggtitle("Flow Meter volum \nper hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))
  
HA_bind$`watering days per ha`  SD_day
g4 <- 
  HA_bind[,c(1,8:9)] %>% filter(crop_type != "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`watering days per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`watering days per ha` - SD_day,
                    ymax=`watering days per ha` + SD_day, width=.2))+
  ggtitle("Watering days \nper hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"))
  
grid.arrange(g1,g2,g3,g4,ncol=4)

# only fish farming ----
HA_bind$`Watered Area in ha` SD_wa
g1 <- 
  HA_bind[,1:3] %>% filter(crop_type == "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`Watered Area in ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`Watered Area in ha`-SD_wa,
                    ymax=`Watered Area in ha`+SD_wa), width=.2)+
  ggtitle("Watered Area \nin hectar") +labs(x = " ",y= " ")+
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
  ggtitle("Hours of use \nper hectar (In thousands)") +labs(x = " ",y= " ")+
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
  ggtitle("Flow Meter volum \nper hectar (In thousands)") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))

HA_bind$`watering days per ha`  SD_day
g4 <- 
  HA_bind[,c(1,8:9)] %>% filter(crop_type == "Fish Farming") %>% 
  ggplot(aes(x=crop_type,y=`watering days per ha`)) +
  geom_bar(stat="identity",fill="steelblue") +
  geom_errorbar(aes(ymin=`watering days per ha` - SD_day,
                    ymax=`watering days per ha` + SD_day, width=.2))+
  ggtitle("Watering days \nper hectar") +labs(x = " ",y= " ")+
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(size = 5))
grid.arrange(g1,g2,g3,g4,ncol=4)


#table by district----
# HA_hr  ----
HA_hr <- 
  HA %>% 
  filter(Hours>0) %>% 
  group_by(district,Seasons,HH,crop_type) %>% 
  summarise(Hours=sum(Hours),`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  mutate(Hours_per_ha=Hours/`Total Area Cultivated`) %>% 
  group_by(district,HH,crop_type) %>% 
  summarise(Hours_per_ha=mean(Hours_per_ha))%>% 
  group_by(crop_type,district) %>% 
  summarise(`Hours per ha`=mean(Hours_per_ha),SD_hr=sd(Hours_per_ha))%>% 
  drop_na() %>% mutate_at (3:4,round)

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

