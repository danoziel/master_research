                            ------------------------------
#                            Gap days between irrigations
                            ------------------------------
                              gapNY
rm(gaps_irrigation)
rm(Ygaps_irrigation,Ngaps_irrigation)
rm(NYgaps_irrigation)
                            
# GENERAL        ----
gaps_irrigation <- water01 %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  #filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001",
#                   "T302603034","T309708020","T300901113")) %>% 
  select(district,HH,date,crop)%>%
  group_by(district,HH) %>% 
  mutate(d1=lag(date),d2=lead(date) ) %>% 
  mutate(start=d1+1) %>%
  mutate(n=date-start) %>%
  mutate(n = ifelse(is.na(d2) | is.na(d1), "1" ,n)) %>% 
  filter(n>0) %>% 
  mutate(start.=date,end.=start,end.=lead(end.)) %>% 
  mutate(n.=end.-start.)

#    count .  days without irrigation
Ngaps_irrigation <- gaps_irrigation %>% select(district,HH,start,date,crop) %>%
  mutate(Irrigation= "No") %>%  rename(end = date) %>% na.omit()
#    count .  irrigation days 
Ygaps_irrigation <- gaps_irrigation %>% select(district,HH,start.,end.,crop) %>% mutate(Irrigation= "Yes") %>% 
  rename(start=start. ,end=end.) %>% na.omit()
# total count and gaps
NYgaps_irrigation <- rbind(Ngaps_irrigation,Ygaps_irrigation) %>%
  arrange(start) %>% mutate(gap=end-start) %>% filter(gap>0)

rm(gaps_irrigation,Ygaps_irrigation,Ngaps_irrigation)

#    Tables      ---- 
#Avg. gaps 
NYgaps_irrigation %>%filter(Irrigation=="No") %>%  group_by(district,HH ) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(district) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`),`Gap Max`=max(`Gap Max`)) 

# Avg. gaps - without 30+ days gaps
NYgaps_irrigation %>%filter(Irrigation=="No",gap<30) %>%
  group_by(district,HH ) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(district) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`)) 
 
#    gantt chart ----
actcols <- c("#BF9000", "#2E75B6") 
ggplot(NYgaps_irrigation, aes(x=start, xend=end, y=HH, yend=HH, color=Irrigation)) +
  theme_bw()+ 
df <-df %>% add_row(x = 4)
  geom_segment(size=10) + 
  scale_color_manual(values=actcols, name="Irrigation") +
  labs(title='Irrigation Habit in Paddy - Chanda Kumari Shah', x=' ', y=' ',
       subtitle = "Irrigation days - 321 / max consecutive days-24   \ngaps between irrigation days - 569 / max consecutive days-118.")+
  theme(text = element_text(family = "Georgia"),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

rm(NYgaps_irrigation)





# SEASONs        ----
#Avg. gaps 
gaps_irrigation <- water01_SEASONs %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  select(district,HH,date,Seasons)%>%
  group_by(district,HH) %>% 
  mutate(d1=lag(date),d2=lead(date) ) %>% 
  mutate(start=d1+1) %>%
  mutate(n=date-start) %>%
  mutate(n = ifelse(is.na(d2) | is.na(d1), "1" ,n)) %>% 
  filter(n>0) %>% 
  mutate(start.=date,end.=start,end.=lead(end.)) %>% 
  mutate(n.=end.-start.)
#    count .  days without irrigation
Ngaps_irrigation <- gaps_irrigation %>% select(district,HH,start,date,Seasons) %>%
  mutate(Irrigation= "No") %>%  rename(end = date) %>% na.omit()
#    count .  irrigation days 
Ygaps_irrigation <- gaps_irrigation %>% select(district,HH,start.,end.,Seasons) %>% mutate(Irrigation= "Yes") %>% 
  rename(start=start. ,end=end.) %>% na.omit()
# total count and gaps
NYgaps_irrigation_season <- rbind(Ngaps_irrigation,Ygaps_irrigation) %>%
  arrange(start) %>% mutate(gap=end-start) %>% filter(gap>0)

NYgaps_irrigation$gap <- as.numeric(NYgaps_irrigation$gap)

rm(gaps_irrigation,Ygaps_irrigation,Ngaps_irrigation)
#    Barplot       ----
# - without 30+ days gaps
gapNY <- 
  NYgaps_irrigation_season %>%filter(Irrigation=="No",gap<30) %>%
  group_by(district,HH,Seasons ) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(district,Seasons) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`)) %>% 
  mutate(across(is.numeric,round,2)) %>% 
  filter(! Seasons %in% c("Annual 2017-2018","Annual 2018-2019","Summer 2016-2017"),
         `Gap Av.` != 6)
  
# 4 district all togather
gapNY4 <- 
  NYgaps_irrigation_season %>%filter(Irrigation=="No",gap<30) %>%
  group_by(HH,Seasons ) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(Seasons) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`)) %>% 
  mutate(district = "Total") %>% 
  select(3,1,2) %>%
  filter(Seasons %in% c("Monsoon 2018-2019","Monsoon 2019-2020","Summer 2018-2019",
                          "Winter 2018-2019","Winter 2019-2020")) %>% 
  bind_rows(gapNY) %>% 
  mutate_at(3,round,2) %>%  rename(District = district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                         "Rautahat\nBara Sarlahi", District)) %>% 
  mutate(Seasons = factor(Seasons, levels=c( 
                            "Monsoon 2017-2018", "Winter 2017-2018", "Summer 2017-2018", 
                            "Monsoon 2018-2019", "Winter 2018-2019", "Summer 2018-2019", 
                            "Monsoon 2019-2020", "Winter 2019-2020")))

title='Gaps between irrigation days'
subtitle = "The average gaps are calculated for each farmer \nIn the graph - the average of the total farmers\nIrrigation by season in the cultivated area"
  
# 1900/500
ggplot(gapNY4,aes(x=Seasons,y=`Gap Av.`,fill=District))+
  geom_bar(stat="identity", position=position_dodge(),alpha=0.9)+
  geom_text(aes(label=`Gap Av.`), vjust=1.6, color="white",
            position = position_dodge(0.9), size=6.5)+
  theme_bw()+
  labs(x=" ",y="Avg. Gap")+
  
  theme( legend.position = "none",
         axis.title = element_text(size = 30),
        axis.text.y = element_text(size = 25))+
  
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4","dimgrey"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"))
-------------------------------------------------------------------------
  # 600/250 1200/500
  ggplot(gapNY4,aes(x=CROPs,y=`Gap Av.`,fill=District))+
  geom_bar(stat="identity", position=position_dodge(),alpha=0.9)+
  geom_text(aes(label=`Gap Av.`), vjust=1.6, color="white",
            position = position_dodge(0.9), size=6.5)+
  theme_bw()+
  labs(x=" ",y="Avg. Gap")+
  theme(axis.title = element_text(size = 30),
        axis.text.x = element_text(size = 30),axis.text.y = element_text(size = 25),
        legend.text = element_text(size=25))+
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4","dimgrey"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"))
---------------------------------------------------------------------------
  
  


# plot Chanda Kumari Shah ----
actcols <- c("#BF9000", "#2E75B6") 

NYgaps_irrigation$SEASONs <- factor(NYgaps_irrigation$SEASONs,levels = c("Monsoon","Winter","Summer"))

title='   Irrigation in cultivated Area - Chanda Kumari Shah'
subtitle = "   Gaps between irrigation days \n   by season"
  
NYgaps_irrigation %>% 
  filter(
         HH %in% c("T210709003") )%>% 
  ggplot(aes(x=start, xend=end, y=HH, yend=HH, color=Irrigation)) +
  theme_bw()+
  geom_segment(size=10) + 
  facet_grid(SEASONs ~ .)+
  scale_color_manual(values=actcols, name="Irrigation") +
  labs( x=' ', y=' ')+
  theme(text = element_text(family = "Georgia"),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

# CROPs     (NYgaps_irrigation)     ----

gaps_irrigation <- water01 %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001",
                    "T302603034","T309708020","T300901113")) %>% 
  mutate(CROPs  = ifelse
         (crop %in% c("Bitter gourd" , "Bitter Gourd" , "Bottle Gourd","Brinjal",
                      "Cabbage","cauliflower","Cauliflower","Chilli","Garlic","Grass",
                      "Green Leafy Vegetables","Lady's Finger","Long Yard Beans",
                      "Luffa Gourd","Onion","Potato","Pumpkin","Radish","Ridge Gourd",
                      "Coriander","Cucumber",
                      "Sponge Gourd","Tomato","vegetable","vegetables"),"Vegetables",
           ifelse(crop %in% c("Paddy", "Summer Paddy","paddy" ), "Paddy",
                  ifelse(crop %in% c("Beans","Black Eyed Beans","Horse Gram",
                                     "Lentil","Red Kidney Beans"),"Pulses",
                         ifelse(crop == "Wheat","Wheat",crop))))) %>% 
  filter(CROPs %in% c("Paddy", "Vegetables", "Pulses","Wheat" )) %>% 
  select(HH,date,CROPs,district)%>%
  group_by(district,HH,CROPs) %>% 
  mutate(d1=lag(date),d2=lead(date) ) %>% 
  mutate(start=d1+1) %>%
  mutate(n=date-start) %>%
  mutate(n = ifelse(is.na(d2) | is.na(d1), "1" ,n)) %>% 
  filter(n>0) %>% 
  mutate(start.=date,end.=start,end.=lead(end.)) %>% 
  mutate(n.=end.-start.)

#    count .  days without irrigation
Ngaps_irrigation <- gaps_irrigation %>% select(district,HH,start,date,CROPs) %>%
  mutate(Irrigation= "No") %>%  rename(end = date) %>% na.omit()
#    count .  irrigation days 
Ygaps_irrigation <- gaps_irrigation %>% select(district,HH,start.,end.,CROPs) %>% mutate(Irrigation= "Yes") %>% 
  rename(start=start. ,end=end.) %>% na.omit()
# total count and gaps
NYgaps_irrigation <- rbind(Ngaps_irrigation,Ygaps_irrigation) %>%
  arrange(start) %>% mutate(gap=end-start) %>% filter(gap>0)

NYgaps_irrigation$gap <- as.numeric(NYgaps_irrigation$gap)

rm(Ygaps_irrigation,Ngaps_irrigation)

#    Barplot    (gapNY)   ----
# - without 30+ days gaps

# Avg. gaps - without 30+ days gaps
gapNY <- NYgaps_irrigation %>%filter(Irrigation=="No",gap<30) %>%
  group_by(district,HH,CROPs) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(district,CROPs) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`)) 

# 4 district all togather
# Avg. gaps - without 30+ days gaps
gapNY4 <- 
  NYgaps_irrigation %>%filter(Irrigation=="No",gap<30) %>%
  group_by(HH,CROPs) %>%
  summarise(`Gap Av.`=mean(gap),`Gap Max`=max(gap)) %>% 
  group_by(CROPs) %>%
  summarise(`Gap Av.`=mean(`Gap Av.`)) %>% 
  mutate(district = "Total") %>% 
  select(3,1,2) %>% rbind(gapNY) %>% 
  mutate(across(is.numeric,round,2)) %>%  rename(District = district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                           "Rautahat\nBara Sarlahi", District)) 
# 600/250 1200/500
ggplot(gapNY4,aes(x=CROPs,y=`Gap Av.`,fill=District))+
  geom_bar(stat="identity", position=position_dodge(),alpha=0.9)+
  geom_text(aes(label=`Gap Av.`), vjust=1.6, color="white",
            position = position_dodge(0.9), size=6.5)+
theme_bw()+
  labs(x=" ",y="Avg. Gap")+
  theme(axis.title = element_text(size = 30),
        axis.text.x = element_text(size = 30),axis.text.y = element_text(size = 25),
        legend.text = element_text(size=25))+
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4","dimgrey"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Times New Roman"))


# --------------------------------------------------------------------
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
gapNY <- NYgaps_irrigation %>%filter(Irrigation=="No",gap<30) %>%
  group_by(HH,crop_type) %>%
  summarise(gap=mean(gap)) %>% 
  group_by(crop_type) %>%
  summarise(`Gap Av.`=mean(gap),`SD Gap`= sd(gap)) 

# plot : Hours vrs DIFFERENCE Only for irrigation days that are after 5-10 days without irrigation ----
f_x5 <-
  NYgaps_irrigation %>% mutate(btwn=lag(gap)) %>%
  filter(Irrigation!="No") %>% select(1,2,3,5,7) %>%
  rename(date=start) %>%
  left_join(water01) %>%
  select(1,2,3,5,13,26) %>%
  rename(time=Hours,flow_meter=DIFFERENCE) %>%
  mutate(time_flowmeter=time/flow_meter) %>% 
  filter(gap>5,flow_meter>0)
  
ggplot(f_x, aes(x = Hours, y=DIFFERENCE, color=district, shape=district)) +
  geom_point(size = .85) + 
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE,color="black")+
  labs(x="Irrigations hours", y="Flow meter reading")+
  theme_minimal() +  
  scale_color_manual(values=c("lightsalmon4", "darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia")  )


res <- 
  t.test(f_x5$time_flowmeter ~ f_x10$time_flowmeter)

t.test(sales_before, sales_after,var.equal = TRUE)



