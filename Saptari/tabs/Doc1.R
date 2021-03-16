
#percentage of days----
# Omit - 30 days in a row without irrigation
# (THE `NYgaps_irrigation` - FROM `Gap.R` Tab)

sp_usege <-
  NYgaps_irrigation %>%
  filter(Irrigation=="Yes" | gap<30 & Irrigation == "No" ) 
sp_usege$gap <-as.numeric(sp_usege$gap)

sp_usege_per <- sp_usege %>%
  group_by(HH,Irrigation) %>%
  summarise(gap=sum(gap)) %>% 
  spread(Irrigation,gap) %>%  
  mutate(total_days=Yes+No,Yes=Yes+1,percentage=Yes/total_days)

filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001",
                  "T302603034","T309708020","T300901113"))

# PLOT # freq hist 450/200                
mean(sp_usege_per$percentage)
ggplot(sp_usege_per) + 
  geom_histogram(aes(x = percentage ,y=stat(count)/sum(stat(count))),
                 color= "gray20", fill = "white", breaks=seq(0, 1, by =0.10)) +
  scale_y_continuous(labels = scales::percent)+
  geom_vline(aes(xintercept = mean(percentage)),linetype = "dashed", size = 0.7)+
  labs(title = "54 HH use SPIP \navg. percentage of use stands at 47% (mark in dash line).",
       x="Percentages of usage days", y="HH frequency")+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 9, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))





sp_usege_seasons <-
  NYgaps_irrigation_season %>%
  filter(Irrigation=="Yes" | gap<30 & Irrigation == "No" ) 
sp_usege_seasons$gap <-as.numeric(sp_usege_seasons$gap)

sp_usege_per_seasons <- sp_usege_seasons %>%
  group_by(SEASONs,HH,Irrigation) %>%
  summarise(gap=sum(gap)) %>% 
  spread(Irrigation,gap) %>%  
  mutate(total_days=Yes+No,Yes=Yes+1,percentage=Yes/total_days)

Master_HH_N %>%  
  inner_join(sp_usege_per_seasons) %>% 
  group_by(SEASONs) %>%
  summarise(mean(percentage,na.rm = T)) %>% 
  kable() %>% kable_styling()

#houres per day ----
water01 %>% 
  group_by(HH,date) %>% 
  summarise(hr=sum(Hours)) %>%
  group_by(HH) %>% 
  summarise(hr=mean(hr)) %>% 
  summarise(mean(hr))

water01 %>% 
  group_by(district,HH,date) %>% 
  summarise(hr=sum(Hours)) %>%
  group_by(district,HH) %>% 
  summarise(hr=mean(hr)) %>% 
  summarise(mean(hr))

sip  <-tribble(~" ", ~"Avg. SPIP usage per day", ~"Avg. sunshine duration hours per day",
               "Saptari", 6.44, 6.98, 
               "Rautahat Bara Sarlahi" ,5.18, 6.27,
               "Total",5.88 ,6.61 )
kable(sip) %>% kable_styling()






# Total hours of SPIP use  YEAR----
x <- water01_SEASONs %>% 
  group_by(district,HH) %>%  
  summarise(Hours=sum(Hours)) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  group_by(district) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round)) %>% 
  kable() %>% kable_styling()

# c f cf   By Year----
x <- water01_SEASONs %>% 
  inner_join(Master_HH_N) %>% 
  group_by(district,HH,var3) %>%  
  summarise(Hours=sum(Hours)) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>% 
  group_by(district,var3) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round)) %>% 
  kable() %>% kable_styling()
-
  #   plot 600/500 +SD
x %>% rename(District = district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                           "Rautahat\nBara Sarlahi", District)) %>% 
  
  ggplot(aes(x=var3, y= `Total Hours`, fill=District)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=`Total Hours`-SD, ymax=`Total Hours`+SD), width=.2,
                position=position_dodge(.9))+
  
  geom_text(aes(label=`Total Hours`), vjust=1.6, color="white",
            position = position_dodge(.9), size=3.5)+
  labs(title="Total hours for the period of holding the SPIP", x=" ")+
  theme(plot.title = element_text( size = 9))+
  theme_minimal() +
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4","dimgrey"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
-
  


# c cf     By Season ----


water01_SEASONs %>% filter(!is.na(SEASONs)) %>% 
  group_by(district,SEASONs,HH) %>% 
  summarise(Hours=sum(Hours)) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  group_by(district,SEASONs) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))%>% 
  kable() %>% kable_styling()

# c cf   by Crop ----
# Paddy season
water01 %>% inner_join(Master_HH_N) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  filter(crop %in% c("Summer Paddy","Paddy","paddy") )%>% 
  group_by(district,HH,crop) %>% 
  summarise(Hours=sum(Hours)) %>% 
  group_by(district,crop) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round)) %>% 
  kable() %>% kable_styling()

# Paddy total
water01 %>% inner_join(Master_HH_N) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  filter(crop %in% c("Summer Paddy","Paddy","paddy") )%>% 
  group_by(district,HH) %>% 
  summarise(Hours=sum(Hours)) %>% 
  group_by(district) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round)) %>% 
  kable() %>% kable_styling()

# vegetables vars
water01 %>% inner_join(Master_HH_N) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  filter(crop %in% c("Bitter gourd" , "Bitter Gourd" , "Bottle Gourd","Brinjal","Cabbage",
                     "cauliflower","Cauliflower","Chilli","Garlic","Grass",
                     "Green Leafy Vegetables","Lady's Finger","Long Yard Beans",
                     "Luffa Gourd","Onion","Potato","Pumpkin","Radish","Ridge Gourd",
                     "Sponge Gourd","Tomato","vegetable","vegetables")) %>% 
  group_by(district,HH,var3) %>% 
  summarise(Hours=sum(Hours)) %>% 
  group_by(district,var3) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))%>% 
  kable() %>% kable_styling()

# vegetables total
water01 %>% inner_join(Master_HH_N) %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>% 
  filter(crop %in% c("Bitter gourd" , "Bitter Gourd" , "Bottle Gourd","Brinjal","Cabbage",
                     "cauliflower","Cauliflower","Chilli","Garlic","Grass",
                     "Green Leafy Vegetables","Lady's Finger","Long Yard Beans",
                     "Luffa Gourd","Onion","Potato","Pumpkin","Radish","Ridge Gourd",
                     "Sponge Gourd","Tomato","vegetable","vegetables")) %>% 
  group_by(district,HH) %>% 
  summarise(Hours=sum(Hours)) %>% 
  group_by(district) %>%  
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))%>% 
  kable() %>% kable_styling()

  
# irrigation  types of crops per ha? Veg, rice, fish pond

# vegetable
x <- 
water01 %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>%
  filter (HH != "T210709003") %>%  
  filter(crop %in% c("Bitter gourd" , "Bitter Gourd" , "Bottle Gourd","Brinjal","Cabbage",
                     "cauliflower","Cauliflower","Chilli","Garlic","Grass",
                     "Green Leafy Vegetables","Lady's Finger","Long Yard Beans",
                     "Luffa Gourd","Onion","Potato","Pumpkin","Radish","Ridge Gourd",
                     "Sponge Gourd","Tomato","vegetable","vegetables")) %>% 
  select(HH,date,crop,`Total Area Irrigated`,Seasons,Hours,district) %>% 
  mutate(`Total Area Irrigated`=`Total Area Irrigated`*0.0339) %>% 
  group_by(district,HH) %>% 
  summarise(`Area irrigated`=sum(`Total Area Irrigated`,na.rm = T),
            `Irrigation hours`=sum(Hours)) %>% 
  mutate(`Area Irrigated for 1 ha`=`Irrigation hours`/`Area irrigated`) %>% 
  group_by(district) %>% 
  summarise(`Area irrigated`=mean(`Area irrigated`),
            `Area Irrigated for 1 ha`=mean(`Area Irrigated for 1 ha`)) %>% 
  kable() %>% kable_styling()
  
#Paddy
x <- 
  water01 %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>%
  filter(crop %in% c("Summer Paddy","Paddy","paddy") )%>% 
  select(HH,date,crop,`Total Area Irrigated`,Seasons,Hours,district) %>% 
  mutate(`Total Area Irrigated`=`Total Area Irrigated`*0.0339) %>% 
  group_by(district,HH) %>% 
  summarise(`Area irrigated`=sum(`Total Area Irrigated`,na.rm = T),
            `Irrigation hours`=sum(Hours)) %>% 
  mutate(`Area Irrigated for 1 ha`=`Irrigation hours`/`Area irrigated`) %>% 
  group_by(district) %>% 
  summarise(`Area irrigated`=mean(`Area irrigated`),
            `Area Irrigated for 1 ha`=mean(`Area Irrigated for 1 ha`)) %>% 
  kable() %>% kable_styling()

#Wheat
xWheat <- 
  water01 %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>%
  filter(crop %in% c("Wheat") )%>% 
  select(HH,date,crop,`Total Area Irrigated`,Seasons,Hours,district) %>% 
  mutate(`Total Area Irrigated`=`Total Area Irrigated`*0.0339) %>% 
  group_by(district,HH) %>% 
  summarise(`Area irrigated`=sum(`Total Area Irrigated`,na.rm = T),
            `Irrigation hours`=sum(Hours))

fix(xWheat) # T104209001	0.0=0.1352 # T300901091	0.0=0.676

xWheat %>% 
  mutate(`Area Irrigated for 1 ha`=`Irrigation hours`/`Area irrigated`) %>% 
  group_by(district) %>% 
  summarise(`Area irrigated`=mean(`Area irrigated`),
            `Area Irrigated for 1 ha`=mean(`Area Irrigated for 1 ha`)) %>% 
  kable() %>% kable_styling()

xfishfarm <- 
water01 %>%
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001","T302603034","T309708020","T300901113")) %>%
  filter(crop %in% c("Fish Farming")) %>% 
  select(HH,date,crop,`Total Area Irrigated`,Seasons,Hours,district) %>% 
  mutate(`Total Area Irrigated`=`Total Area Irrigated`*0.0339) %>% 
  group_by(district,HH) %>% 
  summarise(`Area irrigated`=sum(`Total Area Irrigated`,na.rm = T),
            `Irrigation hours`=sum(Hours)) 

fix(xfishfarm)# T309900000 0.0=40=1.3519  # T200103002 0.0=28.5=0.963

x <- xfishfarm %>% 
  mutate(`Area Irrigated for 1 ha`=`Irrigation hours`/`Area irrigated`) %>% 
  group_by(district) %>% 
  summarise(`Area irrigated`=mean(`Area irrigated`),
            `Area Irrigated for 1 ha`=mean(`Area Irrigated for 1 ha`)) %>% 
  kable() %>% kable_styling()





# Irrigated land  - monitoring and survey ---- 
water01_SEASONs$Seasons

# monitoring----
xArea_Irrigated <- 
  water01_SEASONs %>% 
  filter(!Seasons %in% c( "Summer 2016-2017","Monsoon 2015-2016" )) %>% 
         filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>%
  select(district,HH,crop,Seasons,`Total Area Cultivated`) %>% 
  group_by(district,HH,crop,Seasons) %>%
  summarise(`Total Area Cultivated`=max(`Total Area Cultivated`,na.rm = T)) %>% 
  group_by(district,HH,Seasons) %>% 
  summarise(Irrigated=sum(`Total Area Cultivated`)*0.0339) %>% 
  group_by(district,Seasons) %>% 
  summarise(Irrigated=mean(Irrigated))

# orgnaize "T102007001","T300406089", "T305001120"
# survey    ----
xland_Saptari17 <-
  land_17_18_19 %>% filter(TC==1,!season=="Annual",year ==2017) %>%
  select(district,household_questionnaire_id,land_for_cultivation) %>% 
  distinct() %>% 
  rename(`Net Cropped Area`=land_for_cultivation,
         HH=household_questionnaire_id)

xland_Rautahat_Bara_Sarlahi18 <-
  Land_18_19 %>% filter(TC==1,!season=="Annual",year ==2018) %>%
  select(district,household_questionnaire_id,land_for_cultivation) %>% 
  distinct() %>%
  rename(`Net Cropped Area`=land_for_cultivation,
         HH=household_questionnaire_id) %>% 
  mutate(district = "Rautahat_Bara_Sarlahi")


# Irrigated Area monitoring + Irrigated Area survey + land for cultivation (survey) IN HECTAR ----

# land baseline (xlands_IHH)
#  Irrigated Area survey (xnet_area_ha1)
xnet_area_ha1 <- xagri_su %>%
  inner_join(xlands_IHH) %>% 
  filter(!HH %in% c("A104507035","E0104705010","A0110402001")) %>% #remove the pilot HH
  group_by(district,HH,Seasons) %>% 
  summarise(`Area Irrigated (survey)`=sum(`Area Irrigated (survey)`)) %>% 
  group_by(district,Seasons) %>% 
  summarise(`Area Irrigated (survey)`=mean(`Area Irrigated (survey)`) ) 

# Irrigated Area monitoring (xnet_area_ha2)
xnet_area_ha2 <- 
  water01_SEASONs %>%
  inner_join(xlands_IHH) %>%
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>% 
  select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017")) %>% 
  group_by(district,HH,Seasons,crop) %>% 
  summarise(`Area Irrigated (monitoring)`=max(`Total Area Cultivated`,na.rm = T)*0.0339) %>% 
  summarise(`Area Irrigated (monitoring)`=sum(`Area Irrigated (monitoring)`)) %>% 
  group_by(district,Seasons) %>% 
  summarise(`Area Irrigated (monitoring)`=mean(`Area Irrigated (monitoring)`)) 
  
# bind (xnet_area_ha)
xnet_area_ha <- right_join(xnet_area_ha1,xnet_area_ha2) %>% 
  filter(Seasons != "Summer 2017-2018" | district != "Rautahat_Bara_Sarlahi")

xnet_area_ha <- gather(xnet_area_ha,"Dataset", "Area size (in ha)",3:4) %>% 
  bind_rows(xlands_I)

# plot
#
ggplot(data = xnet_area_ha,aes(x=Seasons)) +
  geom_bar(aes(y=`Area size (in ha)`,fill=Dataset),
           stat="identity",
           position="dodge",
           alpha=.7) +
  labs(x = " ")+
  facet_grid(. ~ district) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits = c("Total land", "Monsoon 2017-2018", "Winter 2017-2018",
                              "Summer 2017-2018","Annual 2017-2018",
                              "Monsoon 2018-2019", "Winter 2018-2019",
                              "Summer 2018-2019","Annual 2018-2019"))+
  scale_fill_manual(values=c("#468189", "#9DBEBB","cyan4"))


# AC Irrigated Area monitoring + Irrigated Area survey + land for cultivation (survey) IN HECTAR ----

# HH from survey - dont have all seasons
filter(!HH %in% c("T302806050","T304802122","T304802123",
                  "T102007001","T104209001","T109205004","T210709003")) 
# HH from monitoring - dont have all seasons
filter(!HH %in% c("T302603034","T100503002","T210701004"))
# HH pilot
filter(!HH %in% c("A104507035","E0104705010","A0110402001")) %>% 
  
  
  
  #  Annual incuded in each season (AC_base_lands)
  # total land data (survey)
  AC_base_lands <- xlands_IHH %>% 
  filter(!HH %in% c("A104507035","E0104705010","A0110402001")) %>% 
  group_by(district) %>% 
  summarise(`Area size (in ha)`=mean(total_land)) %>% 
  mutate( data="Total land for \ncultivation (survey)",
          Seasons= "Total land") %>% 
  select(district,Seasons,data,`Area size (in ha)`)

filter(!HH %in% c("T304802123", "T300406135","T303007001", "T300901091", "T301911010") )

# AC Irrigated Area survey (AC_area_s) 
AC_area_s <- xagri_su %>%
  inner_join(xlands_IHH) %>% 
  group_by(district,HH,Seasons) %>% 
  summarise(`Area Irrigated (survey)`=sum(`Area Irrigated (survey)`))

AC_area_s <- spread(AC_area_s,"Seasons","Area Irrigated (survey)")

AC_area_s [is.na(AC_area_s)] <- 0 

AC_area_s <- AC_area_s %>% 
  mutate(`Summer 2017-2018`=`Summer 2017-2018`+`Annual 2017-2018`,
         `Monsoon 2017-2018`=`Monsoon 2017-2018`+`Annual 2017-2018`,
         `Winter 2017-2018`=`Winter 2017-2018`+`Annual 2017-2018`,
         
         `Summer 2018-2019`=`Summer 2018-2019`+`Annual 2018-2019`,
         `Monsoon 2018-2019`=`Monsoon 2018-2019`+`Annual 2018-2019`,
         `Winter 2018-2019`=`Winter 2018-2019`+`Annual 2018-2019`,
         data="survey") %>% 
  select(-c("Annual 2017-2018","Annual 2018-2019"))

AC_area_s [AC_area_s == 0 ] <- NA


# AC Irrigated Area monitoring (AC_area_m)
AC_area_m <- 
  water01_SEASONs %>%
  inner_join(xlands_IHH) %>%
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>% 
  select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017")) %>% 
  group_by(district,HH,Seasons,crop) %>% 
  summarise(`Area Irrigated (monitoring)`=max(`Total Area Cultivated`,na.rm = T)*0.0339) %>% 
  summarise(`Area Irrigated (monitoring)`=sum(`Area Irrigated (monitoring)`))

AC_area_m <- spread(AC_area_m,"Seasons","Area Irrigated (monitoring)")

AC_area_m [is.na(AC_area_m)] <- 0 

AC_area_m  <- AC_area_m %>% 
  mutate(`Summer 2017-2018`=`Summer 2017-2018`+`Annual 2017-2018`,
         `Monsoon 2017-2018`=`Monsoon 2017-2018`+`Annual 2017-2018`,
         `Winter 2017-2018`=`Winter 2017-2018`+`Annual 2017-2018`,
         
         `Summer 2018-2019`=`Summer 2018-2019`+`Annual 2018-2019`,
         `Monsoon 2018-2019`=`Monsoon 2018-2019`+`Annual 2018-2019`,
         `Winter 2018-2019`=`Winter 2018-2019`+`Annual 2018-2019`,
         data="monitoring"
  ) %>% 
  select(-c("Annual 2017-2018","Annual 2018-2019","Annual 2019-2020",
            "Monsoon 2019-2020" , "Winter 2019-2020"))
AC_area_m [AC_area_m == 0 ] <- NA

# bind (AC_area)
AC_area <- bind_rows(AC_area_s,AC_area_m) %>%
  filter(!HH %in% c("T302806050","T304802122","T304802123",
                    "T102007001","T104209001","T109205004","T210709003")) %>% 
  filter(!HH %in% c("T302603034","T100503002","T210701004"))

AC_area <- AC_area %>% 
  group_by(district,data) %>% 
  summarise_at(c("Monsoon 2017-2018","Monsoon 2018-2019","Summer 2017-2018",
                 "Summer 2018-2019","Winter 2017-2018","Winter 2018-2019"),
               mean, na.rm = TRUE)  

#replace NaN to NA
fix(AC_area)

AC_area <- gather(AC_area,"Seasons", "Area size (in ha)",3:8) 

AC_area <- AC_area %>% bind_rows(AC_base_lands)



# plot---
#
ggplot(data = AC_area,aes(x=Seasons)) +
  geom_bar(aes(y=`Area size (in ha)`,fill=data),
           stat="identity",
           position="dodge",
           alpha=.7) +
  labs(x = " ")+
  facet_grid(. ~ district) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits = c("Total land", "Monsoon 2017-2018", "Winter 2017-2018",
                              "Summer 2017-2018",
                              "Monsoon 2018-2019", "Winter 2018-2019",
                              "Summer 2018-2019"))+
  scale_fill_manual(values=c("#468189", "#9DBEBB","cyan4"))


# Irrigated Area as % from the land for cultivation ----
xnet_area <- rbind(xland_Saptari17,xland_Rautahat_Bara_Sarlahi18) %>%
  filter(!HH %in% c("A104507035","E0104705010","A0110402001")) %>% #remove the pilot HH
  inner_join(xArea_Irrigated) %>% 
  inner_join(growing_season) %>% 
  mutate(per=Irrigated/`Net Cropped Area`) %>% 
  drop_na() %>% 
  mutate(across(is.numeric,round,2)) %>% 
  mutate(year=case_when(
    Seasons == "Monsoon 2017-2018" | Seasons == "Winter 2017-2018" |
      Seasons == "Summer 2016-2017"  ~ "2017",
    Seasons == "Monsoon 2018-2019" | Seasons == "Winter 2018-2019" |
      Seasons == "Summer 2017-2018"  ~ "2018",
    Seasons == "Monsoon 2019-2020" | Seasons == "Winter 2019-2020" |
      Seasons == "Summer 2018-2019"  ~ "2019",
    )) %>% 
  mutate(growing_year=case_when(
    Seasons == "Monsoon 2017-2018" | Seasons == "Winter 2017-2018" |
      Seasons == "Summer 2017-2018"  ~ "2017-2018",
    Seasons == "Monsoon 2018-2019" | Seasons == "Winter 2018-2019" |
      Seasons == "Summer 2018-2019"  ~ "2018-2019",
    Seasons == "Monsoon 2019-2020" | Seasons == "Winter 2019-2020"  ~ "2019-2020",
  ))

xnet_area  %>%filter(per>0) %>% 
  group_by(district, HH,SEASONs) %>% summarise(per=mean(per)) %>% 
  group_by(district,SEASONs) %>% summarise(mean(per)) 

xnet_area  %>%filter(per>0) %>% 
  group_by(district, HH,growing_year) %>% summarise(per=mean(per)) %>% 
  group_by(district,growing_year) %>% summarise(mean(per)) 

# השוואה של שטח מושקה - סקר / מוניטורינג ----
# בסקר- לשמור רק את התצפיות הרלוונטית להשוואה- היינו - ללא בייסלין וללא 2019-2020
# `Total Area Cultivated` is irrigated land

#by crop   ----

#`xagri_su` -  from Master tab

xagri_mo <- 
  water01_SEASONs %>%
  filter(HH != "T305602003" |Seasons != "Summer 2017-2018") %>%
  filter(!crop %in% c("System testing","System Testing","Barren","Barren Land")) %>% 
  select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
  filter(! Seasons %in%c("Monsoon 2015-2016", "Summer 2016-2017",
                         "Monsoon 2019-2020","Winter 2019-2020",
                         "Annual 2019-2020") )%>% 
  distinct() %>%
  group_by(district,HH,Seasons,crop) %>% 
  summarise(`Area Irrigated (monitoring)`=max(`Total Area Cultivated`)*0.0339)

xagri_mo$crop[xagri_mo$crop=="Summer Paddy"] <- "Paddy"

xagri_su$crop[xagri_su$crop == "PADDY"] <- "Paddy"
xagri_su$crop[xagri_su$crop == "WHEAT"] <- "Wheat"
xagri_su$crop[xagri_su$crop == "ONIONS"] <- "Onion"
xagri_su$crop[xagri_su$crop == "BITTER GOURD"] <- "Bitter Gourd"
xagri_su$crop[xagri_su$crop == "BITTER"] <- "Bitter Gourd"
xagri_su$crop[xagri_su$crop == "BOTTLE GOURD"] <- "Bottle Gourd"
xagri_su$crop[xagri_su$crop == "BOTTLE"] <- "Bottle Gourd"
xagri_su$crop[xagri_su$crop == "BRINJAL"] <- "Brinjal"
xagri_su$crop[xagri_su$crop == "CABBAGE"] <- "Cabbage"
xagri_su$crop[xagri_su$crop == "BRINJAL"] <- "Brinjal"
xagri_su$crop[xagri_su$crop == "CUCUMBER"] <- "Cucumber"
xagri_su$crop[xagri_su$crop == "MUSTARD"] <- "Mustard"
xagri_su$crop[xagri_su$crop == "CHILLIES"] <- "Chilli"
xagri_su$crop[xagri_su$crop == "MAIZE"] <- "Maize"
xagri_su$crop[xagri_su$crop == "RED LENTILS"] <- "Lentil"
xagri_su$crop[xagri_su$crop == "POTATO"] <- "Potato"
xagri_su$crop[xagri_su$crop == "GREEN LEAFY VEGETABLE"] <- "Green Leafy Vegetables"
xagri_su$crop[xagri_su$crop == "POTATO"] <- "Potato"
xagri_su$crop[xagri_su$crop == "BLACK GRAM"] <- "Horse Gram"
xagri_su$crop[xagri_su$crop == "CAULI"] <- "Cauliflower"
xagri_su$crop[xagri_su$crop == "CAULIFLOWER"] <- "Cauliflower"
xagri_su$crop[xagri_su$crop == "CORIANDER"] <- "Coriander"
xagri_su$crop[xagri_su$crop == "GARLIC"] <- "Garlic"
xagri_su$crop[xagri_su$crop == "HORSE GRAM"] <- "Horse Gram"
xagri_su$crop[xagri_su$crop == "LINSEED"] <- "Linseed"
xagri_su$crop[xagri_su$crop == "Lady's Finger"] <- "OKRA"
xagri_su$crop[xagri_su$crop == "PUMPKIN"] <- "Pumpkin"
xagri_su$crop[xagri_su$crop == "RADISH"] <- "Radish"
xagri_su$crop[xagri_su$crop == "RED LENTIL"] <- "Lentil"
xagri_su$crop[xagri_su$crop == "SESAME"] <- "Sesame Seeds"
xagri_su$crop[xagri_su$crop == "TOMATO"] <- "Tomato"
xagri_su$crop[xagri_su$crop == "VEGETABLES"] <- "vegetables"
xagri_su$crop[xagri_su$crop == "SUGARCANE"] <- "Sugarcane"

xagri_mo$crop[xagri_mo$crop == "Split Red Lentil"] <- "Lentil"
xagri_mo$crop[xagri_mo$crop == "Grass"] <- "GRASS PEA"
xagri_mo$crop[xagri_mo$crop == "Lady's Finger"] <- "OKRA"
xagri_mo$crop[xagri_mo$crop == "vegetable"] <- "vegetables"


xagri_mo_su <- inner_join(xagri_mo,xagri_su) 
xxleft <-left_join(xagri_mo,xagri_su) 
xxright <- right_join(xagri_mo,xagri_su)
irrigated_plots_monitoringNsurvey <- full_join(xagri_mo,xagri_su)

write.csv(irrigated_plots_monitoringNsurvey, file = "C:/Users/Dan/Documents/R/Saptari/data/irrigated_plots_monitoringNsurvey.csv", row.names=FALSE)


#     plot    ----
ggplot(xagri_mo_su) +
  geom_point(aes(x = `Area Irrigated (monitoring)`,
                 y = `Area Irrigated (survey)`),
             color='#56B4E9')+
    labs(x = "Monitoring Data", y = "Survey Data")+
    theme_light() + xlim(0, 1)+ylim(0, 5)

#by season ----

EN1 <- xagri_mo %>% group_by(district,HH,Seasons) %>% 
  summarise(monitoring=sum(`Area Irrigated (monitoring)`))

EN2 <- xagri_su %>% group_by(district,HH,Seasons) %>% 
  summarise(survey=sum(`Area Irrigated (survey)`))

EN <- inner_join(EN1,EN2)
#     plot    ----
ggplot(xagri_season_mo_su) +
  geom_point(aes(x = monitoring,
                 y = survey),
             color='#56B4E9')+
  labs(x = "Monitoring Data", y = "Survey Data")+
  theme_light() + xlim(0, 3)+ylim(0,9)



  # Irrigation area - over seasons --------------
crop != "Barren Land"

x1 <- water01_SEASONs %>%
  filter(! SEASONs %in% c("Fish Farming","Mango Plant","Sugarcane")) %>% 
  filter(!crop %in% c("System testing","System Testing")) %>% 
  select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
  filter(! Seasons =="Monsoon 2015-2016") %>% 
  distinct() %>%
  group_by(district,HH,Seasons,crop) %>% 
  summarise(`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
  group_by(district,HH,Seasons) %>% 
  summarise(irrigated=sum(`Total Area Cultivated`)) %>% 
  group_by(district,Seasons) %>% 
  summarise(av.irrigated=mean(irrigated)) %>% 
  mutate(`Avg. Irrigated Area`= av.irrigated*0.0339) %>% 
  add_column(Season =c( "Monsoon","Monsoon","Summer","Summer","Winter","Winter",
             "Monsoon","Monsoon","Monsoon","Summer","Summer","Summer","Winter","Winter","Winter"))

  
  ggplot(data = x1,aes(x=Season)) +
  geom_bar(aes(y=`Avg. Irrigated Area`,fill=Seasons),
           stat="identity",
           position="dodge",
           alpha=.7) +
    theme_bw()+
  facet_grid(. ~ district) +
    scale_fill_manual(values=c("#31a354", "#a1d99b", "#00b159",
                               "#31a354", "#a1d99b", "#00b159",
                               "#31a354", "#a1d99b", "#00b159"))

  # Irrigation area - growing_year --------------
x2 <-   water01_SEASONs %>%
    filter(! SEASONs %in% c("Fish Farming","Mango Plant","Sugarcane")) %>% 
    filter(!crop %in% c("System testing","System Testing")) %>% 
    select(district,HH,Seasons,crop,`Total Area Cultivated`) %>%
    filter(! Seasons =="Monsoon 2015-2016") %>% 
    distinct() %>%
    group_by(district,HH,Seasons,crop) %>% 
    summarise(`Total Area Cultivated`=max(`Total Area Cultivated`)) %>% 
    group_by(district,HH,Seasons) %>% 
    summarise(irrigated=sum(`Total Area Cultivated`)) %>% 
  mutate(growing_year=case_when(
    Seasons == "Monsoon 2017-2018" | Seasons == "Winter 2017-2018" |
      Seasons == "Summer 2017-2018"  ~ "2017-2018",
    Seasons == "Monsoon 2018-2019" | Seasons == "Winter 2018-2019" |
      Seasons == "Summer 2018-2019"  ~ "2018-2019",
    Seasons == "Monsoon 2019-2020" | Seasons == "Winter 2019-2020"  ~ "2019-2020",
  )) %>%
    filter(growing_year !="2019-2020") %>% 
    group_by(district,HH,growing_year) %>% 
    summarise(Avg=mean(irrigated)) %>% 
    group_by(district,growing_year) %>% 
    summarise(Avg=mean(Avg))
    
    
    
    
    
    

  
#  Fuel use - 7.16 ----
  
  library(MatchIt)
  
  psf <-
    Procurement_17_18_19 %>%
    select(year,TreatmentControl,household_questionnaire_id,total_litres_consumed_dieselkero) %>% 
    filter(!is.na(total_litres_consumed_dieselkero)) %>%rename(Group=TreatmentControl)
  
  lso <- psf %>%  mutate(own_sp = Group == "Treatment")
  
  lso_M <- filter(lso, year == 2017)
  match.it <- matchit(own_sp ~ total_litres_consumed_dieselkero , data = lso_M, method="nearest", ratio=1)
  df.match <- match.data(match.it)[3]
  df.match <-
    inner_join (df.match,lso,by="household_questionnaire_id") %>% 
    select(year,Group,household_questionnaire_id,total_litres_consumed_dieselkero)%>%
    filter(Group=="Control")
  
  df <- psf %>% filter(Group=="Treatment") %>% bind_rows(df.match) 

g1 <- 
  ggplot(df, aes(year, total_litres_consumed_dieselkero, color = Group)) +
    stat_summary(geom = 'line',size=1.1) +
    theme_minimal()+
    labs(title ="Saptari", x=" ", y="Total fuel consumed") +
    scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "black", hjust = 0))

-------------------------------------------------------------------------------------------
  
  rpsf <- Procurement_18_19 %>%
    select(year,TreatmentControl,household_questionnaire_id,total_litres_consumed_dieselkero) %>% 
    filter(!is.na(total_litres_consumed_dieselkero),
           total_litres_consumed_dieselkero<10000) %>%
    mutate(across(is.numeric, round, 2))%>%rename(Group=TreatmentControl)
  
  lso <-rpsf %>% mutate(own_sp = Group == "Treatment") 
  
  lso_M <- filter(lso, year == 2018)
  match.it <- matchit(own_sp ~ total_litres_consumed_dieselkero , data = lso_M, method="nearest", ratio=1)
  df.match <- match.data(match.it)[3]
  df.match <-
    inner_join (df.match,lso,by="household_questionnaire_id") %>% 
    select(year,Group,household_questionnaire_id,total_litres_consumed_dieselkero)%>%
    filter(Group=="Control")

  df2 <- rpsf %>% filter(Group=="Treatment") %>% bind_rows(df.match) 
  
g2 <-   
  ggplot(df2, aes(year, total_litres_consumed_dieselkero, color = Group)) +
    stat_summary(geom = 'line',size=1.1) +
    theme_minimal()+
    labs(title = "Rautahat Bara Sarlahi",x=" ", y=" ") +
    scale_x_continuous(breaks = c(2018,2019))+
  theme(
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "black", hjust = 0))

----------------------------------------------------------------------------
df  <-  df %>% mutate(District ="Saptari")

  
grid.arrange(g1,g2,ncol=2)

# Cropping Pattern----
library(MatchIt)
#Vegetables----
A17 <- Agriculture_17_18_19 %>% filter(year==2017,name_of_crop=="Vegetables") %>%
  select(1) %>% distinct()
A18 <- Agriculture_17_18_19 %>% filter(year==2018,name_of_crop=="Vegetables") %>%
  select(1) %>% distinct() %>% inner_join(A17)
A19 <- Agriculture_17_18_19 %>% filter(year==2019,name_of_crop=="Vegetables") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_17_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Vegetables") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)
df <- match.data(match.it)[3] %>% inner_join (veg)

gg <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Vegetables - Saptari") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

  df %>% 
  group_by(year,Group) %>%
  summarise(`Avg. area size`= mean(cult_area)) %>%
  mutate_at (3,round,2) %>% kable() %>% kable_paper()

grid.arrange(gg,gt,ncol=2)
#Oilseeds----
#Saptari
A17 <- Agriculture_17_18_19 %>% filter(year==2017,name_of_crop=="Oilseeds") %>%
  select(1) %>% distinct()
A18 <- Agriculture_17_18_19 %>% filter(year==2018,name_of_crop=="Oilseeds") %>%
  select(1) %>% distinct() %>% inner_join(A17)
A19 <- Agriculture_17_18_19 %>% filter(year==2019,name_of_crop=="Oilseeds") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_17_18_19 %>% right_join(A19) %>% 
  filter(name_of_crop=="Oilseeds") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

gg2 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Oilseeds - Saptari") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

grid.arrange(gg,gg2,ncol=2)

#paddy ----
#Saptari
A17 <- Agriculture_17_18_19 %>% filter(year==2017,name_of_crop=="Paddy") %>%
  select(1) %>% distinct()
A18 <- Agriculture_17_18_19 %>% filter(year==2018,name_of_crop=="Paddy") %>%
  select(1) %>% distinct() %>% inner_join(A17)
A19 <- Agriculture_17_18_19 %>% filter(year==2019,name_of_crop=="Paddy") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_17_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Paddy") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g1 <- 
ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Paddy - Saptari") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

# Rautahat, Bara & Sarlahi Agriculture_18_19
A18 <- Agriculture_18_19 %>% filter(year==2018,name_of_crop=="Paddy") %>%
  select(1) %>% distinct()
A19 <- Agriculture_18_19 %>% filter(year==2019,name_of_crop=="Paddy") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Paddy") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2018)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g2 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Paddy - Rautahat, Bara & Sarlahi") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

grid.arrange(g1,g2,ncol=2)

#Wheat----
#Saptari
A17 <- Agriculture_17_18_19 %>% filter(year==2017,name_of_crop=="Wheat") %>%
  select(1) %>% distinct()
A18 <- Agriculture_17_18_19 %>% filter(year==2018,name_of_crop=="Wheat") %>%
  select(1) %>% distinct() %>% inner_join(A17)
A19 <- Agriculture_17_18_19 %>% filter(year==2019,name_of_crop=="Wheat") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_17_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Wheat") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g1 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Wheat - Saptari") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

# Rautahat, Bara & Sarlahi Agriculture_18_19
A18 <- Agriculture_18_19 %>% filter(year==2018,name_of_crop=="Wheat") %>%
  select(1) %>% distinct()
A19 <- Agriculture_18_19 %>% filter(year==2019,name_of_crop=="Wheat") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Wheat") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2018)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g2 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Wheat - Rautahat, Bara & Sarlahi") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

grid.arrange(g1,g2,ncol=2)


#Pulses----
#Saptari
A17 <- Agriculture_17_18_19 %>% filter(year==2017,name_of_crop=="Pulses") %>%
  select(1) %>% distinct()
A18 <- Agriculture_17_18_19 %>% filter(year==2018,name_of_crop=="Pulses") %>%
  select(1) %>% distinct() %>% inner_join(A17)
A19 <- Agriculture_17_18_19 %>% filter(year==2019,name_of_crop=="Pulses") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_17_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Pulses") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g1 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Pulses - Saptari") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

# Rautahat, Bara & Sarlahi Agriculture_18_19
A18 <- Agriculture_18_19 %>% filter(year==2018,name_of_crop=="Pulses") %>%
  select(1) %>% distinct()
A19 <- Agriculture_18_19 %>% filter(year==2019,name_of_crop=="Pulses") %>%
  select(1) %>%distinct() %>%  inner_join(A18)

veg <- Agriculture_18_19 %>%right_join(A19) %>% 
  filter(name_of_crop=="Pulses") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  rename(Group=TreatmentControl) %>% 
  mutate(own_sp = Group == "Treatment")

lso_M <- filter(veg, year == 2018)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)

summary(match.it)

df <- match.data(match.it)[3] %>% inner_join (veg)

g2 <- 
  ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="Cultivated area size (ha)",title = "Pulses - Rautahat, Bara & Sarlahi") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        text = element_text(family = "Times New Roman"))

df %>% group_by(year,Group) %>% summarise(mean(cult_area)) %>% kable() %>% kable_paper()

grid.arrange(g1,g2,ncol=2)



# fish  ----
#monitoring data
wat <- 
  water01_SEASONs %>%
  filter( Seasons!="Monsoon 2015-2016") %>% 
  filter(!HH %in% c("T210701004","T109902002","E0104705010","A0110402001")) %>%
  filter(crops_category=="Fish Farming") %>% 
  group_by(HH,district,Seasons) %>%
  summarise(Area=max(`Total Area Cultivated`))

wat$Seasons[wat$Seasons %in% c("Monsoon 2017-2018","Summer 2017-2018") ] <- "Annual 2017-2018"
wat$Seasons[wat$Seasons %in% c("Monsoon 2018-2019","Summer 2018-2019") ] <- "Annual 2018-2019"
wat$Seasons[wat$Seasons %in% c("Monsoon 2019-2020","Winter 2019-2020") ] <- "Annual 2019-2020"

wat <- wat %>%
  drop_na() %>% 
  group_by(district,Seasons) %>% 
  summarise(mean(Area))

#survey data
aquaculture2.0 <- aquaculture %>%
  mutate(district=case_when(
    district %in% c(1,2,3)~"Rautahat_Bara_Sarlahi",
    district %in% c("Saptari","SAPTARI")~"Saptari")) %>% 
  filter(TC==1,total_area_of_pond<300 ) %>% 
  group_by(district,year) %>% 
  summarise(Mean=mean(total_area_of_pond)*0.0339)
  
d1 <- aquaculture2.0 %>% 
  filter(district=="Saptari") %>% 
  ggplot( aes(year, Mean)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(title="Sapteri",x=" ", y="Pond size (ha)") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",text = element_text(family = "Times New Roman"))
d3 <- aquaculture2.0 %>% 
  filter(district!="Saptari") %>% 
  ggplot( aes(year, Mean)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(title="Rautahat, Bara & Sarlahi",x=" ", y=" ") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(legend.position = "none",text = element_text(family = "Times New Roman"))
grid.arrange(d1, d3, ncol = 2) 
# croping pattern YES/NO ----
Ag <- 
  Agriculture_18_19 %>% 
  filter(TC==1) %>% 
  select(household_questionnaire_id ,name_of_crop, year) %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(name_of_crop)
