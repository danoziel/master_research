# Solar Resource Parameters
# https://www.star.nesdis.noaa.gov/smcd/emb/radiation/solar_resource_definitions.php

library(nasapower)
library(sjPlot)

# Parameter(s): -
#  ALLSKY_SFC_SW_DWN - All Sky Insolation Incident on a Horizontal Surface (kW-hr/m^2/day) 
#  PRECTOT MERRA2 - Precipitation (mm day-1) 
#  KT - Insolation Clearness Index (dimensionless) 
#  CLRSKY_SFC_SW_DWN - Clear Sky Insolation Incident on a Horizontal Surface (kW-hr/m^2/day) 

#CLRSKY_SFC_SW_DWN  (GHI_saptari , GHI_rbs)     ----
GHI_saptari <- get_power(
  community = "SSE",
  lonlat = c(86.63533, 26.60646),
  pars = c("CLRSKY_SFC_SW_DWN","ALLSKY_SFC_SW_DWN","KT"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Saptari") %>% 
  rename(date=YYYYMMDD)

  
GHI_rbs <- get_power(
  community = "SSE",
  lonlat = c(85.16984, 27.00147),
  pars = c("CLRSKY_SFC_SW_DWN","ALLSKY_SFC_SW_DWN","KT"),
  dates =c( "2018-06-21","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Rautahat_Bara_Sarlahi") %>% 
  rename(date=YYYYMMDD)

GHI <- rbind(GHI_saptari[,7:11],GHI_rbs[,7:11]) 
  

# solar_potential----
solar_potential <- GHI %>%
  mutate(Panel_Wp=ifelse(district == "Saptari",1.2,
                         ifelse(district == "Rautahat_Bara_Sarlahi",1.3,NA)),
         kWh_day=Panel_Wp*ALLSKY_SFC_SW_DWN,
         Pump_Kw=ifelse(district == "Saptari",1.1,
                        ifelse(district == "Rautahat_Bara_Sarlahi",0.75,NA)),
         avg_duration_use=ifelse(district == "Saptari",6.44,
                                 ifelse(district == "Rautahat_Bara_Sarlahi",5.18,NA)),
         avg_sunshine_duration=ifelse(district == "Saptari",6.98,
                            ifelse(district == "Rautahat_Bara_Sarlahi",6.27,NA))) %>% 
  inner_join(HH_pct_N_rain)

solar_potential %>%drop_na(ALLSKY_SFC_SW_DWN) %>%  
  mutate(PotentialDay=ifelse(ALLSKY_SFC_SW_DWN>3.4,1,0)) %>% 
#  group_by(district) %>%
  count(PotentialDay) %>% mutate(freq = n / sum(n))

# plot----
ggplot(solar_potential, aes(x=ALLSKY_SFC_SW_DWN, y=pct)) + 
  geom_point(size = 1, alpha = .4,color="dodgerblue4")+
  geom_smooth(method=lm, se=FALSE,color="dimgrey")+
  labs(title="Percentage of SIP users & Solar radiation, per day",
       x="Global Horizontal Irradiance", y=" Fraction of Farmers Using SPIP")+
  scale_x_continuous(breaks = seq(0, 8, 1))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia")  )
  

# --------
annual_SR <- HH_pct_N_rain %>%
  filter(!is.na(ALLSKY_SFC_SW_DWN),!is.na(seasonCat))%>%
  summarise(mean(ALLSKY_SFC_SW_DWN,na.rm = TRUE),
            min(ALLSKY_SFC_SW_DWN,na.rm = TRUE),max(ALLSKY_SFC_SW_DWN,na.rm = TRUE),
  ) 

saesonal_SR <- HH_pct_N_rain %>%
  filter(!is.na(ALLSKY_SFC_SW_DWN),!is.na(seasonCat))%>%
  group_by (seasonCat) %>%
  summarise(Mean_sr=mean(ALLSKY_SFC_SW_DWN,na.rm = TRUE),
            min(ALLSKY_SFC_SW_DWN,na.rm = TRUE),max(ALLSKY_SFC_SW_DWN,na.rm = TRUE),
            Mean_pct=mean(pct,na.rm = TRUE)) %>% 
  mutate_at(2:3,round,2) 

sr <- solar_potential %>% filter(!is.na(ALLSKY_SFC_SW_DWN))

sr$irradiance <- as.factor(sr$irradiance)
sr$irradiance<- relevel(sr$irradiance, ref = "low")

# addition 10/2023 ----
HH_pct_N_rain <- read.csv("~/master_research/DATAs/data_master/data_saptari/HH_pct_N_rain.csv")

sr <- HH_pct_N_rain  %>% filter(!is.na(ALLSKY_SFC_SW_DWN))

AA=lm(pct ~ ALLSKY_SFC_SW_DWN+ rain_day_1mm,sr)

library(sjPlot)
tab_model(AA,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c(" "),pred.labels = c("(Intercept)", "GHI [Solar Radiation]","Rainy Day [>1 mm]"))
#-----
modSR <- lm(pct ~ ALLSKY_SFC_SW_DWN,sr)

tab_model(modSR,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c(" "),
          pred.labels = c("(Intercept)", "Solar Radiation (GHI)"))

modSR <- lm(pct ~ ALLSKY_SFC_SW_DWN+ rainCat,sr)
tab_model(modSR,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c(" "),
          pred.labels = c("(Intercept)", "Solar Radiation (GHI)","Rainy Day"))

modSR <- lm(pct ~ ALLSKY_SFC_SW_DWN+seasonCat,sr)
tab_model(modSR,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c(" "),
          pred.labels = c("(Intercept)", "Solar Radiation (GHI)","Monsoon [1]","Summer [1]"))



summary(modSR)


#rainy days in mm  (rain_saptari , rain_rbs)     ----
rain_saptari <- get_power(
  community = "SSE",
  lonlat = c(86.63533, 26.60646),
  pars = c("PRECTOT"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY") %>% 
  select(7,8)

rain_rbs <- get_power(
  community = "SSE",
  lonlat = c(85.16984, 27.00147),
  pars = c("PRECTOT"),
  dates =c( "2018-06-21","2019-12-16"),
  temporal_average = "DAILY")%>% 
  select(7,8)

HH_rain_4 <-
  rain_rbs %>%
  right_join(rain_saptari,by="YYYYMMDD") %>% 
  rename(date=YYYYMMDD,rbs_mm=PRECTOT.x,saptari_mm=PRECTOT.y ) %>% 
  rename(Rautahat_Bara_Sarlahi=rbs_mm,Saptari=saptari_mm ) %>% 
  gather( "district", "rain_mm",-date) 
rm(rain_saptari,rain_rbs)

#rainy days in mm  PLOT        ----

  rain_4 %>%
  ggplot(aes(x=date, y=`rain_mm`, group=district)) +
  labs(x=" ")+
  geom_line(aes(color=district),linetype = "twodash")+
  scale_color_manual(values=c('black','darkolivegreen3'))+
  theme_light()+
  theme(legend.position="bottom")
  
class(rain_4$date)
# rainy days and pct usage DATASET HH_pct_N_rain -----
HH_pct_N_rain <- 
  A_days_HH_per_saptari[,c(1,22,25)] %>%
  rename(Saptari=per) %>% 
  inner_join(A_days_HH_per_RBS[,c(1,32,35)]) %>% 
  rename(Rautahat_Bara_Sarlahi=per) %>% 
  gather( "district", "pct",-c(date,season)) %>% 
  inner_join(HH_rain_4,by = c("date", "district")) %>% 
  mutate(year=format(date, format="%Y"))
rm(A_days_HH_per_saptari,A_days_HH_per_RBS)

# saptari_rain = dummy varible
# saptari_YN  = binary varible
HH_pct_N_rain <-HH_pct_N_rain %>%
  mutate(rainCat = ifelse(rain_mm <7.5, "0",
                               ifelse(rain_mm > 7.5,"Rainy_Day",NA))) 

HH_pct_N_rain <-HH_pct_N_rain %>%
  mutate(rainIntensity = ifelse(rain_mm <2.5, "light_rain",
                            ifelse(rain_mm > 2.5 & rain_mm < 10, "moderate_rain",
                                   ifelse(rain_mm > 10 & rain_mm < 50, "heavy_rain",
                                   "violent_rain"))))

# seasons = dummy varible

HH_pct_N_rain <-HH_pct_N_rain %>%
  mutate(seasonCat = ifelse(season %in% c("Summer_2017_2018","Summer_2018_2019"), "Summer",
                            ifelse(season %in% c("Winter_2017_2018","Winter_2018_2019","Winter_2019_2020"),"Winter","Monsoon")))
HH_pct_N_rain <- HH_pct_N_rain %>% 
  mutate(Winter=ifelse(seasonCat == "Winter","1","0")) %>% 
  mutate(Summer=ifelse(seasonCat == "Summer","1","0")) %>% 
  mutate(Monsoon=ifelse(seasonCat == "Monsoon","1","0")) 

HH_pct_N_rain <-HH_pct_N_rain %>% filter(!is.na(rain_mm))

HH_pct_N_rain <- HH_pct_N_rain %>% 
  mutate(rain_day_1mm=ifelse(rain_mm>=1,1,0))

n <-HH_pct_N_rain %>%
  mutate(DA=ifelse(rainCat=="Rainy_Day"& after == "Rainy_Day", NA,
                   after))

saptari_mm <- HH_pct_N_rain %>% filter(district=="Saptari",seasonCat == "Monsoon") %>% drop_na(pct)
rbs_mm <- HH_pct_N_rain %>% filter(district=="Rautahat_Bara_Sarlahi",seasonCat == "Monsoon")  %>% drop_na(pct)

# plot ----
HH_pct_N_rain %>%
  mutate(saptari_pct=saptari_pct*100,rbs_pct=rbs_pct*100) %>% 
  gather( "key", "value",-c(date,season,year)) %>% 
  ggplot(aes(x=date, y=value, group=key)) +
  geom_line(aes(color=key))



# correlation ----
cor(saptari_mm$pct,saptari_mm$rain_mm)
cor(rbs_mm$pct,rbs_mm$rain_mm)

# regression ----

HH_pct_N_rain$seasonCat <- as.factor(HH_pct_N_rain$seasonCat)
HH_pct_N_rain$seasonCat <- relevel(HH_pct_N_rain$seasonCat, ref = "Winter")

HH_pct_N_rain$rainIntensity <- as.factor(HH_pct_N_rain$rainIntensity)
HH_pct_N_rain$rainIntensity <- relevel(HH_pct_N_rain$rainIntensity,ref="light_rain")

HH_pct_N_rain$season <- as.factor(HH_pct_N_rain$season)
HH_pct_N_rain$season <- relevel(HH_pct_N_rain$season,ref="Winter_2018_2019")

c("season","district","pct","rain_mm","year","seasonCat",
  "Winter","Summer","Monsoon","rainCat","rainIntensity")

#rain_mm  - figure 6.1 ----
modSAP <- lm(pct ~ rainIntensity , data = saptari_mm)
modRBS <- lm(pct ~ rainIntensity , data = rbs_mm)
tab_model(modSAP,modRBS,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))
          pred.labels = c("(Intercept)", "Rain Intensity"))

#scatterplot
HH_pct_N_rain %>% rename(District=district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                           "Rautahat\nBara Sarlahi", District)) %>% 
  ggplot( aes(x = rain_mm, y=pct, color=District)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ 
  labs(x="Rain per Day (mm)", y=" Fraction of Farmers Using the SPIP")+
  xlim(0, 40)+
  theme_minimal() +  
  scale_color_manual(values=c("lightsalmon4", "darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia")  )

# reg only for rainy days
saptari_mm_only <- saptari_mm %>% filter(rain_mm>10)
rbs_mm_only <- rbs_mm %>% filter(rain_mm>10)

modSAP <- lm(pct ~ rain_mm , data = saptari_mm_only)
modRBS <- lm(pct ~ rain_mm , data = rbs_mm_only)
tab_model(modSAP,modRBS,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "Rain (mm)"))

#rainCat ----
# rainy day>1mm ----

modSAP <- lm(pct~rain_day_1mm,saptari_mm)
modRBS <-  lm(pct~rain_day_1mm,rbs_mm)

tab_model(modSAP,modRBS,digits=4,p.style= "numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "Rainy Day (>1 mm)"))

# rainy day>2mm ----

HH_pct_N_rain <- HH_pct_N_rain %>% 
  mutate(rain_day_2mm=ifelse(rain_mm>2,1,0))%>%
  mutate(one_day_ago= rain_day_2mm )

#HH_pct_N_rain$rain_day_2mm <- as.factor(HH_pct_N_rain$rain_day_2mm)

HH_pct_N_rain[which(HH_pct_N_rain$one_day_ago == 1)+1, "one_day_ago"] <- 1

saptari_mm <- HH_pct_N_rain %>% filter(district=="Saptari") %>% drop_na(pct)
rbs_mm <- HH_pct_N_rain %>% filter(district=="Rautahat_Bara_Sarlahi")  %>% drop_na(pct)

modSAP <- lm(pct ~ rain_day_2mm+ one_day_ago ,data = saptari_mm)
modRBS <- lm(pct ~ rain_day_2mm+ one_day_ago , data = rbs_mm)


tab_model(modSAP,modRBS,digits=4,p.style= "numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "Rain Day (2 mm>)","one_day_ago"))

#  dataset - days before  ----
days_before_rain <- HH_pct_N_rain %>%
  mutate(Rainy_Day=ifelse(rainCat=="Rainy_Day",1,0))

days_before_rain$One_day_ago <- days_before_rain$Rainy_Day
days_before_rain[which(days_before_rain$One_day_ago == 1)+1, "One_day_ago"] <- 1

days_before_rain$Two_day_ago <- days_before_rain$One_day_ago
days_before_rain[which(days_before_rain$Two_day_ago == 1)+1, "Two_day_ago"] <- 1

days_before_rain$Three_day_ago <- days_before_rain$Two_day_ago  #3
days_before_rain[which(days_before_rain$Three_day_ago == 1)+1, "Three_day_ago"] <- 1

days_before_rain$Four_day_ago <- days_before_rain$Three_day_ago #4
days_before_rain[which(days_before_rain$Four_day_ago == 1)+1, "Four_day_ago"] <- 1

days_before_rain$Five_day_ago <- days_before_rain$Four_day_ago   #5
days_before_rain[which(days_before_rain$Five_day_ago == 1)+1, "Five_day_ago"] <- 1

days_before_rain$Six_day_ago <- days_before_rain$Five_day_ago    #6
days_before_rain[which(days_before_rain$Six_day_ago == 1)+1, "Six_day_ago"] <- 1

days_before_rain$Week_ago <- days_before_rain$Six_day_ago    #7
days_before_rain[which(days_before_rain$Week_ago == 1)+1, "Week_ago"] <- 1

saptari_mm_days <- subset (days_before_rain,district == "Saptari")
rbs_mm_days <- subset(days_before_rain,district == "Rautahat_Bara_Sarlahi")

#  reg                    ----
modSAP <- lm(pct ~ Rainy_Day+ One_day_ago ,data = saptari_mm_days)
modRBS <- lm(pct ~ Rainy_Day+ One_day_ago , data = rbs_mm_days)

modSAP <- lm(pct ~ Rainy_Day+ One_day_ago+Two_day_ago+ Three_day_ago+ Four_day_ago+
               Five_day_ago+Six_day_ago+ Week_ago,data = saptari_mm_days)
modRBS <- lm(pct ~ Rainy_Day+ One_day_ago+Two_day_ago+ Three_day_ago+ Four_day_ago+
               Five_day_ago+Six_day_ago+ Week_ago, data = rbs_mm_days)

tab_model(modSAP,modRBS,digits=3,p.style= "numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))

#rainIntensity ----
modSAP <- lm(pct ~ rainIntensity, data = saptari_mm)
modRBS <- lm(pct ~ rainIntensity, data = rbs_mm)
tab_model(modSAP,modRBS,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "2.5mm -10mm","10mm -50mm","50mm +"))


#season ----
modSAP <- lm(pct ~ season, data = saptari_mm)
modRBS <- lm(pct ~ season, data = rbs_mm)

#seasonCat
modSAP <- lm(pct ~ seasonCat, data = saptari_mm)
modRBS <- lm(pct ~ seasonCat, data = rbs_mm)

modSAP <- lm(pct ~ Monsoon+Summer, data = saptari_mm)
modRBS <- lm(pct ~ Monsoon+Summer, data = rbs_mm)
tab_model(modSAP,modRBS,digits=5,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))

#          pred.labels = c("(Intercept) \nWinter", "Monsoon","Summer"))

#scatterplot
dfSAP <- saptari_mm %>% rename(District=district) %>% 
  ggplot( aes(x = rain_mm, y=pct, color=seasonCat)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ 
  labs(title = "Saptari", x="Rain per Day (mm)", y=" Fraction of Farmers Using the SPIP")+
  xlim(0, 40)+
  theme_minimal() +  
  scale_color_manual(values=c("dodgerblue4","dimgrey","darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),legend.position = "none")

dfRBS <- rbs_mm %>% rename(District=district,Season=seasonCat) %>% 
  ggplot( aes(x = rain_mm, y=pct, color=Season)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ 
  labs(title="Rautahat Bara Sarlahi",
       x="Rain per Day (mm)", y=" Fraction of Farmers Using the SPIP")+
  xlim(0, 40)+
  theme_minimal() +  
  scale_color_manual(values=c("dodgerblue4","dimgrey","darkolivegreen4"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),legend.position = "none"
        )
600/300
grid.arrange(dfSAP,dfRBS,ncol=2)

#monsoon only ----
# 1.1
saptari_mm_msm <- saptari_mm %>% filter(rainCat == "Rainy_Day")
rbs_mm_msn <- rbs_mm %>% filter(rainCat == "Rainy_Day")

#1.2
saptari_mm_msm <- saptari_mm %>% filter(rain_day_1mm == 1)
rbs_mm_msn <- rbs_mm %>% filter(rain_day_1mm == 1)

# 2
saptari_mm_msm <- saptari_mm %>% filter(seasonCat == "Monsoon") 
rbs_mm_msn <- rbs_mm %>% filter(seasonCat == "Monsoon")

modSAP <- lm(pct ~ rain_mm, data = saptari_mm_msm)
modRBS <- lm(pct ~ rain_mm, data = rbs_mm_msn)
tab_model(modSAP,modRBS,digits=5,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))



# seasons fixed effect ----
names(HH_pct_N_rain)

library(plm)
#
saptari_fixed <- lm(pct ~ rain_mm + factor(season)-1 ,data = saptari_mm)
summary(saptari_fixed)
tab_model(saptari_fixed,digits=3,p.style="stars")

#
rbs_fixed <- lm(pct ~ rain_mm + factor(season)-1 ,data = rbs_mm)
summary(rbs_fixed)
tab_model(rbs_fixed,digits=3,p.style="stars")

##
tab_model(saptari_fixed,rbs_fixed,digits=3,p.style="stars",string.ci = "Conf. Int (95%)", 
          show.se = TRUE,dv.labels = c("Saptari", "Rautahat Bara Sarlahi"))


fixed <- plm(pct ~ rbs_mm,data = HHrbs,index ="season",model = "within")
fixef(fixed)   

pFtest(fixed, ols)

tab_model(model2,digits=3,p.style="stars")

# diary ----

diary_saptari <- get_power(
  community = "SSE",
  lonlat = c(86.63533, 26.60646),
  pars = c("PRECTOT","ALLSKY_SFC_SW_DWN"),
  dates =c( "2017-06-02","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Saptari") %>% 
  rename(date=YYYYMMDD)


diary_rbs <- get_power(
  community = "SSE",
  lonlat = c(85.16984, 27.00147),
  pars = c("PRECTOT","ALLSKY_SFC_SW_DWN"),
  dates =c( "2018-06-21","2019-12-16"),
  temporal_average = "DAILY") %>% 
  add_column(district = "Rautahat_Bara_Sarlahi") %>% 
  rename(date=YYYYMMDD)

diary_4 <- rbind(diary_saptari[,7:10],diary_rbs[,7:10]) 
# diary_4$ALLSKY_SFC_SW_DWN[is.na(diary_4$ALLSKY_SFC_SW_DWN)] <- 0
# ----
diary_spip_terai <-water01_SEASONs %>% inner_join(diary_4) %>% 
  rename(ghi=ALLSKY_SFC_SW_DWN,precip=PRECTOT)

----
ggplot(diary, aes(x=ghi, y=precip,color=precip)) +
  geom_point()+
  theme(text = element_text(family = "Georgia"))+
  theme_minimal()

ggplot(diary, aes(x=ghi, y=precip)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_minimal()
----

ggplot(diary, aes(x=ghi)) + geom_histogram()

ggplot(diary, aes(x=crops_category, y=Hours)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
# rain intencity bar plot ------------------------------------------------------------------

my_sum <- HH_pct_N_rain %>%
  filter(pct>0) %>% 
  mutate(rainIntensity=ifelse(rain_mm == 0, "no_rain",
                              ifelse(rain_mm >0 & rain_mm < 2.5,"0-2.5",
                                     ifelse(rain_mm>2.5 & rain_mm < 10,"2.5-10",
                                            ifelse(rain_mm >10, "10-70",rainIntensity
                                                   ))))) %>% 
  group_by(district,rainIntensity) %>%
  summarise( 
    n=n(),
    mean=mean(pct),
    sd=sd(pct)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# 850/450
my_sum %>% 
  ggplot(aes(x=rainIntensity,  y=mean ,fill=district)) + 
  geom_bar(stat="identity",position="dodge",alpha=.7) +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=.2,position=position_dodge(.9))+
  labs(x="Daily Rain Intensity (in mm)",y = "Fraction of farmers using the SPIP")+ggtitle("Daily rain vs. Fraction of farmers using the SPIP") +
  theme_minimal() +
  theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(size = (20)),axis.text.y = element_text(size = (17)),legend.position="none",
        plot.title = element_text(hjust = 0.5,size = (30)))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks=c(0.05,0.15,0.25,0.35,0.45))+
  scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))+
  scale_x_discrete(limits = c("no_rain", "0-2.5", "2.5-10", "10-70"))


# solar radiation bar plot======
my_solar <- HH_pct_N_rain %>% rename(ghi= ALLSKY_SFC_SW_DWN) %>% 
  filter(pct>0, ghi>0) %>% 
  mutate(GHI=ifelse(ghi>=0 & ghi<1,"0-1",
                    ifelse(ghi>=1 & ghi <2,"1-2",
                           ifelse(ghi>=2 & ghi <3,"2-3",
                                  ifelse(ghi>=3 & ghi< 4,"3-4",
                                         ifelse(ghi>=4 & ghi< 5,"4-5",
                                                ifelse(ghi>=5 & ghi< 6,"5-6",
                                                       ifelse(ghi>=6,"6-7.66",NA)))))))) %>% 
  group_by(district,GHI) %>%
  summarise( 
    n=n(),
    mean=mean(pct),
    sd=sd(pct)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# 850/500
my_solar %>% 
  ggplot(aes(x=GHI,  y=mean ,fill=district)) + 
  geom_bar(stat="identity",position="dodge",alpha=.7) +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=.2,position=position_dodge(.9))+
  labs(x = " ",y=" ")+ggtitle("Solar irradiance vs. Fraction of farmers using the SPIP") +
  theme_minimal() +
  theme( panel.grid.minor = element_blank(),text = element_text(family = "Times New Roman"))+
  theme(axis.text.x = element_text(size = (20)),axis.text.y = element_text(size = (17)),legend.position="none",
        plot.title = element_text(hjust = 0.5,size = (30)))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)
                     , breaks=c(0.1,0.2,0.3, 0.4, 0.5))+
  scale_fill_manual(values=c("lightsalmon4","darkolivegreen4"))

