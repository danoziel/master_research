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
  

GHIII <- GHI %>%
  mutate(
  Panel_Wp=ifelse(district == "Saptari",1.2,
                  ifelse(district == "Rautahat_Bara_Sarlahi",1.3,NA)))%>% 
  mutate(kWh=Panel_Wp*ALLSKY_SFC_SW_DWN ) %>% 
  mutate(
    Pump_Kw=ifelse(district == "Saptari",1.1,
                    ifelse(district == "Rautahat_Bara_Sarlahi",0.75,NA))) %>% 
  mutate(operate_spip=ifelse(Pump_Kw<kWh,1,0))

GHIII %>%drop_na(ALLSKY_SFC_SW_DWN) %>%  group_by(district) %>%
  count(operate_spip) %>% mutate(freq = n / sum(n))
  
ghi_rain <- HH_pct_N_rain[,c(1,3:5)] %>% inner_join(GHIII) %>% 
  mutate_at(3,round, 2)


names(GHI)




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
# rainy days and pct usage  HH_pct_N_rain -----
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

HH_pct_N_rain <-HH_pct_N_rain %>% mutate(after=rainCat)
HH_pct_N_rain[which(HH_pct_N_rain$after == "Rainy_Day")+1, "after"] <- "Rainy_Day"

n <-HH_pct_N_rain %>%
  mutate(DA=ifelse(rainCat=="Rainy_Day"& after == "Rainy_Day", NA,
                   after))

# count(rain_day) %>% mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))
# plot ----
HH_pct_N_rain %>%
  mutate(saptari_pct=saptari_pct*100,rbs_pct=rbs_pct*100) %>% 
  gather( "key", "value",-c(date,season,year)) %>% 
  ggplot(aes(x=date, y=value, group=key)) +
  geom_line(aes(color=key))



# correlation ----

saptari_mm <- HH_pct_N_rain %>% filter(district=="Saptari")

rbs_mm <- HH_pct_N_rain %>% filter(district=="Rautahat_Bara_Sarlahi") #%>% select(4,5) %>% drop_na()
tab_corr(rbs_mm)

# regression ----

HH_pct_N_rain$seasonCat <- as.factor(HH_pct_N_rain$seasonCat)
HH_pct_N_rain$seasonCat <- relevel(HH_pct_N_rain$seasonCat, ref = "Winter")

HH_pct_N_rain$rainIntensity <- as.factor(HH_pct_N_rain$rainIntensity)
HH_pct_N_rain$rainIntensity <- relevel(HH_pct_N_rain$rainIntensity,ref="light_rain")

HH_pct_N_rain$season <- as.factor(HH_pct_N_rain$season)
HH_pct_N_rain$season <- relevel(HH_pct_N_rain$season,ref="Winter_2018_2019")

c("season","district","pct","rain_mm","year","seasonCat",
  "Winter","Summer","Monsoon","rainCat","rainIntensity")

# lm + pearson correlation
#rain_mm
modSAP <- lm(pct ~ rain_mm , data = saptari_mm)
modRBS <- lm(pct ~ rain_mm , data = rbs_mm)
tab_model(modSAP,modRBS,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "Rain (mm)"))

#rainCat
modSAP <- lm(pct ~ rainCat, data = saptari_mm)
modRBS <- lm(pct ~ rainCat, data = rbs_mm)

modSAP <- lm(pct ~ after, data = saptari_mm)
modRBS <- lm(pct ~ after, data = rbs_mm)

modSAP <- lm(pct ~ rainCat+ after, data = saptari_mm)
modRBS <- lm(pct ~ rainCat+ after, data = rbs_mm)
tab_model(modSAP,modRBS,digits=3,p.style= "numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "Rainy day","Rainy day + day after"))

#rainIntensity
modSAP <- lm(pct ~ rainIntensity, data = saptari_mm)
modRBS <- lm(pct ~ rainIntensity, data = rbs_mm)
tab_model(modSAP,modRBS,digits=3,p.style="numeric",show.se = TRUE,string.ci = "Conf. Int (95%)",
          dv.labels = c("Saptari", "Rautahat Bara Sarlahi"),
          pred.labels = c("(Intercept)", "2.5mm -10mm","10mm -50mm","50mm +"))


#season
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



summary(modSAP)




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
