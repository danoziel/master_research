


############### year_Registration ###
freq(GGRC$year_Registration  , report.nas=T, totals=F, cumul=F, headings=F)

gj1 <- gj11 %>% filter(year_Registration != 2014)

gj1 %>% 
  count(year_Registration) %>% 
  ggplot(aes(x = year_Registration, y = n)) +
  geom_line(color = "green4") +    # Add a line without points
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +  # Ensure all x-axis values appear
  labs(title = "Number of MIS Registrations over the Years", 
       x = "Registrations Year", 
       y = "Number of Registrations") +
  theme_minimal()

### ### ### ### ###

gj1   # A tibble: 441,700 × 26
gj1 %>% filter(!is.na(TotalLand)) # A tibble: 418,835 × 26
gj1 %>% filter(TotalLand<13.88, TotalLand>0.56) # A tibble: 410,303 × 26

gj1 %>% filter(!is.na(TotalLand),
               year_Registration==2005) # A tibble: 129 × 26
gj1 %>% filter(TotalLand<13.88, TotalLand>0.56,
               year_Registration==2005) # A tibble: 64 × 26



# TotalLand group_by(year_Registration) ----
TotalLand_YR= 
  gj1 %>%  
  select(regno,year_Registration,c_code01new,TotalLand,mistype) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(mistype,year_Registration)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  rename(type=mistype)

TotalLand_YR %>% 
  ggplot(aes(x = year_Registration, y = Total_Land, color = type, group = type)) +
  geom_line(size = 1) +  # Add lines for both types
  geom_ribbon(data = filter(TotalLand_YR, type == "Drip"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray90", alpha = 0.5) +  # CI ribbon for "Drip"
  geom_ribbon(data = filter(TotalLand_YR, type == "Sprinkler"), 
              aes(ymin = Total_Land - CI95delta, ymax = Total_Land + CI95delta), 
              fill = "gray95", alpha = 0.5) +  # CI ribbon for "Sprinkler"
 # facet_wrap(~ type, ncol = 1) +  # Separate plots, one per type, arranged vertically
  labs(title = "Total Land Holding over Years by Irrigation Type",
       x = "Year of Registration", 
       y = "Total Land (in Ha)") +
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +
  scale_color_manual(values = c("Drip" = "blue4", "Sprinkler" = "royalblue1")) +  # Set line colors
  theme_minimal()+
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))


TotalLand_YR= 
  gj1 %>%  
  select(regno,year_Registration,c_code01new,TotalLand,mistype) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56,
         year_Registration>=2007
  ) %>% 
  group_by(mistype,year_Registration)%>% 
  summarise(n=n(),Total_Land=mean(TotalLand,na.rm = T),SD=sd(TotalLand,na.rm = T),CI95delta= 1.96*(SD/sqrt(n))) %>% 
  rename(type=mistype)

TotalLand_2013= 
  gj1 %>%  
  select(regno,year_Registration,,TotalLand,mistype) %>%
  filter(!is.na(TotalLand),
         mistype=="Sprinkler",
         year_Registration==2013) %>% 
  arrange(desc(TotalLand))


##







# MIS_Land group_by(year_Registration)----

gj1 %>% 
  filter(year_Registration==2005 ) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56)

##
misarea_D= 
  gj1 %>% filter(mistype=="Drip" ) %>%   
  select(regno, RegistrationDate,year_Registration,c_code01new,misarea,TotalLand) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(year_Registration)%>% 
  summarise(n=n(),
            mis_area=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))
##
misarea_S=
  gj1 %>% filter(mistype=="Sprinkler" ) %>% 
  select(regno, RegistrationDate,year_Registration,c_code01new,misarea, TotalLand) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(year_Registration)%>% 
  summarise(n=n(),
            mis_area=mean(misarea,na.rm = T),
            SD=sd(misarea,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))

combined_year_Registration_misarea <- bind_rows(mutate(misarea_D, type = "Drip"),mutate(misarea_S, type = "Sprinkle"))

combined_year_Registration_misarea %>%
  ggplot(aes(year_Registration, mis_area)) +
  geom_ribbon(data = filter(combined_year_Registration_misarea, type == "Drip"),
              aes(ymin=mis_area-CI95delta, ymax=mis_area+CI95delta), 
              fill = "gray90", color = "white") +
  geom_ribbon(data = filter(combined_year_Registration_misarea, type == "Sprinkle"),
              aes(ymin=mis_area-CI95delta, ymax=mis_area+CI95delta), 
              fill = "gray90", color = "white") +
  geom_line(aes(color = type), size = 1) +
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkle"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' land installed MIS size")+
  ylab("Land installed MIS (in Ha)")+xlab("Registration Year") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))


library(kableExtra)
combined_year_Registration_misarea %>% mutate(Distance_Km = round(Distance_Km))%>% 
  kbl() %>% kable_styling()















########  Distance        ############################################################

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Earth radius in kilometers
  R <- 6371 
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance) # Distance in kilometers
}

dis= 
  gj1 %>%   
  filter(!is.na (RegistrationDate) ) %>% 
  select(regno,mistype,year_Registration, RegistrationDate, c_code01new ,Latitude,Longitude) %>%
  group_by(year_Registration) %>% 
  arrange(RegistrationDate) %>%
  filter(Latitude>20,Latitude<25, Longitude>68.5,Longitude<74.5) %>% 
  group_by(year_Registration) %>%
  mutate(distance = 
           haversine_distance(Latitude[1], Longitude[1], Latitude, Longitude) )%>% 
  ungroup() #%>% 
  filter(distance<5)

df=
  dis %>% 
  group_by(mistype,year_Registration)%>% 
  summarise(n=n(),
            Distance_Km=mean(distance,na.rm = T),
            SD=sd(distance,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n))) %>% 
  ungroup()

library(kableExtra)
df %>% mutate(Distance_Km = round(Distance_Km))%>% 
  kbl() %>% kable_styling()

df %>%
  ggplot(aes(year_Registration, Distance_Km)) +
  geom_ribbon(data = filter(df, mistype == "Drip"),
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), fill = "gray85", color = "white") +
  geom_ribbon(data = filter(df, mistype == "Sprinkler"),
              aes(ymin=Distance_Km-CI95delta, ymax=Distance_Km+CI95delta), fill = "gray80", color = "white") +
  geom_line(aes(color = mistype), size = 1) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkler"="royalblue1"),name="") +
  theme_light() +
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +
  ggtitle("Farms' Distance from the 1st farmer")+
  ylab("Distance (in Km)")+xlab("Registration Year") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))


# THE END----

# TotalLand group_by(year_Registration) ----
rank_TotalLand_D= gj1 %>% filter(mistype=="Drip" ) %>%   
  select(regno, RegistrationDate,year_Registration,c_code01new,TotalLand) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(year_Registration)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))

rank_TotalLand_S=gj1 %>% filter(mistype=="Sprinkler" ) %>% 
  select(regno, RegistrationDate,year_Registration,c_code01new,TotalLand) %>%
  filter(!is.na(TotalLand),TotalLand<13.88, TotalLand>0.56
  ) %>% 
  group_by(year_Registration)%>% 
  summarise(n=n(),
            Total_Land=mean(TotalLand,na.rm = T),
            SD=sd(TotalLand,na.rm = T),
            CI95delta= 1.96*(SD/sqrt(n)))

combined_year_Registration_totalland <- bind_rows(mutate(rank_TotalLand_D, type = "Drip"),mutate(rank_TotalLand_S, type = "Sprinkle"))

combined_year_Registration_totalland %>%
  ggplot(aes(year_Registration, Total_Land)) +
  geom_ribbon(data = filter(combined_year_Registration_totalland, type == "Drip"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), 
              fill = "gray90", color = "white") +
  geom_ribbon(data = filter(combined_year_Registration_totalland, type == "Sprinkle"),
              aes(ymin=Total_Land-CI95delta, ymax=Total_Land+CI95delta), 
              fill = "gray90", color = "white") +
  geom_line(aes(color = type), size = 1) +
  scale_x_continuous(breaks = seq(2005, 2013, 1)) +
  scale_color_manual(values=c("Drip"="blue4", "Sprinkle"="royalblue1"),name="") +
  theme_light() +
  ggtitle("Farms' land holding size")+ylab("Land (in Ha)")+xlab("Registration Year") +
  theme(plot.title = element_text(family = "serif"),axis.title = element_text(family = "serif"))

