library(tidyverse)
library(kableExtra)
# file----
library(haven)
Brooke_Ketchley_APSR_replicationI <-
  read_dta("~/master_research/DATAs/data_yotam/Brooke_Ketchley_APSR_replicationI.dta")
Brooke_Ketchley_APSR_replicationII <-
  read_dta("~/master_research/DATAs/data_yotam/Brooke_Ketchley_APSR_replicationII.dta")

mb <- Brooke_Ketchley_APSR_replicationI

# mb_sub----
mb_sub <-  mb%>% 
  mutate(pop_change_20yrs = ((district_total_pop - pop_1917)/ pop_1917)*100,
         unemployed_men_pct = (male_without_occupations/male_total)*100,
         state_admin_pct = (`_total_gov_employees`/`_population_over_5`)*100,
         european_pct = ((greek+french+british+italian)/district_total_pop)*100,
         missionaries_per10000 = (total_missionaries/district_total_pop)*10000,
         copts = (male_copt + female_copt),
         non_egyptian = (male_foreigners + female_foreigners),
         agriculture = (male_farmers_fishermen_and_hunti + female_farmers_fishermen_and_hun),
         agriculture_pct = (agriculture/total_population)*100,
         literate = (female_age_5_and_above_read_and_ + male_age_5_and_above_read_and_wr),
         religious_other = (male_other_religions + female_other_religions),
         non_muslim = (copts+religious_other),
         non_muslim_pct=(non_muslim/total_population)*100,
         literate_denom = (female_age_5_and_above_read_and_ + female_age_5_and_above_illiterat+
                             male_age_5_and_above_read_and_wr + male_age_5_and_above_illiterate),
         literate_pct = ((female_age_5_and_above_read_and_ + male_age_5_and_above_read_and_wr)/
                           literate_denom)*100) 

mb_subM <- 
  mb_sub %>% 
  select(european_pct,missionaries_per10000,state_admin_pct,non_muslim_pct)  

summary(mb_subM) %>% kable() %>% kable_classic()

#mb_dis----
mb_dis <- mb %>% 
  mutate(agriculture = (male_farmers_fishermen_and_hunti + female_farmers_fishermen_and_hun),
         literate = (female_age_5_and_above_read_and_ + male_age_5_and_above_read_and_wr),
         literate_denom = (female_age_5_and_above_read_and_ + female_age_5_and_above_illiterat+
                             male_age_5_and_above_read_and_wr + male_age_5_and_above_illiterate),
         copts = (male_copt + female_copt),religious_other = (male_other_religions + female_other_religions),
         non_muslim = (copts+religious_other),
         pop_change_20yrs = ((district_total_pop - pop_1917)/ pop_1917)*100,
         european_pct = ((greek+french+british+italian)/district_total_pop)*100,
         missionaries_per10000 = (total_missionaries/district_total_pop)*10000,
         state_admin_pct = (`_total_gov_employees`/`_population_over_5`)*100
         ) %>% 
  group_by(qism) %>% 
  summarise(mb_1937_c=sum(mb_1937),
            agriculture=sum(agriculture),total_population=sum(total_population),
            literate=sum(literate),literate_denom=sum(literate_denom),
            male_without_occupations=sum(male_without_occupations),male_total=sum(male_total),
            non_muslim=sum(non_muslim),
            state_railway_station=sum(state_railway_station),
            pop_change_20yrs=mean(pop_change_20yrs),
            european_pct=mean(european_pct),
            missionaries_per10000=mean(missionaries_per10000),
            state_admin_pct = mean(state_admin_pct),
            admin_centre=mean(admin_centre))%>% 
  mutate(agriculture_pct = (agriculture/total_population)*100,
            literate_pct=(literate/literate_denom)*100,
         unemployed_men_pct = (male_without_occupations/male_total)*100,
         non_muslim_pct=(non_muslim/total_population)*100
         )
# full model
mb_dis_lm <- 
  lm(mb_1937_c~literate_pct + agriculture_pct + unemployed_men_pct + non_muslim_pct +
     state_railway_station + pop_change_20yrs + european_pct + missionaries_per10000 +
     state_admin_pct + admin_centre,
     mb_dis)
summary(mb_dis_lm)

# wuthout european_pct ecs... 
dlm <- 
  lm(mb_1937_c~literate_pct + agriculture_pct + unemployed_men_pct +
       state_railway_station + pop_change_20yrs +
       state_admin_pct + admin_centre,
     mb_dis)

dlm <- 
  lm(mb_1937_c~literate_pct + agriculture_pct + unemployed_men_pct ,
     mb_dismb_dis)
summary(dlm)

#3
mb_dismb_dis <- mb_dis %>%mutate_at(16:18,round) 

dlm <- 
  lm(mb_1937_c~state_railway_station,
     mb_dis)

dlm <- 
  lm(state_railway_station~literate_pct + agriculture_pct + unemployed_men_pct ,
     mb_dismb_dis)
summary(dlm)

cor(agriculture_pct,state_railway_station)

ggplot(mb, aes(x=mb_1937, y=state_railway_station)) + geom_point()+
  xlim(0,0.20)+ylim(0,0.20)
 
#subdis: literate_pct  agriculture_pct unemployed_men_pct non_muslim_pct state_railway_station
#dis:    pop_change_20yrs european_pct missionaries_per10000 state_admin_pct admin_centre

x <- mb %>% select(mb_1937,state_railway_station) %>% 
  mutate(A=mb_1937+state_railway_station) %>% 
  mutate(B=ifelse(A==1,0,A),both=ifelse(B==2,1,B)) %>% 
  summarise(sum(mb_1937),sum(state_railway_station),sum(both))

#map----
library(leaflet)
library(rgdal)
library(dplyr)
mb_map <- mb  %>% 
  rename(longitude = district_X, latitude=district_Y) %>% 
  select(1:2,longitude,latitude) %>% distinct() %>% 
  inner_join(mb_dis)

mb_map[66,4] <- 30.59417
mb_map[66,3] <- 32.30278
  
cairo_lon <- 30.0512531427455 
cairo_lat <- 31.2531474997934

leaflet() %>%
  setView(lng = cairo_lat, lat = cairo_lon, zoom = 6) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = mb_map,
    popup = paste0(
      "<mb_map>qism: </mb_map>",
      mb_map$qism, "<br>"))


    radius = sqrt(10^mb_map$agriculture_pct) *10
    color = "#008B00",
    fillColor = "#008B00",
    fillOpacity = 0.2)






# means
s1 <- mb_sub %>% 
  summarise_at(c("pop_change_20yrs", "unemployed_men_pct","pop_change_20yrs",
                 "unemployed_men_pct","state_admin_pct", "european_pct",
                 "missionaries_per10000","agriculture_pct","non_muslim_pct","literate_pct"),
               mean,na.rm=T)

s2 <- mb_dis %>% 
  summarise_at(c("pop_change_20yrs", "unemployed_men_pct","pop_change_20yrs",
                 "unemployed_men_pct","state_admin_pct", "european_pct",
                 "missionaries_per10000","agriculture_pct","non_muslim_pct","literate_pct"),
               mean,na.rm=T)

s12 <-
  
  rbind(s1,s2) %>%
  mutate(across(is.numeric, round, 2)) %>% 
  add_column(new_col = c("District","Subdistrict")) %>% 
  select(9,1:8)%>% 
  kable() %>% kable_classic()
  









