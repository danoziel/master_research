        ---------------------------------
#       |         last version          |
        ---------------------------------
          
library(tidyverse)
library(kableExtra)

TALYAfarmer <- read.csv("~/R/TalYa/data/TALYAfarmer.csv")

write.csv(TALYAfarmer,"C:/Users/Dan/Documents/R/TalYa/TALYAfarmer.csv")


# harvesting tomato data----
#eliminate: Vijaya narasimha (bitter guord Summer 2019)
#           R Chenna kista(maskmelon Rabi 2020)

talya_Yield_w <- TALYAfarmer %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(starttime,mandal,village,farmer_name,username,
         harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,harvest_damage_CONTROL,
         KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,
         week_number,year,harvest_week,soweek)

talya_Yield_w <- talya_Yield_w[-26,]

talya_Yield_sum <- talya_Yield_w %>% 
  group_by(farmer_name) %>% 
  summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL",
                 "harvest_damage_talya100","harvest_damage_CONTROL",
                 "KG_sold_TALYA100" ,"KG_sold_CONTROL",
                 "revenue_TALYA100","revenue_CONTROL"
                 ), sum, na.rm = TRUE)

talya_Yield_sum <- inner_join(talya_Yield_sum,NE_talya_farmers,by="farmer_name")

talya_Yield_sum <-
  talya_Yield_sum %>%
  mutate(ty_harvest_kg_ac = (harvest_KG_talya100-harvest_damage_talya100) /acre,
         ctrl_harvest_kg_ac = (harvest_KG_CONTROL-harvest_damage_CONTROL) /acre,
         ty_kg_sold_ac = KG_sold_TALYA100 /acre,ctrl_kg_sold_ac = KG_sold_CONTROL/acre,
         ty_revenue_kg_ac =revenue_TALYA100/acre,ctrl_kg_revenue_ac =revenue_CONTROL/acre,
         ty_damage_kg_perc = harvest_damage_talya100 /harvest_KG_talya100,
         ctrl_damage_kg_perc = harvest_damage_CONTROL /harvest_KG_CONTROL
         )

means <- talya_Yield_sum%>%
  group_by(mulching_control) %>% 
  summarise_at(c("ty_harvest_kg_ac","ctrl_harvest_kg_ac",
                 "ty_kg_sold_ac", "ctrl_kg_sold_ac",
                 "ty_revenue_kg_ac", "ctrl_kg_CONTROL_ac",
                 "ty_damage_kg_perc", "ctrl_damage_kg_perc"),mean)

kable(talya_Yield_sum) %>% kable_styling()

# harvest ----
harvest <- talya_Yield%>%
  summarise(`Tal-Ya plot`=mean(ty_harvest_kg_ac),
            `Control Plot`=mean(ctrl_harvest_kg_ac)) %>% 
  mutate(across(is.numeric, round))# %>% summarise(`Tal-Ya plot`/`Control Plot`)

g_harvest <- harvest %>% gather("plot", "harvest", 1:2)

ggplot(g_harvest,aes(x=plot, y=harvest, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_minimal()+
  ggtitle("Harvest Per Acre (In Kg)") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=harvest), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

rm(harvest,g_harvest)

  
# damage  ----
damage <- talya_Yield %>% mutate(AVt=ty_damage_kg_ac/ty_harvest_kg_ac,
                             AVc=ctrl_damage_kg_ac/ctrl_harvest_kg_ac) %>% 
  summarise(`Tal-Ya plot`=mean(AVt)*100,`Control Plot`=mean(AVc)*100) %>% 
  mutate(across(is.numeric, round,2)) 

g_damage <- damage %>% gather("plot", "damage", 1:2)

library(scales)
ggplot(g_damage, aes(x=plot, y=damage, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Damaged harvest percent out of the total") +
  xlab(" ") +
  ylab("Damage % ")+
  geom_text(aes(x=plot, y=damage, label = percent(damage/100), vjust=1.5),
            position = position_dodge(width=0.9))+
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))







# talya_harvest_weekly ----
talya_harvest_weekly <- 
  talya_Yield_w %>%
  inner_join(NE_talya_farmers) %>% 
  filter(mulching_control==0) %>% 
  mutate(cntrl=harvest_KG_CONTROL/acre,ty=harvest_KG_talya100/acre) %>% 
  group_by(harvest_week) %>%
  summarise(Control=mean(cntrl),`Tal-Ya`=mean(ty)) %>% 
  mutate(across(is.numeric, round)) 

kable(talya_harvest_weekly) %>%kable_styling()

talya_harvest_weekly <- 
  talya_Yield_w %>%
  inner_join(NE_talya_farmers) %>% 
  filter(mulching_control==1) %>% 
  mutate(cntrl=harvest_KG_CONTROL/acre,ty=harvest_KG_talya100/acre) %>% 
  group_by(harvest_week) %>%
  summarise(Control=mean(cntrl),`Tal-Ya`=mean(ty)) %>% 
  mutate(across(is.numeric, round)) 

kable(talya_harvest_weekly) %>%kable_styling()







# harvest per farmer ----
harvest_per_farmer <- talya_Yield_w %>% 
  group_by(id) %>%
  summarise(mean(ctrl_harvest_kg_ac),mean(ty_harvest_kg_ac))

kable(harvest_per_farmer) %>% kable_styling()


# revenue per farmer ----
revenue_per_farmer <- talya_Yield_w %>% 
  group_by(id) %>%
  summarise(mean(ctrl_revenue_ac),mean(ty_revenue_ac))

kable(revenue_per_farmer) %>% kable_styling()

# Weed_treat_hours ----

xx <- TALYAfarmer %>% 
  inner_join(NE_talya_farmers) %>% 
  mutate(Weed_hours_control=Weed_treat_hours_control/acre,
         Weed_hours_talya100=Weed_treat_hours_talya100/acre) %>% 
  group_by(mulching_control,farmer_name) %>%
  summarise_at(c("Weed_hours_control","Weed_hours_talya100"),sum,na.rm = T) %>% 
  summarise_at(c("Weed_hours_control","Weed_hours_talya100"),mean,na.rm = T) %>% 
  mutate(Weed_hours_talya100/Weed_hours_control-1)



