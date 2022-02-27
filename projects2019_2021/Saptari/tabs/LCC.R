LCC_solar_diesel <- read_csv("~/master_research/DATAs/data_master/data_saptari/solar_vs_diesel_LCC - LCC_table.csv")
LCC_solar_diesel
names(LCC_solar_diesel)
library(tidyverse)
library(gridExtra)
library(extrafont)
library(scales)

LCC_solar_diesel$user[LCC_solar_diesel$user == "Low_intensity_user"] <- "Low\nIntensity\nUser"
LCC_solar_diesel$user[LCC_solar_diesel$user == "Average_user"] <- "Average\nUser"
LCC_solar_diesel$user[LCC_solar_diesel$user == "High_intensity_user"] <- "High\nIntensity\nUser"

LCC_solar_diesel$Costs <- factor(LCC_solar_diesel$Costs, levels = c("Replacement_Cost","Fuel_Cost","Maintenance_Cost","Capital_Cost","Salvage_Cost"))


  scale_fill_manual(values=c("black", "red","green", "blue"))
  scale_fill_manual(values=c("#E7B800", "#2E9FDF", "#FC4E07","thistle3"))

# ----
1000/800
LCC_solar_diesel %>% filter(Costs != "Salvage_Cost",user!="Optimal_utilization") %>% 
  ggplot(aes(x = user, y = value_20y, fill = Costs,label = Costs)) +
  geom_bar(stat = "identity",width=0.5) +
  theme_minimal()+
  labs(x = "SPIP      Diesel Engine Pump",y="Life Cycle Cost (in USD)")+
  theme(panel.grid.minor= element_blank(),panel.grid.major.x = element_blank(),text = element_text(family = "serif"))+
  theme(axis.text.x = element_text(size = (3)),
        axis.text.y = element_text(size = (3)),
        axis.title.x = element_text(size=16),
        axis.title. = element_text(size=16))+
  scale_x_discrete(limits = c("SPIP","Low\nIntensity\nUser","Average\nUser","High\nIntensity\nUser"))+
  scale_fill_brewer(palette = "Spectral")

# ----
800/200
LCC_solar_diesel %>% filter(Costs != "Salvage_Cost",user %in% c("Optimal_utilization","SPIP")) %>% 
  ggplot(aes(x = pumping_system, y = value_20y, fill = Costs,label = Costs)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  theme_minimal()+
  labs(x =" ",y="Life Cycle Cost (in USD)")+
  theme(panel.grid.minor= element_blank(),panel.grid.major.y = element_blank(),text = element_text(family = "serif"))+
  theme(axis.text.x = element_text(size = (13)),
        axis.text.y = element_text(size = (16)),
        axis.title.x = element_text(size=16),
        plot.title = element_text(hjust = 0.5,size = (30)))+
  scale_x_discrete(labels=c("PV system" = "SPIP", "Diesel_Engine" = "Diesel\nPump"))+
  scale_fill_brewer(palette = "Spectral")
  
