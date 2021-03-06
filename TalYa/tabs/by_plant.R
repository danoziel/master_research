talya_by_plant <- TALYAfarmer %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(starttime,mandal,village,farmer_name,harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,
         harvest_damage_CONTROL,KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,average_price_TALYA100,username,harvest_yesno_talya100,
         week_number,year)
talya_plant
talya_by_plant <- talya_by_plant[-26,]

talya_by_plant <- full_join(talya_by_plant,area_talya_farmers,by="farmer_name")

talya_by_plant <- talya_by_plant %>% mutate(ty_harvest_kg_ac = harvest_KG_talya100 /trays,
                          ctrl_harvest_kg_ac = harvest_KG_CONTROL /trays,
                          ty_damage_kg_ac = harvest_damage_talya100 /trays,
                          ctrl_damage_kg_ac = harvest_damage_CONTROL /trays,
                          ty_kg_sold_ac = KG_sold_TALYA100 /trays,
                          ctrl_kg_sold_ac = KG_sold_CONTROL /trays,
                          ty_revenue_ac = revenue_TALYA100 /trays,
                          ctrl_revenue_ac = revenue_CONTROL /trays)

talya_by_plant <- talya_by_plant %>% group_by(id) %>% 
  summarise_at(vars(ty_harvest_kg_ac :ctrl_revenue_ac), sum, na.rm = TRUE) 

# g_revenue-----
g_revenue <- talya_plant%>%
  summarise(`Tal-Ya plot`=mean(ty_revenue_ac),
            `Control Plot`=mean(ctrl_revenue_ac)) %>% 
  mutate(across(is.numeric, round,3)) %>%
  summarise((`Tal-Ya plot`-`Control Plot`)/`Control Plot`)

g_revenue <- g_revenue %>% tidyr::gather("plot", "Revenue", 1:2)

g_revenue <- ggplot(g_revenue, 
                    aes(x=plot, y=Revenue, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Revenue Per Acre") +
  xlab(" ") +
  ylab("Revenue ")+
  geom_text(aes(label=Revenue), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

g_revenue





# g-harvest----

g_harvest <- talya_by_plant%>%
  summarise(`Tal-Ya plot`=mean(ty_harvest_kg_ac),
            `Control Plot`=mean(ctrl_harvest_kg_ac)) %>% 
  mutate(across(is.numeric, round,3)) %>%
  summarise(`Tal-Ya plot`/`Control Plot`)

g_harvest <- g_harvest %>% tidyr::gather("plot", "harvest", 1:2)

g_harvest <- ggplot(g_harvest, 
                    aes(x=plot, y=harvest, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Harvest Per Plant (In Kg)") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=harvest), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))
g_harvest










# g-sold------
g_sold <- talya_plant%>%
  summarise(`Tal-Ya plot`=mean(ty_kg_sold_ac),
            `Control Plot`=mean(ctrl_kg_sold_ac)) %>% 
  mutate(across(is.numeric, round,3)) %>%
  summarise((`Tal-Ya plot`-`Control Plot`)/`Control Plot`)

g_sold <- g_sold %>% tidyr::gather("plot", "sold", 1:2)

g_sold <- ggplot(g_sold, 
                 aes(x=plot, y=sold, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Kg Sold Per Acre") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=sold), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

g_sold

















# g_damage-----

# damaged harvest as percent of the total harvest
g_damage <- talya_plant %>% mutate(AVt=ty_damage_kg_ac/ty_harvest_kg_ac,
                             AVc=ctrl_damage_kg_ac/ctrl_harvest_kg_ac) %>% 
  summarise(`Tal-Ya plot`=mean(AVt)*100,`Control Plot`=mean(AVc)*100) %>% 
  mutate(across(is.numeric, round,2)) 

g_damage <- g_damage %>% tidyr::gather("plot", "damage", 1:2)

library(scales)
g_damage <- ggplot(g_damage, aes(x=plot, y=damage, fill=plot)) + 
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






# g_farmers_revenue----

g_farmers_revenue <- talya_plant %>% select(id,ty_revenue_ac,ctrl_revenue_ac) %>% 
  rename(`Tal-Ya`=ty_revenue_ac,`Control`= ctrl_revenue_ac) %>% 
  mutate(across(is.numeric, round,3))

g_farmers_revenue <- gather(g_farmers_revenue, "Group", "value", 2:3)

df.mean = g_farmers_revenue %>% 
  group_by(Group) %>% 
  mutate(ymean = mean(value))

ggplot(g_farmers_revenue, aes(id, value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  geom_errorbar(data=df.mean, aes(id, ymax = ymean, ymin = ymean),
                size=0.5, linetype = "longdash", inherit.aes = F, width = 1)

g_farmers_revenue <- ggplot(data=g_farmers_revenue, aes(x=id, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_update()+
  ggtitle("Revenue Per Acre") +
  xlab("Farmer id") +
  ylab("Revenue")+
  geom_text(
    aes(x = id, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)+
  theme(axis.text.x = element_text(angle=0, vjust = 0.7))



# g_farmers_harvest----

g_farmers_harvest <- talya %>% select(id,ty_harvest_kg_ac,ctrl_harvest_kg_ac) %>% 
  rename(`Tal-Ya`=ty_harvest_kg_ac,`Control`= ctrl_harvest_kg_ac) %>% 
  mutate(across(is.numeric, round))

g_farmers_harvest <- gather(g_farmers_harvest, "Group", "value", 2:3)

g_farmers_harvest <- ggplot(data=g_farmers_harvest, aes(x=id, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_update()+
  ggtitle("Harvest Kg Per Acre") +
  xlab("Farmer id") +
  ylab("Kg")+
  geom_text(
    aes(x = id, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)







