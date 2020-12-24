>TALYAfarmer <- read.csv("~/R/TalYa/data/TALYAfarmer.csv")
area_talya_farmers <- read.csv("~/R/TalYa/data/area_talya_farmers.csv")

write.csv(talya,"C:/Users/Dan/Documents/R/TalYa/talya11.csv")


# harvesting tomato data----
#eliminate: Vijaya narasimha (bitter guord Summer 2019)
#           R Chenna kista(maskmelon Rabi 2020)
talya <- TALYAfarmer %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(2,18,19,20,harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,
         harvest_damage_CONTROL,KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,average_price_TALYA100,8,harvest_yesno_talya100,
         week_number,year)

talya <- talya[-26,]

talya <- full_join(talya,area_talya_farmers,by="farmer_name")

talya <- talya %>% mutate(ty_harvest_kg_ac = harvest_KG_talya100 /acre,
                          ctrl_harvest_kg_ac = harvest_KG_CONTROL /acre,
                          ty_damage_kg_ac = harvest_damage_talya100 /acre,
                          ctrl_damage_kg_ac = harvest_damage_CONTROL /acre,
                          ty_kg_sold_ac = KG_sold_TALYA100 /acre,
                          ctrl_kg_sold_ac = KG_sold_CONTROL /acre,
                          ty_revenue_ac = revenue_TALYA100 /acre,
                          ctrl_revenue_ac = revenue_CONTROL /acre)

talya <- talya %>% group_by(id) %>% 
  summarise_at(vars(ty_harvest_kg_ac :ctrl_revenue_ac), sum, na.rm = TRUE) 


# g_revenue-----
g_revenue <- talya%>%
  summarise(`Tal-Ya plot`=mean(ty_revenue_ac),
            `Control Plot`=mean(ctrl_revenue_ac)) %>% 
  mutate(across(is.numeric, round)) %>%
  summarise(`Tal-Ya plot`/`Control Plot`)

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

g_harvest x <- talya%>%
  summarise(`Tal-Ya plot`=mean(ty_harvest_kg_ac),
            `Control Plot`=mean(ctrl_harvest_kg_ac)) %>% 
  mutate(across(is.numeric, round)) %>%
  summarise(`Tal-Ya plot`/`Control Plot`)

g_harvest <- g_harvest %>% tidyr::gather("plot", "harvest", 1:2)

g_harvest <- ggplot(g_harvest, 
       aes(x=plot, y=harvest, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Harvest Per Acre (In Kg)") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=harvest), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))
  

# g-sold------
g_sold <- talya%>%
  summarise(`Tal-Ya plot`=mean(ty_kg_sold_ac),
            `Control Plot`=mean(ctrl_kg_sold_ac)) %>% 
  mutate(across(is.numeric, round)) %>%
  summarise((`Tal-Ya plot`-`Control Plot`)/`Control Plot`)

g_sold <- g_sold %>% tidyr::gather("plot", "sold", 1:2)

g_sold <- ggplot(g_sold, 
       aes(x=plot, y=sold, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_minimal()+
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
g_damage <- talya %>% mutate(AVt=ty_damage_kg_ac/ty_harvest_kg_ac,
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

g_farmers_revenue <- talya %>% select(id,ty_revenue_ac,ctrl_revenue_ac) %>% 
  rename(`Tal-Ya`=ty_revenue_ac,`Control`= ctrl_revenue_ac) %>% 
  mutate(across(is.numeric, round))

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

# g_kg_price------

talya$starttime <- as.Date(talya$starttime, "%d/%m/%y")

class(talya$starttime)




g_price_kg_tomato <- talya %>% group_by(week_number) %>%
  filter(!is.na(average_price_TALYA100),average_price_TALYA100!=1.25) %>% 
  summarise(average_price= mean(average_price_TALYA100)) %>% 
  mutate(across(is.numeric, round,2))


g_price_kg_tomato <- ggplot(g_price_kg_tomato) +theme_grey()+
  geom_line(aes(y = average_price, x = week_number),
            size=0.7, stat="identity", color = "darkgreen")+
  ggtitle("Prices per kg of tomatoes (In IRs.)") +
  labs(x="Week number", y="Kg Price")+
  geom_vline(xintercept = 16)+
  scale_x_continuous(breaks=seq(8,22,1))+
  scale_y_continuous(breaks=seq(1.5,8,1))+
  theme(panel.grid.minor = element_blank())

-----------------------------------------------------------------
  


# ----sctters-harvest vs. sold-harvest vs. revenue----

plot(ty_harvest_kg_ac,ty_kg_sold_ac)
plot(ctrl_harvest_kg_ac,ctrl_kg_sold_ac)


p1 <- ggplot(talya,aes(x = ty_harvest_kg_ac, y = ty_kg_sold_ac, color = week_number))+
  geom_point()+
  ggtitle("tal ya -harvest vs. sold") +
  labs(x = "talya harvest kg per ac", y = "talya kg sold kg ac")

p2 <- ggplot(talya,aes(x = ctrl_harvest_kg_ac, y = ctrl_kg_sold_ac,color= farmer_name)) +
  geom_point()+
  ggtitle("control -harvest vs. sold") +
  labs(x = "control harvest kg per ac", y = "control kg sold kg ac")

p3 <- ggplot(talya,aes(x = ty_harvest_kg_ac, y = ty_revenue_ac,color= farmer_name)) +
  geom_point()+
  ggtitle("tal ya -harvest vs. revenue") +
  labs(x = "talya harvest kg per ac", y = "talya kg revenue kg ac")

p4 <- ggplot(talya,aes(x = ctrl_harvest_kg_ac, y = ctrl_revenue_ac,color= farmer_name)) +
  geom_point()+
  
gridar
  ggtitle("control -harvest vs. revenue") +
  labs(x = "control harvest kg per ac", y = "control kg revenue kg ac")

  
  
  
  (p1, p2, p3, p4, cols=2)


p1 + facet_wrap( ~ farmer_name, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")


# maps-----
library(maps)
UK <- map_data("world") %>% filter(region=="India")

ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() + ylim(50,59) + coord_map() 



AIzaSyBodc2rWbBNhzNJ_idXVtiE-hhHd4nlRoE
key<-"AIzaSyBodc2rWbBNhzNJ_idXVtiE-hhHd4nlRoE"
register_google(key = "AIzaSyBodc2rWbBNhzNJ_idXVtiE-hhHd4nlRoE")
Sys.setenv(SHERPAROMEO_KEY = "AIzaSyBodc2rWbBNhzNJ_idXVtiE-hhHd4nlRoE")
set.api.key("AIzaSyBodc2rWbBNhzNJ_idXVtiE-hhHd4nlRoE")
ggmap(get_googlemap()) 
geocode("Houston", output = "all")
map <- get_googlemap("Montpellier, France", zoom = 8, maptype = "terrain")
ggmap(get_map("Houston"))
ggmap(
  ggmap = get_map(
    "Dayton",
    zoom = 13, scale = "auto",
    maptype = "satellite",
    source = "google"),
  extent = "device",
  legend = "topright"
)