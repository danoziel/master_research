write.csv(TALYAplot,"C:/Users/Dan/Documents/R/TalYa/data/TALYAplot.csv", row.names = FALSE)
TALYAplot <- read_excel("data/TALYAplot.xlsx")

talya_tomato <- TALYAplot %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista")

Plant_height <- talya_tomato %>%
  inner_join(NE_talya_farmers,by="farmer_name") %>% 
  filter(Plant_height_control_1>0) %>%
  select(farmer_name,Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,year,week.,weeknum.year,week_growth,mulching_control) %>% 
  summarise(mean())



# Plant_height    +7%   ---- 
Plant_height <- talya_tomato %>% 
  inner_join(NE_talya_farmers,by="farmer_name") %>% 
  filter(Plant_height_control_1>0) %>%
  select(Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,mulching_control,week_growth) %>%
  
  mutate(MeanC=(Plant_height_control_1+Plant_height_control_2+Plant_height_control_3)/3,
         MeanT=(Plant_height_talya100_plot_1+Plant_height_talya100_plot_2+Plant_height_talya100_plot_3)/3) %>% 
  
  group_by(mulching_control,week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT),`Control`=mean(MeanC)) %>% 
  mutate(across(is.numeric, round)) 
  
#  transmute(week_growth,MeanC = rowMeans(select(., Plant_height_control_1:Plant_height_control_3)),
#            MeanT = rowMeans(select(., Plant_height_talya100_plot_1:Plant_height_talya100_plot_3))) %>% 


kable(Plant_height) %>%kable_styling()

# graph

Plant_height <- gather(Plant_height, "Group", "value", 2:3)

g_Plant_height <- ggplot(data=Plant_height, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Plant Height (In cm)") +
  xlab("week_growth ") +
  ylab("Height")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_Plant_height

# Plant_Fruits    +27%  ----

Plant_Fruits <- talya_tomato %>%
  inner_join(NE_talya_farmers,by="farmer_name") %>% 
  filter(mulching_control==1) %>%
  select(farmer_name, Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Plant_Fruits_control_1:Plant_Fruits_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Fruits_talya100_plot_1:Plant_Fruits_talya100_plot_3),na.rm = T)) %>% 
  group_by(week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0) #%>% mutate(Week=1:11)

kable(Plant_Fruits) %>%kable_styling()


# graph
  
Plant_Fruits <- gather(Plant_Fruits, "Group", "value", 2:3)

g_tomato_fruits <- ggplot(data=Plant_Fruits, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Fruits per Plant") +
  xlab("Week ") +
  ylab("No. Fruits")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_fruits




# tomato_flowers  +9%   ----

Plant_Flowers <- talya_tomato %>% 
  inner_join(NE_talya_farmers,by="farmer_name") %>% 
  filter(mulching_control==0) %>%
  select(Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Plant_Flowers_control_1:Plant_Flowers_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Flowers_talya100_plot_1:Plant_Flowers_talya100_plot_3),na.rm = T)) %>% 
  group_by(week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) 

kable(Plant_Flowers) %>%kable_styling()

View(Plant_Flowers)

kable(Plant_Flowers) %>%kable_styling()
week_growth
# graph

Plant_Flowers <- gather(Plant_Flowers, "Group", "value", 2:3)

g_tomato_flowers <- ggplot(data=Plant_Flowers, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Flowers per Plant") +
  xlab("Week ") +
  ylab("No. flowers")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_flowers


  
# Plant_Branches  +23%  ----
Plant_Branches<-
  talya_tomato %>%
  inner_join(NE_talya_farmers,by="farmer_name") %>% 
  filter(mulching_control==1) %>%
  select(Plant_Branches_control_1,Plant_Branches_control_2,Plant_Branches_control_3,
                          Plant_Branches_talya100_plot_1,Plant_Branches_talya100_plot_2,
                          Plant_Branches_talya100_plot_3,week_growth) %>% 
na_if(0) %>% 
  transmute(week_growth, MeanC = rowMeans(select(., Plant_Branches_control_1:Plant_Branches_control_3),na.rm = T),
            MeanT = rowMeans(select(.,Plant_Branches_talya100_plot_1:Plant_Branches_talya100_plot_3),na.rm = T)) %>% 
  group_by(week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) 

kable(Plant_Branches) %>%kable_styling()

# graph

Plant_Branches <- gather(Plant_Branches, "Group", "value", 2:3)

g_Plant_Branches <- ggplot(data=Plant_Branches, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of branches per Plant") +
  xlab("Week ") +
  ylab("No. branches")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_Plant_Branches



# Fruit_Weight    +17%  ----
Fruit_Weight <- talya_tomato %>% 
  select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,
         Fruit_Weight_talya100_plot_2,
         Fruit_Weight_talya100_plot_3,week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Fruit_Weight_control_1:Fruit_Weight_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Weight_talya100_plot_1:Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
  group_by(week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`> 0) 

kable(Fruit_Weight) %>%kable_styling()

View(tomato_fruits)

Fruit_Weight <- gather(Fruit_Weight, "Group", "value", 2:3)

g_tomato_circumference <- ggplot(data=Fruit_Weight, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Weight of Tomato (In gr)") +
  xlab("Week ") +
  ylab("Weight")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4)


# Fruit_Circumference  +9% -----
Fruit_Circumference <-
  talya_tomato %>% select(Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
                          Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,
                          Fruit_Circumference_talya100_plot_3,week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Fruit_Circumference_control_1:Fruit_Circumference_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Circumference_talya100_plot_1:Fruit_Circumference_talya100_plot_3),na.rm = T)) %>% 
  group_by(week_growth) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0)
View(Fruit_Circumference)

kable(Fruit_Circumference) %>%kable_styling()


# graph

Fruit_Circumference <- gather(Fruit_Circumference, "Group", "value", 2:3)

g_tomato_circumference <- ggplot(data=Fruit_Circumference, aes(x=week_growth, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Circumference of The Fruit (In cm)") +
  xlab("Week ") +
  ylab("Circumference")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = week_growth, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4)
g_tomato_circumference




#plant - height, Fruits,Flowers,Branches----

#Plant_height
xheight <- talya_tomato %>% 
  filter(Plant_height_control_1>0) %>%
  select(Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,year,week.,weeknum.year) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_height_control_1:Plant_height_control_3)),
            MeanT = rowMeans(select(., Plant_height_talya100_plot_1:Plant_height_talya100_plot_3))) %>% 
  summarise(`Tal-Ya`=mean(MeanT),`Control`=mean(MeanC)) %>% 
  mutate(across(is.numeric, round,2))

xheight <- xheight %>% gather("Group", "value", 1:2) %>% mutate(prop="Height")


# Plant_Fruits
xfruits <- talya_tomato %>% 
  select(Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Fruits_control_1:Plant_Fruits_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Fruits_talya100_plot_1:Plant_Fruits_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2))

xfruits <- xfruits %>% gather("Group", "value", 1:2) %>% mutate(prop="Fruits")


  # Plant_Flowers
xflowers <- talya_tomato %>% 
  select(Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Flowers_control_1:Plant_Flowers_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Flowers_talya100_plot_1:Plant_Flowers_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2)) 

xflowers <- xflowers %>% gather("Group", "value", 1:2) %>% mutate(prop="Flowers")


# Plant_Branches
xBranches<-
  talya_tomato %>% select(Plant_Branches_control_1,Plant_Branches_control_2,Plant_Branches_control_3,
                          Plant_Branches_talya100_plot_1,Plant_Branches_talya100_plot_2,
                          Plant_Branches_talya100_plot_3,weeknum.year) %>% na_if(0) %>% 
  transmute(weeknum.year, MeanC = rowMeans(select(., Plant_Branches_control_1:Plant_Branches_control_3),na.rm = T),
            MeanT = rowMeans(select(.,Plant_Branches_talya100_plot_1:Plant_Branches_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2))

xBranches <- xBranches %>% gather("Group", "value", 1:2) %>% mutate(prop="Branches")


# Fruit_Weight

xFruit_Weight <- 
  talya_tomato %>% 
  select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,
         Fruit_Weight_talya100_plot_2,
         Fruit_Weight_talya100_plot_3,week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Fruit_Weight_control_1:Fruit_Weight_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Weight_talya100_plot_1:Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2)) %>% 
  filter(`Tal-Ya`> 0) 

xFruit_Weight <- gather(xFruit_Weight, "Group", "value", 2:3)

# xCircumference
xCircumference <-
  talya_tomato %>% select(Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
                          Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,
                          Fruit_Circumference_talya100_plot_3,week_growth) %>% 
  na_if(0) %>% 
  transmute(week_growth,MeanC = rowMeans(select(., Fruit_Circumference_control_1:Fruit_Circumference_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Circumference_talya100_plot_1:Fruit_Circumference_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2)) %>% 
  filter(`Tal-Ya`>0)

xCircumference <- gather(xCircumference, "Group", "value", 2:3)

# plant_prop
plant_prop <- rbind(xheight,xfruits,xflowers,xBranches,xFruit_Weight,xCircumference)

plant_prop %>%
  kable() %>%
  kable_styling()

ggplot(plant_prop, aes(fill=Group, y=value, x=prop)) + 
  theme_minimal()+
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Plant Characteristics") +
  xlab(" ") +
  ylab(" ")+
  geom_text(
    aes(x = prop, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)

----------------------------------------------------------------------------------------------
  