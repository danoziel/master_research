
talya_tomato <- TALYAplot %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista")

# Plant_height    +7%   ---- 
Plant_height <- talya_tomato %>% 
  filter(Plant_height_control_1>0) %>%
  select(Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,year,week.,weeknum.year) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_height_control_1:Plant_height_control_3)),
            MeanT = rowMeans(select(., Plant_height_talya100_plot_1:Plant_height_talya100_plot_3))) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT),`Control`=mean(MeanC)) %>% 
  mutate(across(is.numeric, round)) %>% 
  mutate(Week=1:17)
View(lant_height)

# graph

Plant_height <- gather(Plant_height, "Group", "value", 2:3)

g_Plant_height <- ggplot(data=Plant_height, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Plant Height (In cm)") +
  xlab("Week ") +
  ylab("Height")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_Plant_height

# Average height per Plant for the entire growing season
# +5.4698%

talya_tomato %>% 
  filter(Plant_height_control_1>0) %>%
  select(Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,year,week.,weeknum.year) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_height_control_1:Plant_height_control_3)),
            MeanT = rowMeans(select(., Plant_height_talya100_plot_1:Plant_height_talya100_plot_3))) %>% 
  summarise(`Tal-Ya`=mean(MeanT),`Control`=mean(MeanC)) 
#    Tal-Ya   Control
#  72.49583   68.73611
       

# Plant_Fruits    +27%  ----

Plant_Fruits <- talya_tomato %>% 
  select(Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Fruits_control_1:Plant_Fruits_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Fruits_talya100_plot_1:Plant_Fruits_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0) %>% 
  mutate(Week=1:11)
View(Plant_Fruits)

# graph
  
Plant_Fruits <- gather(Plant_Fruits, "Group", "value", 2:3)

g_tomato_fruits <- ggplot(data=Plant_Fruits, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Fruits per Plant") +
  xlab("Week ") +
  ylab("No. Fruits")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_fruits

# Average number of fruits per Plant for the entire growing season
# + 27%
+
talya_tomato %>% 
  select(Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Fruits_control_1:Plant_Fruits_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Fruits_talya100_plot_1:Plant_Fruits_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round))
  
#  Tal-Ya  Control
# 27.35641 22.45082


# tomato_flowers  +9%   ----

Plant_Flowers <- talya_tomato %>% 
  select(Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Flowers_control_1:Plant_Flowers_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Flowers_talya100_plot_1:Plant_Flowers_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Control`>0) %>% 
  mutate_all(funs(replace_na(.,0))) %>% 
  mutate(Week=1:13)
View(Plant_Flowers)

# graph

Plant_Flowers <- gather(Plant_Flowers, "Group", "value", 2:3)

g_tomato_flowers <- ggplot(data=Plant_Flowers, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Flowers per Plant") +
  xlab("Week ") +
  ylab("No. flowers")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_flowers

#avarage

talya_tomato %>% 
  select(Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Flowers_control_1:Plant_Flowers_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Flowers_talya100_plot_1:Plant_Flowers_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T))

#  Tal-Ya  Control
# 12.03958 11.42105
  
# Plant_Branches  +23%  ----
Plant_Branches<-
  talya_tomato %>% select(Plant_Branches_control_1,Plant_Branches_control_2,Plant_Branches_control_3,
                          Plant_Branches_talya100_plot_1,Plant_Branches_talya100_plot_2,
                          Plant_Branches_talya100_plot_3,weeknum.year) %>% 
na_if(0) %>% 
  transmute(weeknum.year, MeanC = rowMeans(select(., Plant_Branches_control_1:Plant_Branches_control_3),na.rm = T),
            MeanT = rowMeans(select(.,Plant_Branches_talya100_plot_1:Plant_Branches_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  mutate(Week=1:17)

# graph

Plant_Branches <- gather(Plant_Branches, "Group", "value", 2:3)

g_Plant_Branches <- ggplot(data=Plant_Branches, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of branches per Plant") +
  xlab("Week ") +
  ylab("No. branches")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_Plant_Branches

# avarage

talya_tomato %>% select(Plant_Branches_control_1,Plant_Branches_control_2,Plant_Branches_control_3,
                        Plant_Branches_talya100_plot_1,Plant_Branches_talya100_plot_2,
                        Plant_Branches_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year, MeanC = rowMeans(select(., Plant_Branches_control_1:Plant_Branches_control_3),na.rm = T),
            MeanT = rowMeans(select(.,Plant_Branches_talya100_plot_1:Plant_Branches_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) 

#   Tal-Ya  Control
#  10.74722 9.306944

# Fruit_Weight    +17%  ----
Fruit_Weight <- talya_tomato %>% 
  select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,
         Fruit_Weight_talya100_plot_2,
         Fruit_Weight_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Weight_control_1:Fruit_Weight_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Weight_talya100_plot_1:Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`> 0) %>% 
  mutate(Week=1:11)
View(tomato_fruits)


Fruit_Weight <- gather(Fruit_Weight, "Group", "value", 2:3)

g_tomato_circumference <- ggplot(data=Fruit_Weight, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Weight of Tomato (In gr)") +
  xlab("Week ") +
  ylab("Weight")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4)

# Average weight per tomato for the entire growing season
talya_tomato %>% 
  select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,Fruit_Weight_talya100_plot_2,
         Fruit_Weight_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,
            MeanC = rowMeans(select
                             (., Fruit_Weight_control_1:
                                 Fruit_Weight_control_3),na.rm = T),
            MeanT = rowMeans(select
                             (., Fruit_Weight_talya100_plot_1:
                                 Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T))

#   Tal-Ya Control
# 20.50629 18.1859






# Fruit_Circumference  +9% -----
Fruit_Circumference <-
  talya_tomato %>% select(Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
                          Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,
                          Fruit_Circumference_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Circumference_control_1:Fruit_Circumference_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Circumference_talya100_plot_1:Fruit_Circumference_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0) %>% 
  mutate(Week=1:11)
View(Fruit_Circumference)

# graph

Fruit_Circumference <- gather(Fruit_Circumference, "Group", "value", 2:3)

g_tomato_circumference <- ggplot(data=Fruit_Circumference, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Circumference of The Fruit (In cm)") +
  xlab("Week ") +
  ylab("Circumference")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4)
g_tomato_circumference

# avarage#
talya_tomato %>% select(Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
                        Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,
                        Fruit_Circumference_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Circumference_control_1:Fruit_Circumference_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Circumference_talya100_plot_1:Fruit_Circumference_talya100_plot_3),na.rm = T)) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T))

#   Tal-Ya  Control
# 11.75185 11.61038

