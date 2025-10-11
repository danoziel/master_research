> TALYAplot <- read.csv("~/R/TalYa/data/TALYAplot.csv")

talya_plot <- TALYAplot

talya_plot$starttime <- as.Date(talya$starttime, "%d/%m/%y")
class(talya_plot$starttime)

names(talya_plot)
attach(talya_plot)

talya_plot <- talya_plot[6:144,]

talya_plot <- talya_plot %>%
  select(starttime,8,11:13,
         Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,Plant_height_talya100_plot_3,
         
         Plant_Branches_control_1,Plant_Branches_control_2,Plant_Branches_control_3,
         Plant_Branches_talya100_plot_1,Plant_Branches_talya100_plot_2,Plant_Branches_talya100_plot_3,
         
         Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         
         Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         
         Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
         Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,Fruit_Circumference_talya100_plot_3,
         
         Fruit_Length_control_1,Fruit_Length_control_2,Fruit_Length_control_3,
         fruit_length_talya100_1,fruit_length_talya100_2,Fruit_Length_control_3,
         
         Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,Fruit_Weight_talya100_plot_2,Fruit_Weight_talya100_plot_3,
         
         crop_damages_num_control,crop_damages_num_talya100,comments_control,comments_talya100,year,week.)



