library(ggplot2)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(formattable)
library(kableExtra)



# generic stuff----
https://www.learnbyexample.org/r-plot-function/
main="Main plot title",
xlab="x‐axis label",
ylab="y‐axis label",
las = 1)
, breaks = 40
, breaks = c(0,1,2,3,4,5,6,7)
ylim = c(0,80)

coord_flip()+

# histogram----
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                  breaks=seq(0, 1, by =0.10)) 


# Scatter plot----

#plot I
peace_and_attackes$ intifada <- factor(peace_and_attackes$intifada)

p6 <- ggplot(peace_and_attackes) +
  geom_point(aes(x = total_attacks, y = q3,color =intifada))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  ggtitle("Peace_and_attackes") +
  labs(x = "Number of attacks", y = "Peace Index")



#plot II      
http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html

fill = c("steelblue", "yellowgreen", "violetred1")

p6 <- ggplot(peace_and_attackes, aes(x = total_attacks, y = q3,size = total_killed, fill = intifada)) +
  geom_point(shape = 21) +
  ggtitle("Peace and Attackes") +
  labs(x = "Number of attacks", y = "Peace Index",
       size = " total_killed", fill = "intifada") +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  scale_size(range = c(1, 10)) +
  scale_fill_manual(values = fill) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))

#plot III    
library(ggthemes)

p6 <- ggplot(peace_and_attackes, aes(x = total_attacks, y = q3,size = total_killed, fill = intifada)) +
  theme_economist() +
  scale_fill_economist() +
  geom_point(shape = 21) +
  ggtitle("AAA") +
  labs(x = "attacks", y = "Q3)",
       size = "killed", fill = "Intifada") +
  scale_x_continuous(breaks = seq(1, 12, 5)) +
  scale_size(range = c(1, 10)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))
# Bar plot------

https://www.learnbyexample.org/r-bar-plot-ggplot2/
attach(Agriculture_18_19)

g1 <- ggplot(Agriculture_18_19, 
       aes(x=name_of_crop, y=mean(cult_area_under_crop)*0.0339, fill=name_of_crop)) + 
  geom_bar(stat="identity",width=0.8)+
  theme_calc()+
 ggtitle("Crop Frequency") +
  xlab("Crop") +
  ylab("Cultiveted Area (in ha)")+
  geom_text(aes(label=cult_area_under_crop), vjust=-0.3, size=2.5)


g2 <- ggplot(Agriculture_18_19,
             aes(x=name_of_crop, y=mean(cult_area_under_crop)*0.0339, fill=year)) + 
  geom_bar(stat="identity")


g3 <- ggplot(Agriculture_18_19,
             aes(x=name_of_crop, y=cult_area_under_crop, fill=year)) + 
  geom_bar(stat="identity", position=position_stack())

  scale_alpha_datetimefill_brewer(palette="YIGn")
  
  # Box-whisker Plot-----  
boxplot(Agriculture_18_19$cult_area_under_crop)

boxplot(cult_area_under_crop ~ year, data = Agriculture_18_19)

boxplot(cult_area_under_crop ~ TreatmentControl*year, las = 1,
        data = Agriculture_18_19,col = c("dodgerblue1", "olivedrab2"))


# Tabes----
install.packages("reactable")
https://glin.github.io/reactable/index.html

# kableExtra----
https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

library(knitr)
library(kableExtra)

df1 %>%
  kable() %>%
  kable_styling()
 

kable(df1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>% 
  add_header_above(c(" "=3, "Lans Patterns" = 3, "Intencity" = 2)) %>% 
  column_spec(4:7, bold = T) %>%
  row_spec(3:4, bold = T, color = "white", background = "#D7261E")

kable(df1[,2:8]) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>% 
  add_header_above(c(" "=2, "Lans Patterns" = 3, "Intencity" = 2)) %>%   
  pack_rows("Before SPIP",1,2,label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("After SPIP", 3, 4)


kable(df1) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


# formattable----
formattable(df3)

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

formattable(df3, 
            align =c("l","c","c","c", "r"), 
            list(year = formatter(
              "span", style = ~ style(color = "lightblue",font.weight = "bold")),
              average_hr_per_ha= color_tile(customGreen, customGreen0),
              total_irrigate_hr= color_tile(customGreen, customGreen0),
              N= color_bar(customRed)
            ))

# Combining tables---- 
# Combining tables with tha same columns-----

https://www.displayr.com/how-to-combine-complicated-tables-in-displayr-using-r/
  
library(abind)
new.table = abind(table.satisfaction, table.delay, table.favorite, along = 1) 
dimnames(new.table)[[3]] = c("Score","Sample Size")










# rmarkdown::----
