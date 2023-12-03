plot(model)

plot(week1ago[intifada==0] ,q3[intifada==0],col="blue",
     ylim = c(0,5),xlim = c(0,15))
points(week1ago[intifada==1] ,q3[intifada==1],col="red",pch = 16 )
abline(a=2.036, b=0.0026,col="blue",lwd=1)
abline(a=2.08, b=0.0026,col="red",lwd=1)

pairs(bind11[,2:7], pch = 19, upper.panel = NULL)

library(ggplot2)
library(tidyverse)
library(ggthemes)
library(extrafont)


# Scatter plot----

#plot I
peace_and_attackes$ intifada <- factor(peace_and_attackes$intifada)

p6 <- ggplot(peace_and_attackes) +
  geom_point(aes(x = total_attacks, y = q3,color =intifada))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  ggtitle("Peace_and_attackes") +
  labs(x = "Number of attacks", y = "Peace Index")
p6


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
# ------
library(extrafont)
tail_peace <- tail(peace_and_attackes,100)
p1 <- ggplot() + theme_bw() +
  geom_line(aes(y = q3, x = yearweek, colour = total_attacks), size=1.5, data = tail_peace,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x="Year", y="USD million") +
  ggtitle("Composition of Exports to China ($)") +
  scale_colour_manual(values=colour)