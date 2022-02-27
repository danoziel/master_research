library(tidyverse)
library(extrafont)
#---- pai----
count.data <- data.frame(
  class = c("Support Learning", "Do Not\nsupport learning"),
  n = c(9, 20),
  prop = c(31.04, 68.96)
)
count.data

count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.data

mycols <- c("#0073C2FF",  "#CD534CFF")

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  theme(text = element_text(family = "serif"))

# ----

specie <- c( "Skills","Skills", "Vocabulary","Vocabulary")
condition <- c("Support\nLearning", "Do Not Support\nLearning","Support\nLearning", "Do Not Support\nLearning")
value <- c(31.03,68.96,62.06,37.93)
value <- c(31,69,62,38)
value <- c(9,20,18,11)

data <- 
  data.frame(specie,condition,value) %>% 
  group_by(specie) %>%
  mutate(label_y = cumsum(value))


ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")+
  geom_text(aes(label = value))

ggplot(data, aes(x = specie, y = value, fill = condition)) +
  geom_col() +
#  geom_text(aes(y = label_y, label = value), vjust = 1.5, colour = "white")+
  geom_text(aes(y = label_y, label = paste(format(value), "%")), size = 4,vjust = 1.5)+
  labs(x=" ",y="Percentage of Sample")+
  scale_fill_brewer(palette = "Pastel2")+
  scale_fill_manual(values = mycols) +
  theme_minimal()+
  theme(text = element_text(family = "serif"))
