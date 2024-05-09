# Get the world polygon and extract UK
library(maps)
UK <- map_data("world") %>% filter(region=="UK")
UKn <- map_data("world") %>% filter(region=="India")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc=="UK")
data <- world.cities %>% filter(country.etc=="India")

# Left chart
ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() + ylim(8,34) + coord_map() 
#  theme_void() + ylim(20,25) + coord_map() 

# Second graphic with names of the 10 biggest cities
library(ggrepel)
ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group),
               fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  theme_void() + ylim(8,34) + coord_map() 


ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), 
                   aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(8,34) + coord_map() +
  theme(legend.position="none")

--------------------------------------------------------------------------------
  
# virids package for the color palette
library(viridis)

# Left: use size and color
ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(8,34) + coord_map() 

# Center: reorder your dataset first! Big cities appear later = on top
data %>%
  arrange(pop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(8,34) + coord_map() + 
  theme(legend.position="none")

# Right: just use arrange(desc(pop)) instead
data %>%
  arrange(desc(pop)) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group),
               fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(8,34) + coord_map() + 
  theme(legend.position="none")


--------------------------------------------------------------------------------

mybreaks <- c(0.02, 0.04, 0.08, 1, 7)

# Reorder data to show biggest cities on top
data <- data %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) 

# Build the map
map %>%
  ggplot() +
  geom_polygon(data = UKn, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop),
               shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in M)" ) +
  theme_void() + ylim(8,34) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in the UK") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )





----------------------------------------------------------
  library(maps)
library(ggplot2)
data <- read.csv("foo.csv")
ind <- map(database = "world", regions = "india", exact = FALSE, boundary = T)
india <- map_data(ind , region = "india", exact = F)

(ggplot(aes(x=x, y=y, fill=z), data=data) + 
    geom_tile()) + geom_polygon(data=india, 
                                aes(x=long, y=lat, group=group), 
                                colour="black", fill="red", alpha=0)




states_map <- map_data("state")
fee_map <- merge(states_map, spendstate, by.x = "region", by.y = "state")
fee_map <- arrange(fee_map, group, order) # eeed to sort polygons in right order












