
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

https://stackoverflow.com/questions/45407391/r-split-delimited-strings-in-a-column-and-insert-as-new-column-in-binary

netflix.rotten.tomatoes.metacritic.imdb <- read.csv("~/master_research/DATAs/netflix-rotten-tomatoes-metacritic-imdb.csv")

netflix <- netflix.rotten.tomatoes.metacritic.imdb[1:20,c(1,2,7)]

names(netflix01)

netflix01 <- 
  netflix %>% 
  mutate(Genre = strsplit(as.character(Genre), ",")) %>% 
  mutate(Country.Availability = strsplit(as.character(Country.Availability), ",")) %>% 
  unnest(Genre) %>% 
  unnest(Country.Availability) %>% 
  group_by(Genre, Country.Availability) %>% 
  count() %>% 
  ggplot(aes(x = Genre, y = n , fill = Country.Availability )) +
  geom_bar(stat = "identity") 


netflix <- netflix.rotten.tomatoes.metacritic.imdb[,c(1,2,7)]

nnetflix <- netflix %>% 
  mutate(Genre = strsplit(as.character(Genre), ",")) %>% 
  mutate(Country.Availability = strsplit(as.character(Country.Availability), ",")) %>% 
  unnest(Genre) %>% 
  unnest(Country.Availability) %>% 
  group_by(Genre, Country.Availability) %>% 
  count() %>% 
  ggplot(aes(x = Genre, y = n , fill = Country.Availability )) +
  geom_bar(stat = "identity")+ 
  theme_minimal()

