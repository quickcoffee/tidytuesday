library(tidyverse)
library(gganimate)

# Get the Data
dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

# data cleaning
dog_descriptions$posted <- as.POSIXct(dog_descriptions$posted,format="%Y-%m-%dT%H:%M:%S") #string to datetime

#build data -> only 2018 data with top10 breeds
p <- dog_descriptions %>% 
  select(breed_primary, posted) %>%
  filter(year(posted)==2018) %>% 
  group_by(month=month(posted), breed_primary) %>%
  summarise(n=n()) %>%
  arrange(month, desc(n), breed_primary) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 10) %>% 
  filter(!is.na(month))

#create plot
plt <- p %>% 
  ggplot(aes(-rank,n, fill = breed_primary)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip() + 
  geom_text(aes(-rank,y=0,label = breed_primary,hjust=0)) +       #breed label
  geom_text(aes(-rank,y=n,label = as.character(n), hjust=-1)) + # value label
  theme_minimal() +
  theme(legend.position = "none")+
  transition_states(month,4,1)+
  labs(title = 'Top 10 dog breeds on Petfinder for {closest_state}-2018 (Month-Year)', y= 'Number of Dogs', x= 'Dog Breed')

#create gif/video
animate(plt, 100, fps = 25, duration = 30, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))
animate(plt, 100, fps = 25, duration = 15, width = 800, height = 600, renderer = av_renderer("video.mp4"))
  
  
  