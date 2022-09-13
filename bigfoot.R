# TidyTuesday challenge
# Date : 2022-09-13
# Bigfoot

# Load packages ----
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(viridis) 

# Import dataset ----
tuesdata <- tidytuesdayR::tt_load(2022, week = 37)

bigfoot <- tuesdata$bigfoot

# Data wrangling ----
bigfoot_c<- bigfoot %>% group_by(county, season) %>% 
 summarize(n= n()) %>% 
 arrange(desc(n)) 

bigfoot_c<- bigfoot_c %>%
left_join(bigfoot, by = "county") %>% 
 select(county, season.x, state, latitude, longitude, n )%>%
 na.omit() %>% 
top_n(n=10, n )

bigfoot_c<- bigfoot_c %>%
 filter(n >10)
#bigtop<- bigfoot_c %>%
#filter( n >= 27) 


# map data ----
world <- map_data("world")

    world1<-world %>% filter(region=="USA")

     
    
    
 #plot-----------------
    ggplot(bigfoot_c, aes(longitude, latitude)) + #, label = county
     geom_map(
      data = world1,
      map = world1,
      aes(map_id = region),
      color = "black",
      fill = "lightgray",
      show.legend = FALSE,
      inherit.aes = FALSE
     ) +
     geom_point(aes(longitude, latitude, size = n, color = n)) +
    # geom_text_repel(box.padding = 0.5, max.overlaps = Inf)+
          scale_color_viridis() +
     geom_segment(data = bigfoot_c,
              mapping = aes(x = -121, y = 47, xend = -117, yend = 48),
              colour = alpha("#4a75b0", 0.5), size = 0.05) +
    # geom_point(data= haiti_cities, aes(long, lat), inherit.aes = FALSE,color="darkgrey" ) +
     theme_void()+ coord_quickmap()+
     labs(title = "Top counties with > 10 Bigfoot sightings/season",
          subtitle = "Pierce County, Washington had the largest number ",
          caption = "Data:BFRO by way of Data.World | Viz: @stepminer2"
     ) +
     theme(text = element_text(color="black",size=20,family="Public Sans Medium"),
           plot.title = element_text(size=25, face = "bold"),
           plot.caption = element_text(size=12,hjust=0),
           plot.subtitle = element_text(hjust=0),
           #panel.grid.major.x = element_blank(),
           #panel.border = element_blank(),
           #axis.ticks.x = element_blank(),
           #axis.ticks.y = element_blank(),
           #axis.text.y = element_blank(),
           #axis.text.x = element_blank(),
           #axis.title = element_blank(),
           #panel.grid = element_blank(),
           legend.position = "right"
           #      plot.background = element_rect(fill="black",color="black"),
           #      panel.background = element_rect(fill="black",color="black")
     ) +
     geom_text(data = data.frame(x = -110.3667448054, y = 48.0011243106616, label = "Pierce County, Washington"),
mapping = aes(x = x, y = y, label = label),
inherit.aes = FALSE)

       ggsave("bigfoot.png",
           dpi=320,
           width = 12,
           height = 7)  