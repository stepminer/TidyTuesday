

# 2022-08-23
# TidyTuesday week 34 
# Data: TechPowerUp and WikiChip.org

# Load libraries

library(tidyverse)
library(janitor)
library(showtext)
library(lubridate)
library(DataExplorer)
library(brotools)
library(tidyquant)
library(ggdark) 
library(ggExtra)
library(showtext)

showtext_auto(enable = TRUE)
font_add_google("Bebas Neue", "Bebas Neue")



set.seed(123)


#### DATA ####
tuesdata <- tidytuesdayR::tt_load(2022, week = 34)

 chips<- tuesdata$chips
 
 # Time series cluster analysis of CHIPS
chips1 <-readr::read_csv("00_data/chip_dataset.csv") %>% 
 janitor::clean_names()

#Explore
skimr::skim(chips1 )


str(chips1)
#wrangle
chips1 <- chips1 %>%
    dplyr::filter(type == "CPU" )%>% 
    select("vendor","release_date","transistors_million")%>%

 mutate(release_date= ymd(release_date)) %>% 
 mutate(release_date= year(release_date))%>%
  na.omit()
 
 chips1<-chips1 %>% 
  group_by(release_date, vendor) %>% 
  summarise(transistors_million= mean(transistors_million)) %>% 
 
  chips_wide<- chips1 %>%  
  filter(release_date < 2020)  %>%  
    pivot_wider(names_from = release_date, values_from = transistors_million)   


 
wss <- map_dbl(1:1, ~{kmeans(select(chips_wide, -vendor), ., nstart=2,iter.max = 15 )$tot.withinss})
n_clust <- 1:2
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
ggplot(elbow_df) +
    geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
    theme_tq() 

clusters <- kmeans(select(chips_wide, -vendor), centers = 1)
(centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster"))

chips_wide <- chips_wide %>% 
    mutate(cluster = clusters$cluster)

chips_long <- chips_wide %>%
    pivot_longer(cols=c(-vendor, -cluster), names_to = "year", values_to = "value") 
view (chips_long)  

centers_long <- centers %>%
    pivot_longer(cols = -cluster, names_to = "year", values_to = "value")   
centers_long

font<- "Bebas Neue"

 library(ggannotate)


 
p1 <- ggplot() +
    geom_line(data = chips_long, aes(y =value, x = year, group = vendor), colour = "#FF3030") +
  
    geom_point(data = centers_long, aes(y = value, x = year, group = cluster), col = "#b58900", size = 1) +
 
 geom_text(data = data.frame(x = c(18.87 , 18.62  ),
y = c(3744.30 , 8386.58 ),
label = c("AMD", "Intel")),
mapping = aes(x = x, y = y, label = label),
size = 4.41, colour = "white", inherit.aes = FALSE)+

    dark_theme_classic() + 
    
    labs(title = "CPU Evolution of AMD & Intel chips 2000-2019", 
         subtitle = "CPU number of transistors has significantly increased in 2015. \nIntel leads the pack and has opened a Gap with AMD",
         caption = 'Viz: @stepminer2 | #TidyTuesday Week 34 | Data: TechPowerUp and WikiChip.org',
         y= "Million of Transistors") +
    theme(plot.caption = element_text(colour = "white"),
          plot.subtitle = element_text(hjust = 0.5, family = "font", size = 11),
          plot.title = element_text(hjust = 0.5, family = "font", size = 18, face = "bold")) +

 p1
 ggExtra::rotateTextX(45)

 ggannotate()
 
library(tidyverse) 
library(magick)
library(dplyr)
library(cowplot)

img1 <-     image_read("images/Intel.png")
img2 <-     image_read("images/AMD_29_29.png")


ggdraw() + 
draw_plot(p1) +    
    draw_image(
        img2 ,  x = 0, y = 0,  
        scale =    0.15  ) +

 draw_image(
        img1,  x = 0, y = 0.25,  
        scale =    0.15    )

# save
ggsave("CPU_chips2_F.png", 
       width = 7, height = 5, device = png, res = 320) 