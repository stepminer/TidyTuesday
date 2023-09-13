
#TidyTuesday - Week 37
 #  by  Pat Stephenson (@stepminer2)

# Load packages -
      
  library(tidyverse)  
  library(dplyr)
  library(tidygeocoder)
  library(countrycode)
  library(ggvoronoi)
  library(maps)
 library(tidytuesdayR)

# Load the weekly Data  
 tt <- tt_load("2023-09-12")
 
 all_countries <- tt$all_countries
 
 # explore
 
 skimr::skim(all_countries )
 
# Data Wrangle 
        
      food<- all_countries %>% 
      filter(Category == 'Food provision') %>% 
       group_by(Category,country_iso3)%>% 
      summarize(hoursPerDayCombined = sum(hoursPerDayCombined) )
  
        food_geocoded <-  food %>% 
      
    mutate(name = countrycode(country_iso3 ,
                          origin = "iso3c",destination = "country.name") ) %>% 
     geocode(address = name)     
      
 toplot   <-  food_geocoded %>%
      select(name, long, lat, hoursPerDayCombined)%>%
      rename( hours = hoursPerDayCombined)
      
#mypalette<- brewer.pal(name = "Spectral", n=11)    
  
# display.brewer.all(type="seq")    
#min(toplot$lat)
# max(toplot$lat)
 
#VISUALIZE 
       
 toplot %>% 
   distinct(long, lat, .keep_all = TRUE) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_voronoi(aes(fill = hours), size = 0.25, color = "#3B454A") +
  
 #coord_map("ortho")+
 coord_map("ortho", orientation = c(41, -74, 0)) +       
  labs(x = NULL,
       y = NULL,
      title = "The Human Chronome Project",
 subtitle = "How people across the world spend their time on Food?",
 caption = "Data The Human Chronome Project | Viz: @stepminer2") +
  
   scale_fill_viridis_c(option = "plasma", direction = -1) +  
  
   theme_minimal() +
  theme(plot.title = element_text(size=40, face = "bold"),
           plot.caption = element_text(size=20,hjust=0),
           plot.subtitle = element_text(size=25, hjust=0,lineheight = .5 ),
      axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
       # plot.caption = element_markdown(),
        legend.key.height = unit(2, "mm"))
 
 ggsave("chronome_tree_voronoi2.png",
         width = 14 ,
         height = 11,
         dpi = 300)      
      
 