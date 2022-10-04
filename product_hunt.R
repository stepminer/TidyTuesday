# TidyTuesday challenge
# Week : 40
# Date : 2022-10-04
# Product Hunt
# Author: @stepminer2


# Load packages ----

library(tidyverse)
library(showtext)
library(ggdark)
library(ggtext)

# Import fonts ----
font_add_google("Merriweather", "titleFont")
font_add_google("Lato", "bodyFont")
showtext_auto()


# Import dataset ---- 
 
tuesdata <- tidytuesdayR::tt_load(2022, week = 40)

product_hunt <- tuesdata$product_hunt

product_hunt<- product_hunt %>% 
 janitor::clean_names()

# Explore Data ----

product_hunt %>% DataExplorer::create_report()

product_hunt %>% skimr::skim()

# Data wrangling ----

product_hunt1<- product_hunt %>%
 select(name,hunter,makers,product_of_the_day_date,
        product_ranking,upvotes)%>% 
filter(product_ranking==1)%>% 
 arrange(desc(upvotes))%>% 
 top_n(upvotes, n=10)


#Plot-------
p1<- product_hunt1 %>% 
 mutate(name = fct_reorder(name, upvotes)) %>%
 ggplot( aes(x=name, y=upvotes)) +
 geom_segment( aes(x=name, xend=name, y=0, yend=upvotes), color="skyblue") +
 geom_point( color="red", size=6, alpha=2) +
 geom_text(aes(label = upvotes), size = 16, hjust = 1.5, family = "bodyFont") + 

 dark_theme_classic (base_size = 24) +
 coord_flip() +
 
 theme(
  panel.grid.major.y = element_blank(),
  panel.border = element_blank(),
  axis.ticks.y = element_blank(),
        legend.position = "none",
   # Axis
      axis.ticks = element_line(colour = "gray60", arrow = arrow(length = unit(0.01,"inches"), ends = "last", angle = 20,type = "closed")),
   axis.text = element_text(family = "bodyFont", size = 30, colour = "gray60", face = "italic"),
   # Grid
   panel.grid = element_blank(),
   
   plot.title = element_textbox_simple(family = "titleFont", size = 70, colour = "white", face = "bold", margin = margin(15,0,0,0)),
   # Subtitle
   plot.subtitle = element_textbox_simple(family = "bodyFont", size = 50, lineheight = 0.4, colour = "white",hjust = 0.5, margin = margin(5,0,15,0)),
   # Caption
   plot.caption = element_textbox_simple(family = "bodyFont",size = 20, lineheight = 0.5, face = "bold", colour = "white", maxwidth = unit(8.4,"in"), margin = margin(0,0,10,0))
  )+
 labs(title="Top 10 products released on Product Hunt",
      subtitle="based on ranking and upvotes",
      caption = "Data:components.one by way of Data is Plural.<br>Visualization by @stepminer2",
      size = "upvotes"
 )+
 ylab("UPVOTES")+
 xlab("PRODUCTS") 

p1


ggsave("app_product_votes.png",
       plot = p1, width = 8.5, height = 8.5, units = 'in',
       dpi = 300)

