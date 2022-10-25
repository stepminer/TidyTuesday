---
 title: "Great British Bakeoff"
date: 2022-10-25
 
---
 
 # TidyTuesday
 
 
library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(scales)
library(ggtext)
 

# Load the weekly Data

 

bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')


# Wrangle


data_new<- bakers%>% select(baker_full,percent_episodes_appeared,first_date_appeared,last_date_appeared, total_episodes_appeared)|>
 pivot_longer(cols = 3:4,
                 names_to = "time_frame",
                 values_to = "value") 

data_new1<-data_new %>% 
arrange(desc(total_episodes_appeared)) %>% 
 top_n(n=10,total_episodes_appeared )%>%
 na.omit()
 
data_new1<- data_new1 %>% 
  dplyr::mutate(paired =rep(1:(n()/2), each=2),
        baker_full=factor(baker_full)) #PAIR FOR DUMBELL DOES NOT WORK

#ADD PAIRS MANUALLY
add <- tribble(
    ~pair,
    "1",
    "1",
    "2",
    "2",
    "3",
    "3",
    "4",
    "4",
    "5",
    "5",
    "6",
    "6",
    "7",
    "7",
    "8",
    "8",
    "9",
    "9",
    "10",
    "10",
    "11",
    "11",
    "12",
    "12",
     "13",
    "13",
    "14",
    "14",
    "15",
    "15",
    "16",
    "16",
    "17",
    "17",
    "18",
    "18",
    "19",
    "19"
    )

   data_new1<-data_new1  %>% 
   cbind(add)

data_new1<- data_new1 %>% 
  mutate(baker_full=factor(baker_full))
# Visualize

   ## Adding Google Fonts
  sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
  
  font <- "teko"
  
  
  # Colours for the dumbbells
  
  chart_colours <- c("first_date_appeared" = "#5B9374", "last_date_appeared" = "#C34A4A")
  
  # With the ggtext package we can use html to colour the text in the title
  
  subtitle_html <- glue::glue(
   "These contenders all appeared in 10 episodes or 100%. 
   <br>
   <span style='color:{chart_colours[1]};'>first date appeared</span>
   compared to the
   <span style='color:{chart_colours[2]};'>last date appeared</span>.
   <br>
   
    "
  )
  
  p <- data_new1 |>  mutate(baker_full = fct_reorder(baker_full, total_episodes_appeared)) %>% 
   ggplot(aes(x = value, y =  baker_full)) +
   
   # Add the line
   
   geom_line(aes(group = pair), colour = "white") +
   
   # Add the points on the dumbbells
   
   geom_point(aes(colour = time_frame), size = 5) +
   scale_colour_manual(values = chart_colours) +
    
   labs(
    x = "Date",
    y = "",
    title = "Top contenders in The Great British Bake Off",
    subtitle = subtitle_html,
    caption = "Source: bakeoff package from Alison Hill, Chester Ismay, and Richard Iannone | Viz: @stepminer2") +
   theme_bw() +
   theme(text = element_text(family = font),
         panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "none",
         #panel.grid.major.x = element_line(colour = "#253139"),
         axis.text.x = element_text(size = 24, colour = "white"),
         axis.text.y = element_text(size = 24, colour = "white"),
         axis.title.x = element_text(size = 24,
                                     margin = margin(15, 0, 0, 0),
                                     colour = "white"),
         axis.ticks = element_blank(),
         plot.title = element_text(size = 60,
                                       margin = margin(15, 0, 10, 9),
                                       face = "bold",
                                       colour = "white"),
         plot.title.position = "plot",
         
         # Allows us to use html to colour the subtitle text
         
         plot.subtitle = element_markdown(size = 40,
                                          margin = margin(0, 0, 20, 9),
                                          colour = "white",
                                          lineheight = 1.1),
         plot.caption = element_text(margin = margin(15, 0, 0, 5),
                                     size = 20,
                                     colour= "white"),
         plot.background = element_rect(fill = "#2E3D47"),
         panel.background = element_rect(fill = "#2E3D47")) 
   
   
  p
  ggsave("bakers_dumbell.png",p,
         width = 10.65,
         height = 6.375,
         dpi = 550)