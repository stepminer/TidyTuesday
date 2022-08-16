

# 2022-08-16
# TidyTuesday week 33, 
# Data: Open-Source Psychometrics Project courtesy: Tanya Shapiro

# Load libraries
library(tidyverse)
library(DataExplorer) 
library(highcharter)
library(cowplot)
library(magick)
# Load data
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psych_stats <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv")

#explore
characters %>% na.omit %>% create_report()
skimr::skim(characters)
 

# wrangle

s_questions <- psych_stats %>% #select the 4 questions
 count(question) %>% 
 mutate(id=row_number()) %>%
 filter(id %in% c(138,172,187,204))

show_chr<- characters %>% 
 filter(uni_name=="Star Trek: The Next Generation")

trek_stats<- psych_stats %>% 
 filter(uni_name=="Star Trek: The Next Generation", 
        question %in% s_questions$question) %>%
 left_join(show_chr %>% select(id, image_link), by=c("char_id"="id")) %>%
 select(char_name, question, personality) %>%
 pivot_wider(names_from = question, values_from = personality) %>%
 arrange(char_name) 

#PLOT

p <- hchart(data_to_sankey(trek_stats), "sankey", name = "traits")



#ADD TITLES
p %>%
 hc_title(text= "Star Trek: The Next Generation characters personality types") %>%
 hc_subtitle(text= "The U.S.S. Enterprise-D, under the command of Captain Jean-Luc Picard, housed many different personality types")  %>%
 hc_credits(enabled = TRUE, text = "Data Source: Open-Source Psychometrics")%>%
 hc_caption(text = "Visualization: stepminer2 | #TidyTuesday week 33")%>%
 hc_plotOptions(series = list(dataLabels = list(style = list(fontSize = "12px")))) %>%
 hc_add_theme(hc_theme_elementary())-> p1

ggsave("star_trek_personality.png",  p1, w = 12, h = 10)


# ADD PICTURES

images<- c("https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/1.jpg",
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/2.jpg", 
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/3.jpg",
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/4.jpg", 
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/5.jpg",
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/6.jpg", 
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/7.jpg",
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/8.jpg", 
           "https://openpsychometrics.org/tests/characters/test-resources/pics/TNG/9.jpg"
)

img <- magick::image_read(images) %>%
 magick::image_montage(tile = '3') %>% 
 image_resize("570x380") %>%
 image_colorize(45, "white")

#COMBINE IMAGES

img2<- magick::image_read("star_trek_personality.png") %>%
 #magick::image_montage(tile = '3') %>% 
 image_resize("570x380") 

ggdraw() + 
 draw_image(img2)+
 draw_image(
  img, x = 1, y = 0.81, hjust = 1, vjust = 1, halign = 1, valign = 1,
  width = 0.15
 )

# SAVE FINAL PLOT
ggsave("star_trek_personalities2.png",  w = 12, h = 10)

