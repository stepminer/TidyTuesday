#TOP5 COUNTRIES WITH CLEANEST ENERGY SOURCES WITH OLYMPIC RINGS
library(tidyverse)
library(dplyr)
library(countrycode)
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

df <- 
 technology |> 
 filter(
  stringr::str_detect(label, "Electricity|electric energy")
 ) |> filter(year >= 2020) 

df1<- df %>% 
 pivot_wider(names_from = variable, values_from = value) %>% 
 replace(is.na(.), 0) %>% 
 group_by(iso3c) %>% 
 summarise(proportion = sum(elec_hydro+elec_wind+elec_solar+elec_renew_other)/elecprod)%>%
 filter(!proportion ==  "Inf")

df2<-df2 %>% 
 mutate(country_name = countrycode(iso3c,
                                   destination = "country.name",
                                   origin = "iso3c")) %>% 
 mutate(ymin = 1 - proportion,
        ymax =  proportion  )


library(patchwork)
library(showtext)
library(tidytuesdayR)
library(tidyverse)


library(datapasta)
library(tidyverse)

showtext.auto()


# Plots ----

europe_ring <- df2 %>% 
 filter(country_name == "Costa Rica") %>% 
 ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax)) +
 geom_rect(show.legend = FALSE, fill = "#0081C8") +
 coord_polar(theta = "y") +
 xlim(c(0.05, 4)) +
 theme_void() +
 annotate("text", x = 0.05, y = 0, size = 15, label = "Costa Rica\n99%", family = "space",
          colour = "#0081C8")

africa_ring <- df2 %>% 
 filter(country_name == "Norway") %>% 
 ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax)) +
 geom_rect(show.legend = FALSE, fill = "#000000") +
 coord_polar(theta = "y") +
 xlim(c(0.05, 4)) +
 theme_void() +
 annotate("text", x = 0.05, y = 0, size = 15, label = "Norway\n98%", family = "space",
          colour = "#000000")

americas_ring <- df2 %>% 
 filter(country_name == "Uruguay") %>% 
 ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax)) +
 geom_rect(show.legend = FALSE, fill = "#EE334E") +
 coord_polar(theta = "y") +
 xlim(c(0.05, 4)) +
 theme_void() +
 annotate("text", x = 0.05, y = 0, size = 15, label = "Uruguay\n94%", family = "space",
          colour = "#EE334E")

asia_ring <- df2%>% 
 filter(country_name == "Tajikistan") %>% 
 ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax)) +
 geom_rect(show.legend = FALSE, fill = "#FCB131") +
 coord_polar(theta = "y") +
 xlim(c(0.05, 4)) +
 theme_void() +
 annotate("text", x = 0.05, y = 0, size = 15, label = "Tajikistan\n93%", family = "space",
          colour = "#FCB131")

oceania_ring <- df2 %>% 
 filter(country_name == "Brazil") %>% 
 ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax )) +
 geom_rect(show.legend = FALSE, fill = "#00A651") +
 coord_polar(theta = "y") +
 xlim(c(0.05, 4)) +
 theme_void() +
 annotate("text", x = 0.05, y = 0, size = 15, label = "Brazil\n84%", family = "space",
          colour = "#00A651")

p <- (europe_ring + africa_ring + americas_ring) / (asia_ring + oceania_ring) +
 plot_layout(widths = c(1, 1, 1, 1, 1)) +
 plot_annotation(title = "Top 5 Countries with cleanest Energy Sources 2020",
                 subtitle = "Proportion of Energy from Renewables vs Total",
                 caption = "Data : TidyTuesday | Viz : @stepminer2",
                 theme = theme(plot.title = element_text(hjust = 0.5, family = "space", size = 60),
                               plot.subtitle = element_text(hjust = 0.5, family = "space", size = 50),
                               plot.caption = element_text(hjust = 0.5, family = "space", size = 20)))

# Save plot ----
p
ggsave("olympic_electricity_2020.png", p, dpi = 320, width = 12, height = 6)