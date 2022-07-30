#VACCINES IN THE CARIBBEAN IN TIME REVISITED
# Week 29 
library(dplyr)
library(ggplot2)
library(countrycode)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(tidyverse)
library(ggdark)

font_add_google("Merriweather", "titleFont")
font_add_google("Lato", "bodyFont")
showtext_auto()
# LOAD DATA
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')
data <- data %>%
 group_sector = group,
rename(category_sector = category,
       variable_name = variable,
       date_year = year) %>%
 select(iso3c, variable_name,
        category_sector, group_sector, date_year, value)


glimpse(data)
# Filter date and variables of interest
# CARIBEAN countries
countries <- c("Cuba", "Dominican Republic", "Haiti","Jamaica")
data_countries_iso3c <- map_data("world") %>%
 tibble() %>%
 mutate(iso3c = countrycode(region,
                            destination = "iso3c",
                            origin = "country.name")) %>%
 select(region, iso3c) %>%
 filter(region %in% countries) %>%
 distinct()
countries_iso3c <- data_countries_iso3c %>%
 pull(iso3c)

# Filter by countries and date + add their country names
data_01 <- data %>%
 filter(iso3c %in% countries_iso3c,
        date_year >= 1990) %>%
 mutate(country_name = countrycode(iso3c,
                                   destination = "country.name",
                                   origin = "iso3c"))

glimpse(data_01)
# Focused on VACCINES
data_02 <- data_01 %>%
 filter(category_sector=="Vaccines") %>%
 select(iso3c, variable_name, date_year, category_sector,value)

data_02 <- data_02 %>%
 mutate(country = countrycode(iso3c,
                              destination = "country.name",
                              origin = "iso3c"))


# DATA VISUALIZATION
#'s convert country as a grouping variable
data_02$country <- as.factor(data_02$country) 
data_02$variable_name <- as.factor(data_02$variable_name) 

data_02<- data_02 %>% 
 rename(coverage= value,
        year= date_year,
        vaccine= variable_name)


ggplot(data_02, aes(x = year, y = coverage)) +
 geom_point(aes(color = vaccine), alpha = 0.5)  +
 scale_size(range = c(1, 5)) + # Adjust the range of points size
 facet_wrap(~country,ncol = 2) +
 theme_set(dark_theme_classic(base_size = 50) )+
 
 # Title
 #plot.title.position = "plot",
 theme(legend.position = "top",
       plot.title = element_textbox_simple(family = "titleFont", size = 70, colour = "white", face = "bold", margin = margin(15,0,0,0)),
       # Subtitle
       plot.subtitle = element_textbox_simple(family = "bodyFont", size = 50, lineheight = 0.4, colour = "white", margin = margin(5,0,15,0)),
       # Caption
       plot.caption = element_textbox_simple(family = "bodyFont",size = 20, lineheight = 0.5, face = "bold", colour = "white", maxwidth = unit(8.4,"in"), margin = margin(0,0,10,0))
 )+
 labs(title="Vaccination coverage in the Caribbean",
      subtitle="All vaccines coverage in % from 1990 to 2019",
      caption = "Data:Technology Adoption<br>Visualization by @stepminer2<br>Data: Charles Kenny and George Yang. 2022. _Technology and Development: An Exploration of the Data._ CGD Working Paper 617. Washington, DC: Center for Global Development",
      size = "coverage"
 )+
 ylab("% COVERAGE")+
 xlab("YEAR") 

ggsave("carib_VACCINE3.png",
       width = 10, height = 14, units = 'in',
       dpi = 300)

