#CARIBBEAN  ELECTRIC ENERGY PER UNIT POPULATION

library(tidyverse)
library(dplyr)
library(countrycode)
library(sysfonts)
library(showtext)

font_add_google("Gugi", "gugi")

showtext_auto()

# DATA
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

df <- 
 technology |> 
 filter(
  stringr::str_detect(label, "Electricity|electric energy")
 ) |> filter(year >= 2019) %>% 
 filter(iso3c %in%  c("BRB","DMA","SUR","BLZ","KNA","CUB","GRD","BHS","TTO","GUY","ATG","DOM","JAM","HTI","PRI"))

df<-df %>% 
 filter( variable=="elecprod")

#ADD POP DATA
pop<- tibble::tribble(
 ~ country, ~ popul,
 "Antigua and Barbuda",	97929,
 "Dominica"	,71986,
 "Saint Kitts and Nevis",	53199,
 "Bahamas",	393244,
 "St. Marteen",	38666,
 "Jamaica",	2961167,
 "Barbados",	287375,
 "Puerto Rico",	2860853,
 "Saint Lucia",	183627,
 "Trinidad and Tobago",	1399488,
 "Haiti",	11402528,
 "Cuba",	11326616,
 "Grenada",	112523,
 "Saint Vincent and the Grenadines",	110940,
 "Dominican Republic",	10847910) %>% 
 
 mutate(code = countrycode::countrycode(country,
                                        destination = "iso3c",
                                        origin = "country.name")) 

df1<- df%>%
 left_join(pop, by = c("iso3c" = "code")) %>% 
 na.omit() 

df1<- df1 %>% 
 mutate(elect_perunit= (value/popul)*1e9) %>% 
 mutate(elect_perunit= round(elect_perunit, digits = 0))

df1<- df1 %>%
 mutate(code = countrycode::countrycode(iso3c,
                                        destination = "iso2c",
                                        origin = "iso3c")) %>% 
 
 df1<- df1%>% select(country, iso3c, code, elect_perunit) %>% 
 mutate(country= as.factor(country))

str(df1)

#PLOT
library(ggimage)

df1 %>% mutate(country = fct_reorder(country, elect_perunit)) %>%  
 ggplot( aes(x=country, y=elect_perunit, group=country, fill = country)) +
 
 geom_chicklet(colour = "black",width = 1, radius = grid::unit(9, "pt")) +
 coord_flip() +
 geom_flag( y= 6000, size=0.1, aes(image = code)) + #ADD FLAG
 labs(title="Electricity Production in the Caribbean",
      subtitle="in KWH per Million Population",
      caption = "Data: TidyTuesday/Worldometer | Analysis: @stepminer2",
      size = "elect_perunit"
 )+
 ylab("KWH/Million")+
 xlab("Country") +
 dark_theme_classic(base_size = 30)+
 
 theme(
  legend.position = "none",
  legend.title = element_blank(),
  plot.title = element_text(size = 50, hjust = 0.5, color = "white", family ="gugi", face = "bold"),
  plot.subtitle = element_text(size = 40, hjust = 0.5, color = "white", family ="gugi")
 )

#SAVE
ggsave( filename = "elect_production_M.png", width = 6.5, height = 8.5, dpi = 320, device = ragg::agg_png) 


