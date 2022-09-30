# US MAP of ARTITS
 ################################################################################
# author: @STEPMINER2
# purpose: tidytuesday, week 39
################################################################################



library(tidyverse)
library(readxl)
library(fs)
library(sf)
library(showtext)
 

tuesdata <- tidytuesdayR::tt_load(2022, week = 39)

artists <- tuesdata$artists


artists1 <- artists %>% 
 
 group_by(state, race)%>%
summarise(share= sum(artists_share, na.rm = T))%>%
ungroup()


# loading hexgrid from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
hex <- st_read("00_data/us_states_hexgrid.geojson")


# state abbreviations
states <- tibble(name = state.name, abbr = state.abb)
states <- rbind(states,
                tibble(name = "District of Columbia", abbr = "DC"))

# create dataset for map

artists1 <- artists1 %>% 
  filter(state %in% states$name) # continential US only

artists1 <- artists1 %>% 
 left_join( states, by = c("state" = "name"))
 
# configure hex grid with labels
  hex <- hex %>%
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))
  
  hex <- st_transform(hex, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  hex <- left_join(hex, states, by = c("google_name" = "name"))

  hex <- left_join(hex, artists1 , by = c("abbr" = "abbr"))
  
  # labels at center of hexagons
  centroids <- st_centroid(hex)
  
  centroids <-  centroids %>%
    mutate(long = unlist(map(centroids$geometry, 1)),
           lat = unlist(map(centroids$geometry, 2)))
  
  centroids <- left_join(centroids, artists1, 
                         by = c("abbr" = "abbr"))

  
str(hex)  
# create gif

  ggplot(hex) + 
    geom_sf(aes(fill = share)) + 
   # geom_text(data = centroids, aes(x = long, y = lat, label = abbr),
   #           color = "white", size = 2) +
    scale_fill_gradient(low = "white",
                        high = "red",
                       # breaks = c(1000, 2000, 3000),
                        #limits = c(1000, 3400),
                        #labels = scales::dollar_format(),
                        name = "Proportion") +
   facet_wrap(~race,  ncol = 2) +
    labs(title = "Proportion of artists in the US workforce by race",
         subtitle = "African-American and hispanics represent 0.06% of the \nworkforce each while Asians and white represent 0.13% each",
         caption = "Visualization by @steminer2 | #tidytuesday week 39 | Data from the arts.gov by way of Data is Plural") +
    ggthemes::theme_map() +
    theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center",
          legend.key.height = unit(0.5, 'cm'),
          legend.key.width= unit(1.6, 'cm'),
          legend.title = element_text(size = 8, color = "white"),
         strip.text = element_text(
                              size = 12, color = "black"),
           plot.title = element_text(hjust = 0.5, size = 18, 
                                    face = "bold", color = "white"),
          legend.background = element_rect(fill = "black"),
          legend.text = element_text(color = "white"),
          plot.subtitle = element_text(hjust = 0.5, size = 12, 
                                       lineheight = 2, color = "white"),
          plot.caption = element_text( color = "white", 
                                      lineheight = 2, size = 10, hjust = 0.5),
          plot.background = element_rect(fill = "black")) +
    guides(fill = guide_colourbar(ticks = FALSE, title.position = "top")) 
   

# save plot

ggsave("US_artists.png", width = 8, height = 12, units = "in", dpi=320) 


 artists %>% 
group_by(race)%>% 
summarise(s1=(sum(artists_n, na.rm=T)/sum(all_workers_n, na.rm=T)*100))










all_xl <- fs::dir_ls("2022/2022-09-27/ADP-31-artists-in-the-workforce-StateTables/")

all_xl |> 
  str_subset("AllArtists", negate = TRUE)

test_df <- all_xl[2] |> read_xlsx()
test_df |> glimpse()

all_xl

names(test_df)[1] |> 
  str_extract(art_pattern) |> 
  str_to_title()

read_and_clean <- function(file){
  
  raw_full <- read_excel(file)
  
  art_pattern <- "(?<=Number of ).+(?= in the U.S. labor force, for all the states and Puerto Rico: 2015-2019)" 
  
  art_type <- names(raw_full)[1] |> 
    str_extract(art_pattern) |> 
    str_to_title()
  
  races <- c("Hispanic", "White", "African-American", "Asian", "Other")
  
  race_data <- function(sheet, race){
    read_excel(file, sheet = sheet, skip = 3) |> 
      mutate(race = race, .after = 1) |> 
      slice(c(-1,-2)) |> 
      filter(!is.na(State)) |> 
      mutate(across(3:6, as.numeric)) |> 
      select(1:6) |> 
      set_names(
        nm = c(
          "state", "race", "all_workers_n", "artists_n", "artists_share", "location_quotient")
        ) |> 
      mutate(type = art_type, .before = 3)
  }
  
  map2_dfr(2:6, races, race_data)
  
}

test_all <- all_xl[2] |> 
  read_and_clean()

test_all |> glimpse()

all_df <- map_dfr(all_xl[2:length(all_xl)], read_and_clean)

all_df |> 
  glimpse()

all_df |> 
  write_csv("2022/2022-09-27/artists.csv")