#TIDYTUESDAY TECHNOLOGY  ADOPTION OF PESTICIDES
library(tidyverse)
library(lubridate)
library(showtext)
library(ggdark)
library(extrafont)
library(countrycode)
library(ggtext)

choose_font(c("Kontrapunkt Miki" ,"GillSans",  "Verdana", "sans"), quiet =TRUE)
# You can download the data here: https://www.drought.gov/states/Wisconsin

data <- tidytuesdayR::tt_load("2022-07-19")
head(data)


G7<-  c("Canada", "United States","Germany","France","Italy", "United Kingdom","Japan" )
countryname(G7)

GG7<- countryname(G7, destination = 'iso3c')


G7_world<- data$technology %>% filter (iso3c  %in% GG7) %>% 
 filter(category=="Agriculture"& group== "Production") %>% 
 filter(variable=="pest_total") %>% 
 filter(year >= 2018) %>% 
 mutate(country = countrycode(iso3c,
                              destination = "country.name",
                              origin = "iso3c")) %>% 
 select(country, iso3c,year, value) %>% 
 arrange(desc(value))

#get flags

library(stringr)
data1_url <- G7_world%>%
 mutate(flag_url = case_when(
  str_detect(country,"United States") ~ "https://flagpedia.net/data/flags/w702/us.png",
  str_detect(country,"Canada") ~ "https://flagpedia.net/data/flags/w702/ca.png",
  str_detect(country,"France") ~ "https://flagpedia.net/data/flags/w702/fr.png",
  str_detect(country, "Italy") ~ "https://flagpedia.net/data/flags/w702/it.png",
  str_detect(country,"Japan") ~ "https://flagpedia.net/data/flags/w702/jp.png",
  str_detect(country,"Germany") ~ "https://flagpedia.net/data/flags/w702/de.png",
  str_detect(country,"United Kingdom") ~ "https://flagpedia.net/data/flags/w702/gb.png"
  
 )) 

# Add use per unit land area
intensity<- tibble::tribble(
 ~kg_per_ha,
 1.02,
 1.56,
 2.96,
 4.35,
 11.82,
 2.71,
 1.09 )


data1_url<- data1_url %>% 
 cbind(intensity) %>% 
 
 select(country, flag_url, year, value, kg_per_ha)

# Load {gt}
library(gt)
library(gtExtras)
# Make table with gt()

tab2<- data1_url%>%
 gt()%>%
 tab_header(
  title = ("Pesticide Use in the G7 Countries in 2018"),
  subtitle = md("Data: TidyTuesday | Table **@stepminer2**" ))%>%
 gtExtras::gt_theme_nytimes()


tab2<-tab2 %>% 
 gt()%>%
 
 # Plot the table
 tab2<- data1_url%>%
 gt()%>%
 tab_header(
  title = ("Tons of Pesticide Use in the G7 Countries in 2018"),
  subtitle = md("Data: TidyTuesday | Table **@stepminer2**" ))%>%
 gtExtras::gt_theme_nytimes() %>%
 gtExtras::gt_highlight_rows(
  # Row to highlight
  rows = 1 ,
  # Background color
  fill = "skyblue",
  # Bold for target column only
  bold_target_only = TRUE,
  # Select target column
  target_col = value)%>%
 # Add flag images
 gtExtras::gt_img_rows(columns = flag_url, height = 20) %>% 

### Colors the kg-per -ha column
gtExtras::gt_color_rows(
 columns = "kg_per_ha",
 palette = "ggsci::blue_material",
 type = "continuous"
)  

tab2