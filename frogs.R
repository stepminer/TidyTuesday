## Challenge: #TidyTuesday 2022 week 31
## Data:      frogs
## Author:    @stepminer2
## Date:      2022-07-19


## 1. Load packages ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, skimr, highcharter)


## 2. Read in the data ----
tt <- tidytuesdayR::tt_load(2022, week = 31) 

frogs <- tt$frogs %>% clean_names()

# EXPLORE
DataExplorer::create_report(frogs)

# prepare data for sankey diagram 

frogs<- frogs %>% 
 select(hab_type, female, water, detection) %>% 
mutate(female = case_when(
 female == 1 ~ "Female",
 female == 0  ~ "Male")
 )

## 3. Sankey diagram ----
p <- hchart(data_to_sankey(frogs), "sankey", name = "frogs")
p

#ADD TITLES
p %>%
 hc_title(text= "Sankey Diagram for Oregon spotted frog (Rana pretiosa) habitat use") %>%
 hc_subtitle(text= "Majority of frogs reside in the reservoir, were female, prefer shallow water which is the site of captures")  %>%
 hc_credits(enabled = TRUE, text = "Data Source: U.S. Geological Survey")%>%
 hc_caption(text = "Visualization: stepminer2 | #TidyTuesday")%>%
 hc_plotOptions(series = list(dataLabels = list(style = list(fontSize = "12px")))) %>%
 hc_add_theme(hc_theme_elementary())-> p1

p1

## . save final figure ----

