#TAXONOMIC CLASSIFICATION OF HAITI ENDANGERED SPECIES
# 12.09.2022, Patrick Stephenson (@stepminer2)
# GPL-3.0 license


# load libraries

library(tidyverse)
library(taxize)

# load data --------------------------------------------------------------- 
 


species<- read_csv("https://raw.githubusercontent.com/stepminer/TidyTuesday/data/hti_biodiversity.csv")

names <- species$scientific_name

taxo<- tax_name(query = c(names), get = c("kingdom","phylum","class","order","family"), 
                                db = "ncbi")


taxo2<- taxo %>% select(-db,-query)

library(highcharter)

p <- hchart(data_to_sankey(taxo2), "sankey", name = "species")

p

#ADD TITLES
p %>%
 hc_title(text= "Critically Endangered species in Haiti") %>%
 hc_subtitle(text= "Out of the 66 listed species,there were 20 amphibians, 5 reptiles and 17 plants" )  %>%
 hc_credits(enabled = TRUE, text = "Data Source:  IUCN Red List | via : rainforests.mongabay.com")%>%
 hc_caption(text = "Visualization: stepminer2")%>%
 hc_plotOptions(series = list(dataLabels = list(style = list(fontSize = "12px")))) %>%
 hc_add_theme(hc_theme_darkunica())-> p1

 
p1
