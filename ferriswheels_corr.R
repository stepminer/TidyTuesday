## Challenge: #TidyTuesday 2022 week 32
## Data:      frogs
## Author:    @stepminer2
## Date:      2022-08-08



#install packages()
remotes::install_github("emilhvitfeldt/ferriswheels")

## 1. Load packages ---- 
library(pacman) 
p_load(tidyverse, tidytuesdayR, ggtext, showtext, MetBrewer)
p_load(janitor, skimr, ferriswheels,visdat,naniar,simputation)
 
## 2. Read in the data ----
glimpse(wheels) 

# EXPLORE
DataExplorer::create_report(wheels)

wheels<- wheels
#tOO MUCH MISSING DATA LETS SEE IF WE CAN IMPUTE SOME
# Label if mag is missing
wheels1<- wheels %>%  add_label_missings(height) %>%
 impute_lm( height ~ diameter)%>%
 add_label_missings(diameter) %>%
 impute_lm( diameter ~ height)%>%
 add_label_missings(seating_capacity) %>%
 impute_rf( seating_capacity ~ hourly_capacity) %>%
 select(name,height,diameter,seating_capacity,any_missing) %>% 
 
 rename(capacity= seating_capacity) %>% 
 na.omit() %>% 
 mutate(capacity=log10(abs(capacity)))
 
library(corrmorant)

data_cormoran <- wheels1 %>% select(-any_missing)
  
corrmorant(data_cormoran, style = "blue_red") +
 #theme_dark() +
 labs(title = "Correlations among ferriswheels") +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1)
 )
 