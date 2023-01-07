# FRANCAIS SMAPE BY MONTH QTR OR WEEK FUNCTION

#loadlibraries
library(tidyverse)
 library(readxl)

#load data
data<-read_xlsx("00_data/HTI_xrate_ACTUAL22.xlsx") %>% 
 janitor::clean_names()

data2<- data %>% select(date, forecast, actual ) %>% 
 na.omit() %>% 
 mutate(forecast= as.numeric(forecast))


 library(dplyr)
smape2 <- function(a, f) 
    {

    return(1/length(a) * sum(2*abs(f-a) / (abs(a)+abs(f))*100))
}

data2 %>%
   group_by(date) %>% 
     summarise(smape = smape2(actual, forecast), .groups = 'drop')-> data3 

library(ggdark)
# Horizontal version
ggplot(data3, aes(x=date, y=smape)) +
 geom_segment( aes(x=date, xend=date, y=0, yend=smape), color="skyblue") +
 geom_point( color="red", size=6, alpha=0.6) +
 coord_flip() +
 dark_theme_classic()+
 theme(
  panel.grid.major.y = element_blank(),
  panel.border = element_blank(),
  axis.ticks.y = element_blank()
 )+
 labs(title="Précision des prévisions pour le taux de change de la Gourde",
      subtitle="par date : la taille est l' erreur en %",
      caption = "Data: BRH | Analysis: Pat Stephenson @stepminer2",
      size = "smape"
 )+
 ylab("---------------------SMAPE %-------------------")+
 xlab("---------------------DATE---------------------")

