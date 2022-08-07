#HAITI DISASTERS , TIMELINE



library(tidyverse)
library(viridis)
library(ggrepel)
library(ggdark)
library(ggplot2)
library(scales)
library(lubridate)
library(ggblanket)
library(ggdark)
library(sysfonts)
library(showtext)

font_add_google("Gugi", "gugi")

showtext_auto()




HURR<- read.csv("00_data/HTI_disasters.csv")
HURR<- HURR%>% rename(date= ï..date ) %>% 
 mutate(date = dmy(date)) %>% 
 mutate(year=  year(date)) %>% 
 na.omit()


firsts <- HURR
 
# focusing on first achievements by events
event_firsts <- firsts  
  
# Create "positions" for the text so they don't overlap. This is where they will
# sit on the y-axis in each direction (negative and positive direction)
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

#create a dataset with positions/directions for each year and assign to each 
# accomplishment
line_pos <- data.frame(
 "year"=unique(event_firsts$year),
 "position"=rep(positions, length.out=length(unique(event_firsts$year))),
 "direction"=rep(directions, length.out=length(unique(event_firsts$year))))

event_firsts_working <- merge(x=event_firsts, y=line_pos, by="year", all = TRUE)
event_firsts_working <- event_firsts_working[with(event_firsts_working, order(year, location_name)), ]

text_offset <- 0.05
event_firsts_working$year_count <- ave(event_firsts_working$year==event_firsts_working$year, event_firsts_working$year, FUN=cumsum)
event_firsts_working$text_position <- (event_firsts_working$year_count * text_offset * event_firsts_working$direction) + event_firsts_working$position

year_buffer <- 2

year_date_range <- seq(min(event_firsts$year) - 2, max(event_firsts$year) + 1, by = 2)
year_df <- data.frame(year_date_range)

#Too many accomplishments to label, so i just subset the ones im most interested in

event_firsts_working<- event_firsts_working %>% mutate (event= as.factor(event))
#plot
timeline_plot<-ggplot(event_firsts_working, aes(x=year, y = 0, col= event)) +
 geom_point(aes(y = 0), size=2) +
 labs(col="event", title = "Haiti Disasters Timeline",
      caption = "Data:from wikipedia| Viz: @stepminer2") +
 dark_theme_classic(base_size = 20) + 
 geom_hline(yintercept=0, color = "white", size=0.3) +
 geom_segment(data=event_firsts_working[event_firsts_working$year_count == 1,], aes(y=position,yend=0,xend=year), color='white', size=0.2) +
 theme(text = element_text(size = 16.5),
       axis.line.y=element_blank(),
       axis.text.y=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank(),
       axis.ticks.y=element_blank(),
       axis.text.x =element_blank(),
       axis.ticks.x =element_blank(),
       axis.line.x =element_blank(),
       legend.title = element_blank(),
       plot.title = element_text(hjust = 0.5, size = 40, family = "gugi"),
       legend.position = "none") +
 geom_text(data=year_df, aes(x=year_date_range,y=-0.1, label = year_date_range),size=6.5,vjust=0.5, color='white', angle=90) + 
# geom_text(data = event_firsts_working, aes(y=text_position,label=event),size=4.5)+
 ggrepel::geom_text_repel(max.overlaps = 100,
                          data =  event_firsts_working,
                          aes(y = text_position, label = event),
                          size = 6.5
                          )
# ggrepel::geom_text_repel(
# data =  event_firsts_working,
# aes(y = text_position, label = event),
#  color = "grey10",
# size = 4.5,
# nudge_x = -90,
# hjust = 0.5,
# segment.size = 0.7,
# segment.curvature = -0.7,
# segment.angle = 65,
# segment.square = TRUE,
# segment.inflect = TRUE,
# family = "Fira Mono",
# min.segment.length = 0.1,
# box.padding = unit(0.45, "lines"))

timeline_plot    

ggsave("HTI_disasters3.png",timeline_plot, width = 5, height = 6.5, dpi = 320, device = ragg::agg_png) 
