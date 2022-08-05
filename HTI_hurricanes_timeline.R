#HAITI HURRICANES , TIMELINE



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



#load data
HURR<- read.csv("00_data/hurricanes.csv")

#clean data
HURR<- HURR%>% rename(nameyear= ï..name_year ) %>% 
 mutate(year=  parse_number(nameyear)) %>% 
 na.omit()

HURR<- HURR[-1,]
HURR <- HURR %>%
 arrange(year) %>% 
 filter( year >= 1954)

HURR <- HURR %>%
 extract( nameyear, c("FirstName", "LastName"), "([^ ]+) (.*)") 

HURR<- HURR[-29,]

HURR1 <- HURR %>%
 mutate(LastName= str_remove_all(LastName, "[0-9]+")) %>% 
 #   An alternative is to swap out all non-alphanumeric characters.
 mutate (LastName =str_replace_all(LastName, "[^[:alnum:]]", " "))


# prepare data for plotting
firsts <- HURR1
first$event <- first$LastName
 
hurr_firsts <- firsts %>% 
 rename(event = LastName)
# Create "positions" for the text so they don't overlap.  
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

#create a dataset with positions/directions for each year and assign to each 
# event
line_pos <- data.frame(
 "year"=unique(hurr_firsts$year),
 "position"=rep(positions, length.out=length(unique(hurr_firsts$year))),
 "direction"=rep(directions, length.out=length(unique(hurr_firsts$year))))

hurr_firsts_working <- merge(x=hurr_firsts, y=line_pos, by="year", all = TRUE)
hurr_firsts_working <- hurr_firsts_working[with(hurr_firsts_working, order(year, location_name)), ]

text_offset <- 0.05
hurr_firsts_working$year_count <- ave(hurr_firsts_working$year==hurr_firsts_working$year, hurr_firsts_working$year, FUN=cumsum)
hurr_firsts_working$text_position <- (hurr_firsts_working$year_count * text_offset * hurr_firsts_working$direction) + hurr_firsts_working$position

year_buffer <- 2

year_date_range <- seq(min(hurr_firsts$year) - 2, max(hurr_firsts$year) + 1, by = 10)
year_df <- data.frame(year_date_range)

# mutate event as factor

hurr_firsts_working<- hurr_firsts_working %>% mutate (event= as.factor(event))

#plot
timeline_plot<-ggplot(hurr_firsts_working, aes(x=year, y = 0, col= event, label=event)) +
 geom_point(aes(y = 0), size=2) +
 labs(col="event", title = "Haiti Hurricanes Timeline",
      caption = "Data:from wikipedia| Viz: @stepminer2") +
 dark_theme_classic(base_size = 20) + 
 geom_hline(yintercept=0, color = "white", size=0.3) +
 geom_segment(data=hurr_firsts_working[hurr_firsts_working$year_count == 1,], aes(y=position,yend=0,xend=year), color='white', size=0.2) +
 theme(text = element_text(size = 12.5),
       axis.line.y=element_blank(),
       axis.text.y=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank(),
       axis.ticks.y=element_blank(),
       axis.text.x =element_blank(),
       axis.ticks.x =element_blank(),
       axis.line.x =element_blank(),
       legend.title = element_blank(),
       plot.title = element_text(hjust = 0.5, size = 30, family = "gugi"),
       legend.position = "none") +
 geom_text(data=year_df, aes(x=year_date_range,y=-0.1, label = year_date_range),size=4.5,vjust=0.5, color='white', angle=90) + 
 geom_text(data = hurr_firsts_working, aes(y=text_position,label=event),size=4.5)+
 ggrepel::geom_text_repel(max.overlaps = 100)
# ggrepel::geom_text_repel(
# data =  hurr_firsts_working,
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

ggsave("HTI_hurricanes3.png",timeline_plot, width = 5, height = 6.5, dpi = 320, device = ragg::agg_png) 
