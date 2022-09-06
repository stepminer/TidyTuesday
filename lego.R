 ################################################################################
 author: Patrick Stephenson (@stepminer2)
 purpose: tidytuesday, week of 09-06-2022
################################################################################


library(tidyverse)
library(tidyquant)
library(showtext)

# add fonts
font_add_google(name = "Grandstander", family = "Grandstander")
font_add_google(name = "Bebas Neue", family = "Bebas Neue")
showtext_auto()


inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

all_df <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num") 


#EXPLORE

DataExplorer::create_report(all_df)

skimr::skim(all_df)
all_df %>% arrange(desc(num_parts))


df<- all_df %>% 
# mutate(num_parts_log= log10(num_parts))%>% 
 select(name, year,num_parts)%>%
 group_by(year)%>%
summarise(av = mean(num_parts+1 )) %>%
ungroup()
          
df%>% 
    # Setup canvas with year (x-axis) and sales (y-axis)
    ggplot(aes(x = year, y = av)) +
    
    # Geometries
    geom_point(size = 0.5, color = "#2c3e50") +
  #  geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    theme_tq(base_family ="Bebas Neue", base_size = 30 ) +
 theme(
  plot.title = element_text(family = "Bebas Neue", size = 80, colour = "black",
                            margin = margin(t = 20, b = 10)),
  plot.subtitle = element_text(family = "Bebas Neue", size = 60, colour = "black",
                               margin = margin(b = 20)),
  plot.caption = element_text(color = "black", size = 24, hjust = 0.5)
   )+
 #   scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "LEGO MOCs Mean Number of parts by Year",
        subtitle = "There seems to exist an Upward trend",
        x = "",
        y = "Number of parts",
      caption = "Visualization: stepminer2 | Data: rebrickable courtesy of Georgios Karamanis" 
    )

ggsave("lego.png", width = 8.5, height = 7, units = "in", dpi=320)

