library(tidyverse)
library(tidytuesdayR)
 
# Load data

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")
 
 # CUMULATIVE SENTIMENTS ANALYSIS  
 
#load packages

library(tidyverse)
library(epubr)
library(tidytext)
library(zoo)
library(showtext)
library(ggtext)
library(glue)
library(dplyr)
library(tokenizers)
library(tm)            # Text mining cleaning
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs 
library(tidytext)    #  


# wrangle data

df <- dialogue  %>% 
 
 mutate(index= row_number())

df1<-df%>%
 filter(season==1)

df1 <- tokenize_sentences(df1$dialogue) %>% 
 unlist()

sentences<- as_tibble(df1)  

newdf<- sentences %>%drop_na()



#sentiments

scored <- newdf %>%
 unnest_tokens(word, value) %>%
 left_join(sentiments) %>%
 left_join(get_sentiments("afinn")) %>%
 mutate(sentiment = case_when(is.na(sentiment) ~ 0,
                              sentiment == "negative" ~ -1,
                              TRUE ~ 1),
        cumsent = cumsum(sentiment),
        ind = row_number(),
        rollm = rollmean(sentiment, 10, na.pad = TRUE),
        value = replace_na(value, 0),
        afinn_cum = cumsum(value))

#fonts
font_add_google("Quicksand", "Quicksand")

showtext_auto()


sec_text <- "grey50"
p_text <- "grey70"

#plot1
p1 <- scored %>%
 ggplot(aes(ind, cumsent)) +
 geom_area(fill = alpha("red", .6),
           color = "grey50") +
 geom_segment(x = 0, xend = 0, y = 10, yend = -10,
              arrow = arrow(length = unit(.15, "cm"), type = "closed"),
              color = sec_text) +
 
 geom_hline(yintercept = 0, linetype = 2, color = sec_text) +
 scale_x_continuous(expand = c(0,0)) +
 scale_y_continuous(breaks = c(0),
                    labels = c("Neutral\nSentiment")) +
 coord_cartesian(clip = "off") +
 theme(text = element_text(family = "Quicksand",size = 20, color = sec_text),
       panel.background = element_blank(),
       plot.background = element_rect(fill = "grey20", color = NA),
       axis.line.y = element_blank(),
       axis.line.x = element_line(arrow = arrow(length = unit(.15, "cm"), 
                                                type = "closed"),
                                  color = sec_text),
       panel.grid = element_blank(),
       axis.text.y = element_text(color = sec_text, face = "bold", size = 12),
       axis.text.x = element_blank(),
       plot.title.position = "plot",
       plot.title = element_textbox(face = "bold", size = 60, hjust = .5,
                                    color = p_text),
       plot.subtitle = element_textbox(face = "bold", size = 30, hjust = .5,
                                       color = p_text),                                      
       plot.caption =  element_text(hjust = 0.03, color = alpha(p_text, .5),
                                    family = "Quicksand", margin = margin(t = -5)),
       plot.caption.position = "plot") +

 
 
 
  labs(title = glue("Sentiment analysis of <span style='color:{alpha('red', .75)}'>Stranger Things</span>"),
      subtitle = glue("Season 1",
                      " "),
      x = "Flow Progression",
      y = "",
      caption = "\nData:8flix.com via Dan Fellowes & Jonathan Kitt | Viz: @stepminer2")
p1
############################################
df2<-df%>%
 filter(season==2)


df2 <- tokenize_sentences(df2$dialogue) %>% 
 unlist()

sentences<- as_tibble(df2)  


newdf<- sentences %>%drop_na()

scored <- newdf %>%
 unnest_tokens(word, value) %>%
 left_join(sentiments) %>%
 left_join(get_sentiments("afinn")) %>%
 mutate(sentiment = case_when(is.na(sentiment) ~ 0,
                              sentiment == "negative" ~ -1,
                              TRUE ~ 1),
        cumsent = cumsum(sentiment),
        ind = row_number(),
        rollm = rollmean(sentiment, 10, na.pad = TRUE),
        value = replace_na(value, 0),
        afinn_cum = cumsum(value))


font_add_google("Quicksand", "Quicksand")

showtext_auto()


sec_text <- "grey50"
p_text <- "grey70"

#plot2
p2 <- scored %>%
 ggplot(aes(ind, cumsent)) +
 geom_area(fill = alpha("red", .6),
           color = "grey50") +
 geom_segment(x = 0, xend = 0, y = 10, yend = -10,
              arrow = arrow(length = unit(.15, "cm"), type = "closed"),
              color = sec_text) +
 
 geom_hline(yintercept = 0, linetype = 2, color = sec_text) +
 scale_x_continuous(expand = c(0,0)) +
 scale_y_continuous(breaks = c(0),
                    labels = c("Neutral\nSentiment")) +
 coord_cartesian(clip = "off") +
 theme(text = element_text(family = "Quicksand",size = 20, color = sec_text),
       panel.background = element_blank(),
       plot.background = element_rect(fill = "grey20", color = NA),
       axis.line.y = element_blank(),
       axis.line.x = element_line(arrow = arrow(length = unit(.15, "cm"), 
                                                type = "closed"),
                                  color = sec_text),
       panel.grid = element_blank(),
       axis.text.y = element_text(color = sec_text, face = "bold", size = 12),
       axis.text.x = element_blank(),
       plot.title.position = "plot",
       plot.title = element_textbox(face = "bold", size = 60, hjust = .5,
                                    color = p_text),
       plot.subtitle = element_textbox(face = "bold", size = 30, hjust = .5,
                                       color = p_text),                                      
       plot.caption =  element_text(hjust = 0.03, color = alpha(p_text, .5),
                                    family = "Quicksand", margin = margin(t = -5)),
       plot.caption.position = "plot") +
 
 
 
 
  labs(title = glue("Sentiment analysis of <span style='color:{alpha('red', .75)}'>Stranger Things</span>"),
      subtitle = glue("Season 2",
                      " "),
      x = "Flow Progression",
      y = "",
      caption = "\nData:8flix.com via Dan Fellowes & Jonathan Kitt | Viz: @stepminer2")
 
p2

##########################################
df3<-df%>%
 filter(season==3)

df3 <- tokenize_sentences(df3$dialogue) %>% 
 unlist()

sentences<- as_tibble(df3)  
 


newdf<- sentences %>%drop_na()




scored <- newdf %>%
 unnest_tokens(word, value) %>%
 left_join(sentiments) %>%
 left_join(get_sentiments("afinn")) %>%
 mutate(sentiment = case_when(is.na(sentiment) ~ 0,
                              sentiment == "negative" ~ -1,
                              TRUE ~ 1),
        cumsent = cumsum(sentiment),
        ind = row_number(),
        rollm = rollmean(sentiment, 10, na.pad = TRUE),
        value = replace_na(value, 0),
        afinn_cum = cumsum(value))


font_add_google("Quicksand", "Quicksand")

showtext_auto()


sec_text <- "grey50"
p_text <- "grey70"

#plot3
p3 <- scored %>%
 ggplot(aes(ind, cumsent)) +
 geom_area(fill = alpha("red", .6),
           color = "grey50") +
 geom_segment(x = 0, xend = 0, y = 10, yend = -10,
              arrow = arrow(length = unit(.15, "cm"), type = "closed"),
              color = sec_text) +
 
 geom_hline(yintercept = 0, linetype = 2, color = sec_text) +
 scale_x_continuous(expand = c(0,0)) +
 scale_y_continuous(breaks = c(0),
                    labels = c("Neutral\nSentiment")) +
 coord_cartesian(clip = "off") +
 theme(text = element_text(family = "Quicksand",size = 20, color = sec_text),
       panel.background = element_blank(),
       plot.background = element_rect(fill = "grey20", color = NA),
       axis.line.y = element_blank(),
       axis.line.x = element_line(arrow = arrow(length = unit(.15, "cm"), 
                                                type = "closed"),
                                  color = sec_text),
       panel.grid = element_blank(),
       axis.text.y = element_text(color = sec_text, face = "bold", size = 12),
       axis.text.x = element_blank(),
       plot.title.position = "plot",
       plot.title = element_textbox(face = "bold", size = 60, hjust = .5,
                                    color = p_text),
       plot.subtitle = element_textbox(face = "bold", size = 30, hjust = .5,
                                       color = p_text),                                      
       plot.caption =  element_text(hjust = 0.03, color = alpha(p_text, .5),
                                    family = "Quicksand", margin = margin(t = -5)),
       plot.caption.position = "plot") +
 
 
 
 labs(title = glue("Sentiment analysis of <span style='color:{alpha('red', .75)}'>Stranger Things</span>"),
      subtitle = glue("Season 3",
                      " "),
      x = "Flow Progression",
      y = "",
      caption = "\nData:8flix.com via Dan Fellowes & Jonathan Kitt | Viz: @stepminer2")
 
p3
============================================
 df4<-df%>%
 filter(season==4)

df4 <- tokenize_sentences(df4$dialogue) %>% 
 unlist()

sentences<- as_tibble(df4)  
# rename(text=value)



newdf<- sentences %>%drop_na()




scored <- newdf %>%
 unnest_tokens(word, value) %>%
 left_join(sentiments) %>%
 left_join(get_sentiments("afinn")) %>%
 mutate(sentiment = case_when(is.na(sentiment) ~ 0,
                              sentiment == "negative" ~ -1,
                              TRUE ~ 1),
        cumsent = cumsum(sentiment),
        ind = row_number(),
        rollm = rollmean(sentiment, 10, na.pad = TRUE),
        value = replace_na(value, 0),
        afinn_cum = cumsum(value))


font_add_google("Quicksand", "Quicksand")

showtext_auto()


sec_text <- "grey50"
p_text <- "grey70"

#plot4
p4 <- scored %>%
 ggplot(aes(ind, cumsent)) +
 geom_area(fill = alpha("red", .6),
           color = "grey50") +
 geom_segment(x = 0, xend = 0, y = 10, yend = -10,
              arrow = arrow(length = unit(.15, "cm"), type = "closed"),
              color = sec_text) +
  
 geom_hline(yintercept = 0, linetype = 2, color = sec_text) +
 scale_x_continuous(expand = c(0,0)) +
 scale_y_continuous(breaks = c(0),
                    labels = c("Neutral\nSentiment")) +
 coord_cartesian(clip = "off") +
 theme(text = element_text(family = "Quicksand",size = 20, color = sec_text),
       panel.background = element_blank(),
       plot.background = element_rect(fill = "grey20", color = NA),
       axis.line.y = element_blank(),
       axis.line.x = element_line(arrow = arrow(length = unit(.15, "cm"), 
                                                type = "closed"),
                                  color = sec_text),
       panel.grid = element_blank(),
       axis.text.y = element_text(color = sec_text, face = "bold", size = 12),
       axis.text.x = element_blank(),
       plot.title.position = "plot",
       plot.title = element_textbox(face = "bold", size = 60, hjust = .5,
                                    color = p_text),
       plot.subtitle = element_textbox(face = "bold", size = 30, hjust = .5,
                                       color = p_text),                                      
       plot.caption =  element_text(hjust = 0.03, color = alpha(p_text, .5),
                                    family = "Quicksand", margin = margin(t = -5)),
       plot.caption.position = "plot") +
 
 
 
 labs(title = glue("Sentiment analysis of <span style='color:{alpha('red', .75)}'>Stranger Things</span>"),
      subtitle = glue("Season 4",
                      " "),
      x = "Flow Progression",
      y = "",
      caption = "\nData:8flix.com via Dan Fellowes & Jonathan Kitt | Viz: @stepminer2")
 
p4







library(cowplot)
# manually setting the number of rows, auto-generate upper-case labels
plot_grid(p1, p2,p3,p4,
          nrow = 2,
          labels = "AUTO",
          label_size = 12,
          align = "v"
)

ggsave("Cum_sentiments_strangerthings1.png", device= png, dpi = 320, width = 18, height = 11)