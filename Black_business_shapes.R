# Reproduction of Dubois's Black Business men shapes


library(tidyverse)
library(ggforce)
library(grid) 

#Intuitively generate the rectangle coordinates 
rectangle1 <- data.frame(x = c(2.5, 2.5, 4.5, 4.5), y = c(0.3, 2.3, 2.3, 0.3))
rectangle3 <- data.frame(x = c(2.5, 2.5, 4.8, 4.8), y = c(2.6, 3.6, 3.6, 2.6))
rectangle4 <- data.frame(x = c(2.5, 2.5, 4.5, 4.5), y = c(3.8, 5.6, 5.6, 3.8))
rectangle5 <- data.frame(x = c(4.85, 4.85, 10.85, 10.85), y = c(2.2, 8.2, 8.2, 2.2))
rectangle6 <- data.frame(x = c(4.85, 4.85, 10.85, 10.85), y = c(8.5, 10.25, 10.25, 8.5))
  
rectangle7 <- data.frame(x = c(6.625, 6.625, 8.375 , 8.375), y = c(0, 1.75, 1.75, 0))
rectangle8 <- data.frame(x = c(4.85, 4.85, 6.2 , 6.2), y = c(-0.2, 0.4, 0.4, -0.2))
rectangle9 <- data.frame(x = c(3, 3, 3.5, 3.5), y = c(5.7, 6.2, 6.2, 5.7))

# Draw ggplot2 plot with multiple RECTANGLES
 ggplot() +  
 
geom_shape(data= rectangle1, aes(x, y),fill = "forestgreen", colour = 'grey50')+
geom_shape(data= rectangle3, aes(x, y),fill = "black", colour = 'grey50')+
geom_shape(data= rectangle4, aes(x, y),fill = "blue", colour = 'grey50')+
geom_shape(data= rectangle5, aes(x, y),fill = "gold", colour = 'grey50')+  
geom_shape(data= rectangle6, aes(x, y),fill = "forestgreen", colour = 'grey50')+    
geom_shape(data= rectangle7, aes(x, y),fill = "brown", colour = 'grey50')+    
geom_shape(data= rectangle8, aes(x, y),fill = "#CA3D55", colour = 'grey50')+      
geom_shape(data= rectangle9, aes(x, y),fill = "#CA3D55", colour = 'grey50')+      
  
        lims(x = c(0, 14), y = c(-0.20, 14)) +
 theme_void()

ggsave("Dubois_shapes.png", plot = last_plot(), width = 6, height = 6, dpi = 300, bg = "#ffffff")