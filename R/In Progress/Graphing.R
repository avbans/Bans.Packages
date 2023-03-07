#THIS PACKAGE IS FOR CREATING MY CUSTOM GGPLOT AESTHETIC

#USE TIDY VERSE
library(tidyverse)

#DEMO WITH MT CARS DATASET
cars <- mtcars

#BASE PLOT (MY INSPIRATION)
plot(cars$hp,cars$mpg,
     pch = 16,
     cex = 1.75,
     col = "forestgreen",
     type = "p",
     xlab = "Horse Power",
     ylab = "Miles per Gallon",
     main = "Horsepower vs. Miles per Gallon")

#CREATING THE THEME
theme_alex <- function(){
    theme_bw()+
    theme(text = element_text(size = 14, face = "bold", family = "Helvetica"),
          axis.text = element_text(face = "plain"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          aspect.ratio = 1)
  }

#TESTING THE THEME
ggplot(cars,aes(hp,mpg))+
  geom_point(size = 3, color = "forestgreen")+
  scale_x_continuous(breaks = scales::breaks_width(50))+
  labs(title = "Horsepower vs. Miles per Gallon",
       x = "Horse Power",
       y = "Miles per Gallon")+
  theme_alex()

#TESTING THE THEME 2
ggplot(cars,aes(mpg))+
  geom_histogram(bins = 5, fill = "red", color = "black")+
  labs(title = "Miles per Gallon")+
  theme_alex()
