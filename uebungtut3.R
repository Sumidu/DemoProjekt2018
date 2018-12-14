## AnalyseScript Übung Boxplot

library(tidyverse)

diamondslibrary(ggplot2)

ggplot(data = diamonds) +
  aes(x = carat, y = carat) +
  geom_boxplot() +
  labs(title = 'Most diamonds are small',
    x = 'Carat',
    y = 'Distribution',
    caption = 'Dots are outliers',
    subtitle = 'Boxplot of carat of all diamonds') +
  theme_minimal() +
  coord_flip() +
  NULL

psych::describe(diamonds)

library(ggplot2)

ggplot(data = diamonds) +
  aes(x = depth, y = depth) +
  geom_boxplot(fill = '#f7fbff') +
  labs(title = 'Depths of Diamond Cuts are normally distributed',
    x = 'Depths',
    y = 'Distribution',
    caption = 'Dots are outliers',
    subtitle = 'Boxplot of depths') +
  theme_gray() +
  coord_flip() +
  NULL


library(ggplot2)

ggplot(data = diamonds) +
  aes(x = color, y = carat) +
  geom_boxplot() +
  labs(title = 'Depths of Diamond Cuts are normally distributed',
       x = 'Color',
       y = 'Distribution',
       caption = 'Dots are outliers',
       subtitle = 'Boxplot of depths') +
  theme_gray() +
  coord_flip() +
  NULL


