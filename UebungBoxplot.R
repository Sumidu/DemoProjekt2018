## Analyseskript

library(tidyverse)



ggplot(data = diamonds) +
  aes(x = carat, y = carat) +
  geom_boxplot() +
  labs(title = 'Most diamonts are small',
    x = 'Carat',
    y = 'Distribution',
    caption = 'Dots are outliers',
    subtitle = 'Boxplot of carat of all diamons') +
  coord_flip() +
  theme_minimal()


ggplot(data = diamonds) +
  aes(x = depth, y = depth) +
  geom_boxplot() +
  labs(title = 'Depth of Diamond cuts are normally distributed',
    x = 'Depth',
    y = 'Distribution',
    caption = 'Dots are outliers',
    subtitle = 'Boxplot of depths') +
  theme_minimal() + coord_flip()

ggplot(data = diamonds) +
  aes(x = color, y = carat) +
  geom_boxplot() +
  labs(title = 'Depth of Diamond cuts are normally distributed',
       x = 'Color',
       y = 'Distribution',
       caption = 'Dots are outliers',
       subtitle = 'Boxplot of depths') +
  theme_minimal() + coord_flip()


psych::describe(diamonds)



