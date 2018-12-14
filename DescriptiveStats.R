library(tidyverse)
data <- readRDS("data/robotik2.rds")

library(ggplot2)

ggplot(data = data) +
  aes(x = gender, y = age) +
  geom_boxplot(fill = '#0c4c8a') +
  theme_minimal()


t.test(data$TROST_HUMAN, mu=3.5)


# Bibliotheken laden

library(tidyverse)
library(psych)

# Liste alle Verfügbaren Datensätze
data()

# Hilfe zu ausgewähltem Datensatz
?Tal_Or

# Datensatz laden
data("Tal.Or")
Tal_Or
