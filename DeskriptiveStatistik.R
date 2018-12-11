## Deskriptive Auswertung des bereinigten Datensatzes

library(tidyverse)
library(ggthemes)

data_robot <- readRDS("data/robo_pflege.rds")

psych::describe(data_robot) 


data_robot %>% 
  filter(gender != "keine Angabe") %>% 
  filter(!is.na(job_type)) %>% 
  ggplot() +
  aes(x = age, fill = job_type) +
  geom_histogram(bins = 20) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = 'Studentische Stichprobe',
       x = 'Alter (in Jahren)',
       y = 'Häufigkeit (absolute)',
       fill = "Berufsstatus",
       caption = 'Histogramm mit 20 Bins',
       subtitle = 'Histogramm des Alters nach Geschlecht und Berufsstatus') +
  theme_bw() +
  facet_wrap(vars(gender))

ggsave("alter_histogramm.pdf", width = 7, height = 4)


data_robot %>% ggplot() +
  aes(x = age, y = kut, color = gender) +
  geom_point() +
  labs(title = "Junge Teilnehmer haben eine höhere Technik-Kontrollüberzeugung",
       subtitle = "Scatterplot von KUT und Alter",
       x = "Alter",
       y = "Kontrollüberzeugung im Umgang mit Technik",
       color = "Geschlecht",
       caption = "") +
  ylim(c(0,6)) +
  NULL -> p_kutalter
p_kutalter

breite <- 15
hoehe <- breite * 0.681

ggsave(plot = p_kutalter, "alter_kut_scatter.pdf", width = breite, height = hoehe, units = "cm")





data_robot %>% group_by(gender) %>% summarise(mean_age = mean(age), sd_age = sd(age)) %>%
  ggplot() +
  aes(x = gender, fill = gender,  weight = mean_age, ymin = mean_age - sd_age, ymax = mean_age + sd_age) +
  geom_bar() +
  geom_errorbar(width = 0.5) +
  theme_minimal() + 
  labs(title = "Titel", 
       subtitle = "Was für ein Diagramm",
       caption = "Fehlerbalken bezeichnen die Standardabweichung",
       x = "Geschlecht",
       y = "Alter (in Jahren)",
       fill = "Geschlecht") +
  NULL

  