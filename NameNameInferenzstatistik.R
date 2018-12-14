## NameNameInferenzstatistik.R

# source("install_libraries.R")

library(knitr) 
stitch("NameNameInferenzstatistik.R")
stitch(script="NameNameInferenzstatistik.R", system.file("misc", "knitr-template.Rhtml", package="knitr"))

library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotrix)
library(jmv)
data_robot <- readRDS("data/robo_pflege.rds")

psych::describeData(data_robot)
rwthcolor <- hcictools::rwth.colorpalette()

## Unverbundener T-Test:

data_robot %>% filter(gender != "keine Angabe") -> data4t_test
t.test(data4t_test$kut ~ data4t_test$gender)

t.test( filter(data_robot, gender == "männlich")$kut, filter(data_robot, gender == "weiblich")$kut)


# Balkendiagramm:

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  group_by(gender) %>% 
  summarise(mean_kut = mean(kut), sem_kut = std.error(kut)) %>%
  ggplot() +
  aes(x = gender, fill = gender, weight = mean_kut, ymin = mean_kut - sem_kut, ymax = mean_kut + sem_kut ) +
  geom_bar( width = 0.5) +
  scale_fill_manual(values=c(rwthcolor$blue, rwthcolor$red)) + 
  geom_errorbar(width = 0.2) +
  ylim(0,5) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Balkendiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [0 - 5]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("examplebar.png", width = 6, height = 6)

## Punktdiagramm:

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  group_by(gender) %>% 
  summarise(mean_kut = mean(kut), sem_kut = std.error(kut)) %>%
  ggplot() +
  aes(x = gender, y = mean_kut, colour = gender, ymin = mean_kut - sem_kut, ymax = mean_kut + sem_kut ) +
  geom_errorbar(width = 0.2, colour = rwthcolor$black) +
  geom_point(size = 3) +
  scale_colour_manual(values=c(rwthcolor$blue, rwthcolor$red)) + 
  ylim(2,5) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Punktdiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [1 - 6]",
       colour = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes. Y-Achse zur verbesserten Übersicht gekürzt.") +
  NULL

ggsave("examplepoint.png", width = 6, height = 6)

## Zweifaktorielle ANOVA

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  filter(job_type == "Arbeitnehmer/in" | job_type == "Studierend/Schüler/in" ) %>% 
  jmv::anova(dep = "kut", factors = c("job_type", "gender"), emMeans = list(c("job_type", "gender")) ) -> res

res$main 
res$emm

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  filter(job_type == "Arbeitnehmer/in" | job_type == "Studierend/Schüler/in" ) %>% 
  group_by(gender, job_type) %>% 
  summarise(mean_kut = mean(kut), sem_kut = std.error(kut)) %>%
  ggplot() +
  aes(x = job_type, y = mean_kut, colour = gender, weight = mean_kut, ymin = mean_kut - sem_kut, ymax = mean_kut + sem_kut ) +
  geom_errorbar(width = 0.2, colour = rwthcolor$black) +
  geom_point(size = 3) +
  geom_line(aes(group = gender)) +
  scale_colour_manual(values=c(rwthcolor$blue, rwthcolor$red)) + 
  ylim(3,6) +
  theme_gray() +
  guides(fill = guide_legend(title = "LEFT", title.position = "left")) +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können, Schüler und Studierende auch!", 
       subtitle = "Punktdiagramm: KUT im Vergleich zwischen ArbeitnehmerInnen und Studierenden, gruppiert nach Geschlecht", 
       x = "Beruf",
       y = "KUT [1 - 6]",
       colour = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("exampleInteraction.png", width = 9, height = 6)
