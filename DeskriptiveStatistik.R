## Deskriptive Auswertung des bereinigten Datensatzes

library(tidyverse)
library(ggthemes)
library(ggplot2)
data_robot <- readRDS("data/robo_pflege.rds")

# install.packages("plotrix")
library(plotrix)

psych::describeData(data_robot)

# (data_robot %>% ggplot() +
#   aes(x = age, y = kut, color = gender) + 
#   geom_point() +
#   geom_smooth(method = lm) +
#   labs(title = my_y_title,
#        subtitle = "Scatterplot von KUT und Alter",
#        x = "Alter",
#        y = "Kontrollüberzeugung im Umgang mit Technik",
#        caption = "") + 
#   NULL -> p_KUT_Alter)
# 
# breite <- 21
# hoehe <- breite * 0.681
#   
# ggsave(plot = p_KUT_Alter, "Alter_KUT_Scatterplot.pdf", width = breite, height = hoehe, units = "cm")
# 
# data_robot %>% group_by(gender) %>% summarise(mean_age = mean(age), sd_age = sd(age)) %>% 
#   ggplot() +
#   aes(x = gender, fill = gender, weight = mean_age, ymin = mean_age - sd_age, ymax = mean_age + sd_age) +
#   geom_bar() +
#   geom_errorbar(width = 0.5) +
#   theme_minimal() +
#   
#   labs(title = "Titel ÄÜßÖ", subtitle = "Erklärung", 
#        x = "Geschlecht",
#        y = "Alter in Jahren",
#        fill = "Geschlecht") +
#   NULL


rwthcolor <- hcictools::rwth.colorpalette()


data_robot %>% 
  filter(gender != "keine Angabe") %>%
  group_by(gender) %>% 
  summarise(mean_kut = mean(kut), sem_kut = std.error(kut)) %>%
ggplot() +
  aes(x = gender, y = mean_kut, colour = gender, weight = mean_kut, ymin = mean_kut - sem_kut, ymax = mean_kut + sem_kut ) +
  geom_errorbar(width = 0.2, colour = rwthcolor$black) +
  geom_point(size = 3) +
  scale_colour_manual(values=c(rwthcolor$blue, rwthcolor$red)) + 
  # coord_cartesian(ylim = c(1,6)) + 
  ylim(2,5) +
  # scale_y_continuous(limits = c(1,6)) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Punktdiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [1 - 6]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("examplepoint.png", width = 6, height = 6)

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  group_by(gender) %>% 
  summarise(mean_kut = mean(kut)-1, sem_kut = std.error(kut)) %>%
  ggplot() +
  aes(x = gender, fill = gender, weight = mean_kut, ymin = mean_kut - sem_kut, ymax = mean_kut + sem_kut ) +
  geom_bar( width = 0.5) +
  scale_fill_manual(values=c(rwthcolor$blue, rwthcolor$red)) + 
  geom_errorbar(width = 0.2) +
  # coord_cartesian(ylim = c(1,6)) + 
  ylim(0,5) +
  # scale_y_continuous(limits = c(1,6)) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Balkendiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [0 - 5]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("examplebar.png", width = 6, height = 6)

data_robot %>% 
  filter(gender != "keine Angabe") -> data2
  t.test(data2$kut ~ data2$gender)


t.test( filter(data_robot, gender == "männlich")$kut, filter(data_robot, gender == "weiblich")$kut)
  

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

# install.packages("jmv")
library(jmv)

data_robot %>% 
  filter(gender != "keine Angabe") %>%
  filter(job_type == "Arbeitnehmer/in" | job_type == "Studierend/Schüler/in" ) %>% 
jmv::anova(dep = "kut", factors = c("job_type", "gender"), emMeans = list(c("job_type", "gender")) ) -> res

res$main 
res$emm
