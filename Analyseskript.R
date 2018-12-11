# Analyseskript


# Bibliotheken laden
library(tidyverse)
source("surveymonkey.R")



#Datei laden
data <- readRDS("robo_pflege.rds")


### descriptive statistics

#library(psych)
#describe(data)
psych::describe(data) 

table(data$gender)
table(data$job_type)
table(data$job)


data %>% group_by(gender) %>% summarise(mean_age = mean(age), sd_age = sd(age),
                                        mean_kut = mean(kut), sd_kut = sd(kut),
                                        mean_robo_bath = mean(robo_bath, na.rm = T)) 




# Grouping as a variable
data %>% mutate(age_groups = ntile(age, 3)) -> data
data %>% group_by(age_groups) %>% summarise(mean_kut = mean(kut), m_age = mean(age))

n <- 3
v <- (1:n)/n
quantile(data$age, probs = v)

t.test(data$kut ~ data$gender)

#install.packages("esquisse")


library(ggthemes)
data %>% filter(gender == "männlich" | gender == "weiblich") %>% 
 ggplot() +
  aes(x = age) +
  geom_histogram(bins = 50, fill = '#8EBAE5') +
  labs(title = 'Das Alter ist typisch für eine studentische Stichprobe',
       x = 'Alter',
       y = 'Häufigkeit',
       caption = '50 Bins',
       subtitle = 'Histogramm der Altersverteilung') +
  theme_gray() +
  facet_wrap(vars(gender)) -> p



grafik_breite <- 15
grafik_hoehe <- grafik_breite * 0.681

ggsave(plot = p, "Altersverteilung.pdf", width = grafik_breite, height = grafik_hoehe, units = "cm")



t.test(data$age) -> cort

apastats::describe.ttest(cort, type = "latex")
library(apaTables)





aov(libido~dose, viagra) -> model
anova(model)


