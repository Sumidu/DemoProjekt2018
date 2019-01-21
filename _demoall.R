###### How-To Datei
# Daten sind schon gecleaned und liegen in data vor

# Pakete laden ----
library(tidyverse)



# Daten laden ----
data <- read_rds("data/robo_pflege.rds")

# Deskriptive Statistik ----

psych::describeData(data)

# Inferenz Statistik ----

# t-Test ----

## Independent sample t-Test
# Gender hat 3 Ausprägungen aber es werden nur 2 benötigt
table(data$gender)

# Mit base-R
data %>% 
  filter(gender!="keine Angabe") %>% 
  t.test(formula = robo_bed ~ gender, data = .)

# mit jamovi

data %>% filter(gender!="keine Angabe") %>% 
  jmv::ttestIS(vars=c("robo_bed"), group=gender)


# mit ggplot über stat_summary
data %>% 
  filter(gender!="keine Angabe") %>% 
  ggplot() + 
  aes(x=gender, y=robo_bed) + 
  stat_summary(fun.data = mean_cl_normal) +
  geom_errorbar() +
  labs(title="Robo_Bed nach Geschlecht", caption="Fehlerbalken sind 95% Konfidenzintervalle") +
  scale_y_continuous(limits = c(1, 6)) +
  NULL

se <- function(x, na.rm = F) {
  if(na.rm){
    x <- na.omit(x)
  }
  sd(x)/sqrt(length(x))
}

# mit ggplot manuell
data %>% 
  filter(gender!="keine Angabe") %>%
  group_by(gender) %>% 
  summarise(robo_bed_mean = mean(robo_bed, na.rm = T),
            robo_bed_se = se(robo_bed, na.rm = T)) %>% 
  ggplot() + 
  aes(x=gender, y=robo_bed_mean, ymin = robo_bed_mean - 1.96*robo_bed_se, ymax = robo_bed_mean + 1.96*robo_bed_se,
      fill = gender) + 
  geom_col() +
  geom_errorbar(width = 0.5) +
  labs(title="Robo_Bed nach Geschlecht", caption="Fehlerbalken sind 95% Konfidenzintervalle") +
  scale_y_continuous(limits = c(0, 6)) +
  NULL


library(plotrix)
