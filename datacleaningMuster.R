#### Analyse Skript 

#### Bibliotheken laden

install.packages("tidyverse")
install.packages("lubridate")
install.packages("psych")
install.packages("esquisse")
install.packages("ggthemes")
install.packages("ggplot2")

install.packages("devtools")
library(devtools)
devtools::install_github("HCIC/r-tools")

source("surveymonkey.R")

#### Datei laden ----
filename <- "data/robotik.csv"
raw <- load_surveymonkey_csv(filename)
# WICHTIG: Ihre csv-Datei muss unbedingt in den Ordner data, da sie sonst öffentlich wird.

#### Daten cleanen----

### Schritt 1: Unnötige Spalten löschen. In diesem Fall löschen wir die Spalten 1 bis 8.
raw.short <- raw[,c(-1:-8)] 

### Schritt 2: Variablen umbenennen 
## Variante 1:
# names(raw.short)[1] <- "age" 
# names(raw.short)[2] <- "gender"
# names(raw.short)[3] <- "kut1"
# usw...
## diese Variante ist sehr umständlich.

## Variante 2: Eine eigene Datei mit den Variablennamen erzeugen:
generate_codebook(raw.short, "codebook.csv")
# Dann codebook.csv in Excel öffnen, die Vairablennamen per Hand umbenennen, 
# die Datei als codebook_final.csv abspeichern und hier wieder einlesen:
codebook <- read_codebook("codebook_final.csv")

#neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen


# Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

# Skala für Likertskala anlegen:
scale.zustimmung <-c("Stimme gar nicht zu", 
                     "Stimme nicht zu", 
                     "Stimme eher nicht zu", 
                     "Stimme eher zu", 
                     "Stimme zu", 
                     "Stimme völlig zu")

scale.zustimmung2 <-c("Stimme gar nicht zu", 
                      "Stimme nicht zu", 
                      "Stimme eher nicht zu", 
                      "Stimme eher zu", 
                      "Stimme zu", 
                      "Stimme sehr zu")

# Likert-Items zu ordinalen levels machen:
raw.short$kut1 <- ordered(raw.short$kut1, levels = scale.zustimmung)
raw.short$kut2 <- ordered(raw.short$kut2, levels = scale.zustimmung)
raw.short$kut3 <- ordered(raw.short$kut3, levels = scale.zustimmung)
raw.short$kut4 <- ordered(raw.short$kut4, levels = scale.zustimmung)
raw.short$kut5 <- ordered(raw.short$kut5, levels = scale.zustimmung)
raw.short$kut6 <- ordered(raw.short$kut6, levels = scale.zustimmung)
raw.short$kut7 <- ordered(raw.short$kut7, levels = scale.zustimmung)
raw.short$kut8 <- ordered(raw.short$kut8, levels = scale.zustimmung)

raw.short$diffpref1 <- ordered(raw.short$diffpref1, levels = scale.zustimmung)
raw.short$diffpref2 <- ordered(raw.short$diffpref2, levels = scale.zustimmung)
raw.short$diffpref3 <- ordered(raw.short$diffpref3, levels = scale.zustimmung)
raw.short$diffpref4 <- ordered(raw.short$diffpref4, levels = scale.zustimmung)
raw.short$diffpref5 <- ordered(raw.short$diffpref5, levels = scale.zustimmung)
raw.short$diffpref6 <- ordered(raw.short$diffpref6, levels = scale.zustimmung)

raw.short$tv1 <- ordered(raw.short$tv1, levels = scale.zustimmung)
raw.short$tv2 <- ordered(raw.short$tv2, levels = scale.zustimmung)
raw.short$tv3 <- ordered(raw.short$tv3, levels = scale.zustimmung)
raw.short$tv4 <- ordered(raw.short$tv4, levels = scale.zustimmung)

raw.short$dsb1 <- ordered(raw.short$dsb1, levels = scale.zustimmung)
raw.short$dsb2 <- ordered(raw.short$dsb2, levels = scale.zustimmung)
raw.short$dsb3 <- ordered(raw.short$dsb3, levels = scale.zustimmung)
raw.short$dsb4 <- ordered(raw.short$dsb4, levels = scale.zustimmung)

raw.short$auto1 <- ordered(raw.short$auto1, levels = scale.zustimmung)
raw.short$auto2 <- ordered(raw.short$auto2, levels = scale.zustimmung)

scale.zutreffen <-c("trifft gar nicht zu", 
                    "trifft nicht zu", 
                    "trifft eher nicht zu", 
                    "trifft eher zu", 
                    "trifft zu", 
                    "trifft völlig zu")

raw.short$pflege1 <- ordered(raw.short$pflege1, levels = scale.zutreffen)
raw.short$pflege2 <- ordered(raw.short$pflege2, levels = scale.zutreffen)
raw.short$pflege3 <- ordered(raw.short$pflege3, levels = scale.zutreffen)
raw.short$pflege4 <- ordered(raw.short$pflege4, levels = scale.zutreffen)

scale.gerne <-c("Auf keinen Fall", 
                "ungerne", 
                "eher ungerne", 
                "eher gerne", 
                "gerne", 
                "Sehr gerne")

raw.short$robot_bed <- ordered(raw.short$robot_bed, levels = scale.gerne)
raw.short$robot_food <- ordered(raw.short$robot_food, levels = scale.gerne)
raw.short$robot_med <- ordered(raw.short$robot_med, levels = scale.gerne)
raw.short$robot_bodyc <- ordered(raw.short$robot_bodyc, levels = scale.gerne)
raw.short$robot_hair <- ordered(raw.short$robot_hair, levels = scale.gerne)
raw.short$robot_massage <- ordered(raw.short$robot_massage, levels = scale.gerne)
raw.short$robot_bath <- ordered(raw.short$robot_bath, levels = scale.gerne)
raw.short$robot_wash <- ordered(raw.short$robot_wash, levels = scale.gerne)
raw.short$robot_toilet <- ordered(raw.short$robot_toilet, levels = scale.gerne)
raw.short$robot_facec <- ordered(raw.short$robot_facec, levels = scale.gerne)
raw.short$robot_haircut <- ordered(raw.short$robot_haircut, levels = scale.gerne)

raw.short$human_bed <- ordered(raw.short$human_bed, levels = scale.gerne)
raw.short$human_food <- ordered(raw.short$human_food, levels = scale.gerne)
raw.short$human_med <- ordered(raw.short$human_med, levels = scale.gerne)
raw.short$human_bodyc <- ordered(raw.short$human_bodyc, levels = scale.gerne)
raw.short$human_hair <- ordered(raw.short$human_hair, levels = scale.gerne)
raw.short$human_massage <- ordered(raw.short$human_massage, levels = scale.gerne)
raw.short$human_bath <- ordered(raw.short$human_bath, levels = scale.gerne)
raw.short$human_wash <- ordered(raw.short$human_wash, levels = scale.gerne)
raw.short$human_toilet <- ordered(raw.short$human_toilet, levels = scale.gerne)
raw.short$human_facec <- ordered(raw.short$human_facec, levels = scale.gerne)
raw.short$human_haircut <- ordered(raw.short$human_haircut, levels = scale.gerne)

raw.short$trost_robot <- ordered(raw.short$trost_robot, levels = scale.zustimmung2)
raw.short$trost_human <- ordered(raw.short$trost_human, levels = scale.zustimmung2)

#### Schritt 4: Skalen berechnen

library(psych)

schluesselliste <- list(KUT= c("kut1", "-kut2", "kut3", "kut4", "-kut5", "kut6", "-kut7", "-kut8"),
                        DIFFPREF = c("diffpref1", "diffpref2", "diffpref3", 
                                     "diffpref4", "-diffpref5", "-diffpref6"),
                        TV = c("tv1", "tv2", "tv3", "tv4"),
                        DSB = c("dsb1", "dsb2", "-dsb3", "dsb4"),
                        AUTO = c("auto1", "-auto2"),
                        PFLEGE = c("pflege1", "pflege2", "pflege3", "pflege4"),
                        ROBOT = c("robot_bed", "robot_food", "robot_med", "robot_bodyc", "robot_hair", 
                                  "robot_massage", "robot_bath", "robot_wash", "robot_toilet", 
                                  "robot_facec", "robot_haircut"),
                        HUMAN = c("human_bed", "human_food", "human_med", "human_bodyc", "human_hair", 
                                  "human_massage", "human_bath", "human_wash", "human_toilet", 
                                  "human_facec", "human_haircut"),
                        TROST_HUMAN = c("trost_human"),
                        TROST_ROBOT = c("trost_robot")
)

scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 1, max = 6)

data <- bind_cols(raw.short, as.tibble(scores$scores))
data <- data %>% 
  select(-starts_with("kut", ignore.case = F)) %>% 
  select(-starts_with("diffpref", ignore.case = F)) %>%
  select(-starts_with("tv", ignore.case = F)) %>%
  select(-starts_with("dsb", ignore.case = F)) %>%
  select(-starts_with("auto", ignore.case = F)) %>%
  select(-starts_with("pflege", ignore.case = F)) %>%
  select(-starts_with("robot_", ignore.case = F)) %>%
  select(-starts_with("human_", ignore.case = F)) %>% 
  select(-starts_with("trost_", ignore.case = F))

saveRDS(data, "data/robotik2.rds")
# WICHTIG: Ihre rds-Datei muss unbedingt in den Ordner data, da sie sonst öffentlich wird.

