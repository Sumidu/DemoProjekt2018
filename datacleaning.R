# Analyseskript


# Bibliotheken laden
library(tidyverse)
source("surveymonkey.R")



#Datei laden
filename <- "data/robotik.csv"
raw <- load_surveymonkey_csv(filename) 



## Variablen löschen ----
variable_count <- dim(raw)[2]
data <- raw[ ,c(9:variable_count)]

##generate_codebook(data, "codebook.csv")

## Variablen umbenennen ----
#names(data)
#names(data)[1] <- "gender"
#names(data)[2] <- "age"
#names(data)[3] <- "education"
#names(data)[4] <- "job"
#names(data)[5] <- "income"


new_names <- read_codebook("codebookfinal.csv")
names(data) <- new_names$variable

data$human_trost

## Variablen im Typ anpassen ----
str(data$gender)
data$gender <- as.factor(data$gender)


skala.zustimmen <- rev(c("Stimme völlig zu", "Stimme zu", "Stimme eher zu", 
                     "Stimme eher nicht zu", "Stimme nicht zu", "Stimme gar nicht zu"))
skala.zutreffen <- c("trifft gar nicht zu", "trifft nicht zu", "trifft eher nicht zu",
                     "trifft eher zu", "trifft zu", "trifft völlig zu")
skala.gerne <- c("Auf keinen Fall", "ungerne", "eher ungerne",
                     "eher gerne", "gerne", "Sehr gerne")

scale2numeric <- function(x, scale){
  as.numeric(ordered(x, levels = scale))
}

scale2numeric.zustimmen <- function(x){
  scale2numeric(x, scale = skala.zustimmen)
}

scale2numeric.zutreffen <- function(x){
  scale2numeric(x, scale = skala.zutreffen)
}

scale2numeric.gerne <- function(x){
  scale2numeric(x, scale = skala.gerne)
}


data %>% mutate_at(vars(starts_with("KUT")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("diffpref")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("tv")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("dsb")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("autot")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("pflege")), scale2numeric.zutreffen) %>% 
  rename(ht = human_trost, rt = robo_trost) %>% 
  mutate_at(vars(starts_with("robo")), scale2numeric.gerne) %>% 
  mutate_at(vars(starts_with("human")), scale2numeric.gerne) %>% 
  mutate_at(vars(starts_with("ht")), scale2numeric.zustimmen) %>% 
  mutate_at(vars(starts_with("rt")), scale2numeric.zustimmen) -> data.clean



## Variablen berechnen -----

# Bibliothek zum berechnen von Skalen 
library(psych)

# Schlüsselzuweisungen zu den Skalen
key.list <- list(kut = c("KUT1", "-KUT2", "KUT3", "KUT4", "-KUT5", "KUT6", "-KUT7", "-KUT8"),
                 sdiffpref = c("diffpref1", "diffpref2", "diffpref3", "diffpref4", "diffpref5", "-diffpref6"),
                 stv = c("tv1", "tv2", "tv3", "tv4"),
                 sdsb = c("dsb1", "dsb2", "dsb3", "dsb4"),
                 sautot = c("autot1", "-autot2"),
                 spflege = c("pflege1", "pflege2", "pflege3", "pflege4"))

scales <- scoreItems(key.list,
                     data.clean,
                     min = 1,
                     max = 6,
                     missing = T)

#### Reliability
scales$alpha

as.tibble(scales$scores)

# generierte Variablen
data.clean <- bind_cols(data.clean, as.tibble(scales$scores))

# Daten sortieren und ansehen ----

data.clean %>% filter(age > 18, age < 99)
data.clean %>% select(age, kut)
data.clean %>% arrange(desc(age))
data.clean %>% group_by(gender) %>% summarise(age = mean(age, na.rm = T))

data.clean %>% select(-starts_with("KUT", ignore.case = F), 
                      -starts_with("diffpref", ignore.case = F),
                      -starts_with("tv", ignore.case = F),
                      -starts_with("dsb", ignore.case = F),
                      -starts_with("autot", ignore.case = F),
                      -starts_with("pflege", ignore.case = F)) %>% 
  filter(age > 18, age < 99) %>% 
  rename(diffpref = sdiffpref, tv= stv, dsb=sdsb, autot = sautot, pflege=spflege) -> data.analysis 


data.analysis$gender <- factor(data.analysis$gender)
data.analysis$job_type <- factor(data.analysis$job_type)
describe(data.analysis[,c(-2)])

write_rds(data.analysis, "robo_pflege.rds")
