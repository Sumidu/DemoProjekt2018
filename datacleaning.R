# Analyse Skript 

# Bibliotheken laden

library(tidyverse)
source("surveymonkey.R")

# Datenbereinigung ----

filename <- "data/Data_All_181105/CSV/Akzeptanz von Einwegbechern.csv"
raw <- load_surveymonkey_csv(filename = filename)


## Variablen löschen ----
data <- raw[ ,c(10:25)]



## Variablen umbenennen ----
names(data)
names(data)[1] <- "gender"
names(data)[2] <- "age"
names(data)[3] <- "education"
names(data)[4] <- "job"
names(data)[5] <- "income"
names(data)[6] <- "b5_1"
names(data)[7] <- "b5_2"
names(data)[8] <- "b5_3"
names(data)[9] <- "b5_4"
names(data)[10] <- "b5_5"
names(data)[11] <- "b5_6"
names(data)[12] <- "b5_7"
names(data)[13] <- "b5_8"
names(data)[14] <- "b5_9"
names(data)[15] <- "b5_10"


## Variablen im Typ anpassen ----
str(data$gender)
data$gender <- as.factor(data$gender)

str(data$education)
data$education <- ordered(data$education, levels = c("Hauptschulabschluss", "Mittlere Reife", "Fachhochschulreife", 
                                    "allgemeine Hochschulreife", "Bachelor", "Master"))


zutreffen <- c("Trifft gar nicht zu", "Trifft nicht zu", "Trifft eher nicht zu", 
               "Trifft eher zu", "Trifft zu", "Trifft voll zu")
data$b5_bug <- as.numeric(ordered(data$b5_1, levels = zutreffen))

summary(data)


## Variablen berechnen -----


data %>% mutate(b5 = b5_1 + b5_2 / 2)


library(psych)

key.list <- list(b5 = c("age"))

scales <- scoreItems(key.list, data, missing = T)

c(data, scales$scores)


data %>% filter(age > 50)
data %>% select(age)
data %>% arrange(desc(age))
data %>% group_by(gender) %>% summarise(age = mean(age))
