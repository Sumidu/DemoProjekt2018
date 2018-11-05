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
factor(data$b5_1, levels = zutreffen)

data$b5_1[2]==test

summary(data[,1:15])
