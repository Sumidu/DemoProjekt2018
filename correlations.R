# Setup -----

library(tidyverse)
library(jmv)

library(corrplot)
# read data ----

df_pflege <- readRDS("robo_pflege.rds")


cor(df_pflege$age, df_pflege$kut)

# Korrelationstest mit Konfidenzintervall
model <- cor.test(df_pflege$age, df_pflege$kut)


cor.test(~age + kut, data=df_pflege, method = "kendall")

# Datenf für explorative Analyse vorbereiten (nur numerische Variablen)
df_pflege %>% select(age, robo_bed, starts_with("robo"), starts_with("human"), kut) -> df_test

# Matrix von korrelationen
cor(df_test, use = "pairwise.complete.obs") -> M


# Korrelationsplot für Explorative Analyse
hcictools::cor.matrix.plot(df_test)


selected_vars <- names(df_test)

jmv::corrMatrix(df_test, vars=c(age, kut), spearman = T, sig = F)


# View top few rows of attitude data set
head(attitude)
# Use apa.cor.table function
apa.cor.table(attitude)
apa.cor.table(attitude, filename="ex.CorTable1.doc")

apa.cor.table(df_test[,1:4])

names(df_test)
jmv::pca(df_test, vars=c("robo_bed", "robo_food","robo_med","robo_body",
                         "robo_hair_wash", "robo_mass","robo_bath","robo_wash",
                         "robo_toilett","robot_face","robo_hair_cut","human_bed","human_food",
                         "human_med","human_body","human_hair_wash", "human_mass",
                         "human_bath","human_wash","human_toilett","human_face","human_hair_cut"), screePlot = T,
         kmo = T, bartlett = T, factorSummary = T, eigen = T)


mean_se(df_pflege$robo_body)
ggplot(df_pflege) + aes(gender, robo_body) + stat_summary(fun.data = "mean_cl_boot")


