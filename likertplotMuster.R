### Muster

library(tidyverse)
library(likert)
library(grid)

data <- read_rds("data/robo_pflege.rds")

response_map <- function(x){
  factor(x, labels = c("strongly disagree", "disagree", 
                         "rahter disagree", "rather agree",
                          "agree", "strongly agree"))
}

data %>% 
  select(robo_bed:robot_face) %>% 
  mutate_all(response_map) ->  lik

lik <- as.data.frame(lik)
result <- likert(lik, nlevels = 6)

likert.options(low.color = "#FF0000", high.color = "#00FF00", neutral.color = "grey90", neutral.color.ramp = "white", colors = NULL, plot.percent.low = TRUE, plot.percent.high = TRUE, plot.percent.neutral = TRUE, plot.percents = FALSE, text.size = 3, text.color = "black", centered = TRUE, include.center = TRUE, ordered = TRUE, wrap = 50, wrap.grouping = 50, legend = "Response", legend.position = "bottom", panel.arrange = "v", panel.strip.color = "#F0F0F0")
plot(result, type="bar") + labs(title="Test") + scale_fill_manual(values=c(rwthcolor$blue, rwthcolor$lightblue, rwthcolor$magenta, rwthcolor$yellow, rwthcolor$petrol, rwthcolor$red))
str(lik)


rwthcolor <- hcictools::rwth.colorpalette()
