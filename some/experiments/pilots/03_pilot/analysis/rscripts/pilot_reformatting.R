# Uses example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence --> pilot.csv

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

pilot <- read.csv("example-trials.csv")

pilot <- separate(pilot,response,into=c("rating","strange"),sep=",")
pilot$rating <- as.character(gsub("\\[","",pilot$rating))
pilot$strange <- as.character(gsub("\\]","",pilot$strange))
pilot$Answer.condition <- NULL
pilot <- pilot[c(5,6,3,4,1,2)]

write.csv(pilot, file = "pilot.csv")

View(pilot)
