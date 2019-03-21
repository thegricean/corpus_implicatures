# Uses example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence --> pilot.csv

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data')

pilot <- read.csv("example-trials.csv")

pilot <- separate(pilot,response,into=c("rating","strange"),sep=",")
pilot$rating <- as.character(gsub("\\''","",pilot$rating))
pilot$rating <- as.character(gsub("\\[","",pilot$rating))
pilot$strange <- as.character(gsub("\\]","",pilot$strange))

pilot$Answer.condition <- NULL

write.csv(pilot, file = "trials.csv")

View(pilot)
