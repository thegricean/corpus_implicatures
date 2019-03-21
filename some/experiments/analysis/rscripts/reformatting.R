# Use example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data')

trials <- read.csv("example-trials.csv")

trials <- separate(pilot,response,into=c("rating","strange"),sep=",")
trials$rating <- as.character(gsub("\\''",""trials$rating))
trials$rating <- as.character(gsub("\\[","",trials$rating))
trials$strange <- as.character(gsub("\\]","",trials$strange))

trials$Answer.condition <- NULL

write.csv(trials, file = "trials.csv")

View(trials)
