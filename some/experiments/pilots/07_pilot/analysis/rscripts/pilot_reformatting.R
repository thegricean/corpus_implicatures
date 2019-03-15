# Uses example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence --> pilot.csv

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data')

pilot <- read.csv("example-trials.csv")

pilot <- separate(pilot,response,into=c("rating","strange"),sep=",")
pilot$rating <- as.character(gsub("\\''","",pilot$rating))
pilot$rating <- as.character(gsub("\\[","",pilot$rating))
pilot$strange <- as.character(gsub("\\]","",pilot$strange))
pilot$strange <- as.character(gsub("'false'","False",pilot$strange))
pilot$strange <- as.character(gsub("'true'","True",pilot$strange))
pilot$strange <- as.character(gsub("'unclear'","Not_sure",pilot$strange))

pilot$Answer.condition <- NULL

write.csv(pilot, file = "pilot.csv")

View(pilot)
