# Uses example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence --> pilot.csv

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data')

pilot <- read.csv("example-trials.csv")

pilot <- separate(pilot,response,into=c("rating","strange"),sep=",")
pilot$rating2 <- as.character(gsub("\\[","",pilot$rating))
pilot$strange2 <- as.character(gsub("\\]","",pilot$strange))
pilot$rating <- NULL
pilot$strange <- NULL
colnames(pilot)[colnames(pilot)=="rating2"] <- "rating"
colnames(pilot)[colnames(pilot)=="strange2"] <- "strange"
pilot$Answer.condition <- NULL
pilot <- pilot[c(5,1,6,7,3,4)]

write.csv(pilot, file = "pilot.csv")
