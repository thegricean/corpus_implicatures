# Uses example-trials.csv to create new csv with two seperate columns for "rating" and "strange sentence --> pilot.csv

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pilot <- read.csv("pilot1.csv")

pilot <- pilot[c("workerid","tgrep_id","rating","strange","slide_number_in_experiment","Answer.time_in_minutes")]

View(pilot)

write.csv(pilot, file = "pilot1.csv")


pilot$tgrep_id <- str_replace_all(pilot$tgrep_id,"\"","")
colnames(pilot)[colnames(pilot) =="speaker_cond"] <- "Answer.time_in_minutes"
