# reformat pilot.csv files for all 6 files

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pilot <- read.csv("pilot1.csv")

pilot <- pilot[c("workerid","tgrep_id","rating","strange","slide_number_in_experiment","Answer.time_in_minutes")]

View(pilot)

write.csv(pilot, file = "pilot1.csv")

#for pilot1 because submiterator batched and supersubmiterator save results differently
pilot$tgrep_id <- str_replace_all(pilot$tgrep_id,"\"","")
colnames(pilot)[colnames(pilot) =="speaker_cond"] <- "Answer.time_in_minutes"

#for pilot2 because workerid 0,1,2 were not paid
pilot <- pilot[!(pilot$workerid %in% c("0","1","2")),]
pilot$workerid = pilot$workerid-3