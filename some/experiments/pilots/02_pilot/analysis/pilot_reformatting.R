#REFORMAT DATAFRAME --> TRIAL INFO
library(tidyverse)
# merge_resultes.R creates results_merged.cvs


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
