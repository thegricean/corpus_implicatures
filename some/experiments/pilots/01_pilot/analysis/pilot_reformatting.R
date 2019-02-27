#REFORMAT DATAFRAME --> TRIAL INFO
library(tidyverse)
# merge_resultes.R creates results_merged.cvs

setwd("/Users/leylakursat/Desktop/repos/impl35_some/mturk/")

pilot <- read.csv("results_merged.csv")

pilot <- separate(pilot,response,into=c("rating","strange"),sep=",")
pilot$rating2 <- as.character(gsub("\\[","",pilot$rating))
pilot$strange2 <- as.character(gsub("\\]","",pilot$strange))
pilot$rating <- NULL
pilot$strange <- NULL
colnames(pilot)[colnames(pilot)=="rating2"] <- "rating"
colnames(pilot)[colnames(pilot)=="strange2"] <- "strange"
pilot <- pilot[c(1,2,3,5,6,4)]
write.csv(pilot, file = "pilot.csv")

#.results files --> DEMOGRAPHIC INFO
round1 <- read.csv("round1/example-subject_information.csv", header=TRUE)
round2 <- read.csv("round2/example-subject_information.csv", header=TRUE)
round3 <- read.csv("round3/example-subject_information.csv", header=TRUE)
round4 <- read.csv("round4/example-subject_information.csv", header=TRUE)
round5 <- read.csv("round5/example-subject_information.csv", header=TRUE)
round6 <- read.csv("round6/example-subject_information.csv", header=TRUE)
round7 <- read.csv("round7/example-subject_information.csv", header=TRUE)
results <- rbind(round1, round2, round3, round4, round5, round6, round7)

add_row_numbers <- function(df, name = "row_number", zero_based = FALSE) {
  # Drop variable if exists
  if (name %in% names(df)) df[, name] <- NULL
  # Create sequence of integers
  sequence <- seq.int(1, to = nrow(df))
  if (zero_based) sequence <- sequence - 1L
  # Add sequence to data frame
  df[, name] <- sequence
  # Move variable to the first column position
  df <- select(df, !!name, everything())
  return(df)
}
results <- add_row_numbers(results, name = "workerid", zero_based = TRUE)
View(results)

results$language <- as.character(gsub("\"","",results$language))
results$enjoyment <- as.character(gsub("\"","",results$enjoyment))
results$gender <- as.character(gsub("\"","",results$gender))
results$age <- as.numeric(gsub("\"","",results$age))
results$comments <- as.character(gsub("\"","",results$comments))
results$asses <- as.character(gsub("\"","",results$asses))
results$fairprice <- as.numeric(gsub("\"","",results$fairprice))
results$education <- as.numeric(gsub("\"","",results$education))

write.csv(results, file = "pilot_demographic.csv")

#FIX HTHIS to get Answer.time_in_minutes..
round1 <- read.csv("repos/impl35_some/Submiterator/round1/example-mturk.csv", header=TRUE)
d1$time = round1$Answer.time_in_minutes)
View(d1)
round2 <- read.csv("repos/impl35_some/Submiterator/round2/example-mturk.csv", header=TRUE)
d2$time = round2$Answer.time_in_minutes
total = rbind(d1,d2)

View(total)
Answer.time_in_minutes <- rbind(round1, round2, round3, round4, round5, round6, round7)




