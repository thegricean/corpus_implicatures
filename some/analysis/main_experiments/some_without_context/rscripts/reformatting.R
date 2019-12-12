library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data/trials/')

df <- read.csv("trials_merged.csv")
df <- separate(df,response,into=c("rating","checkbox"),sep=",")

df$rating <- as.character(gsub("\\[","",df$rating))
df$checkbox <- as.character(gsub("\\]","",df$checkbox))

df$rating <- as.character(gsub("\\'","",df$rating))
df$checkbox <- as.character(gsub("\\'","",df$checkbox))


write.csv(df, file = "trials.csv")
