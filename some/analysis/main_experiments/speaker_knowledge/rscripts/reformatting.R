# Create new csv with two seperate columns for "rating" and "strange sentence

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../data')

df <- read.csv("results_merged.csv")

df <- separate(df,response,into=c("rating","strange"),sep=",")
df$rating <- as.character(gsub("\\''","",df$rating))
df$rating <- as.character(gsub("\\[","",df$rating))

df$strange <- as.character(gsub("True","true",df$strange))
df$strange <- as.character(gsub("False","false",df$strange))
df$strange <- as.character(gsub("Not_sure","not_sure",df$strange))
df$strange <- as.character(gsub("Unclear","not_sure",df$strange))

df$strange <- as.character(gsub("\\]","",df$strange))
df$strange <- as.character(gsub("\\'","",df$strange))


df$Answer.condition <- NULL

write.csv(df, file = "results_formatted.csv")

View(df)
