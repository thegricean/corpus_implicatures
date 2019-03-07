library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d = read.csv("pilot_demographic.csv", header = TRUE)

# look at comments
unique(d$comments)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram(stat="count")

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")

table(d$asses)

d_no = d %>% 
  filter(asses == "No")

d_confused = d  %>% 
  filter(asses == "Confused")

#------------------------- workers who didn't do HIT correctly---------------------------
d_no$workerid
#------------------------- workers who were confused-------------------------------------
d_confused$workerid

# enjoyment (3 levels)
ggplot(d, aes(x=enjoyment)) +
  geom_histogram(stat="count")

# age
ggplot(d, aes(x=age)) +
  geom_histogram(stat="count")

# gender
ggplot(d, aes(x=gender)) +
  geom_histogram(stat="count")

# education
ggplot(d, aes(x=education)) +
  geom_histogram(stat="count")

# language
ggplot(d, aes(x=language)) +
  geom_histogram(stat="count")
