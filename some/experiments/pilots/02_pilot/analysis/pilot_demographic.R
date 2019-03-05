library(tidyverse)
d = read.csv("example-subject_information.csv", header = TRUE)

d <- d[c(8,1,2,3,4,5,6,7,9)]

# EXCLUDE WORKERID = 0,1,2 --> THEY WEREN'T PAID
d <-d[!(d$workerid=="0"| d$workerid=="1"| d$workerid=="2"),]

# look at comments
unique(d$comments)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram(stat="count")

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")
table(d$asses)

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

# average time 
df = read.csv("pilot.csv", header = TRUE)
df <-df[!(df$workerid=="0"| df$workerid=="1"| df$workerid=="2"),]
times = rbind(as.integer(unique(df$workerid)),unique(df$Answer.time_in_minutes))
View(times)
