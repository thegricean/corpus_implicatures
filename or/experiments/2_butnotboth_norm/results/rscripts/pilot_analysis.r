theme_set(theme_bw(18))
require(tidyverse)
library(stringr)

source("helpers.R")

d1 = read.table(file="../data/pilot1.csv",sep=",",header=T)
d2 = read.table(file="../data/pilot2.csv",sep=",",header=T)
d = rbind(d1,d2)
d = data.frame(lapply(d, function(x) { gsub('\"','',x,fixed=T) } ))
d$Answer.time_in_minutes = as.numeric(as.character(d$Answer.time_in_minutes))
d$slide_number_in_experiment = as.numeric(as.character(d$slide_number_in_experiment))
d$Trial = d$slide_number_in_experiment - 4
nrow(d)
str(d)
summary(d)
head(d)

# look at comments
unique(d$comments)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram(stat="count")
table(d$fairprice)

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")

# enjoyment
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

# completion time
ggplot(d, aes(x=Answer.time_in_minutes)) +
  geom_histogram(stat="count")

d = d %>%
  filter(Trial > 0)

dd = d %>%  
  count(tgrep.id,response,sort=T)
View(dd[dd$n > 3,])
nrow(dd)  
View(dd)  
  
  
  
  
  
  