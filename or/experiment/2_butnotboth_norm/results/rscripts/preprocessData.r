library(tidyverse)
library(stringr)


#read in all data, path should point to the directory with all experiment folders
source("helpers.R")
directories = list.dirs(path = "../../data/", full.names = TRUE, recursive = TRUE)

d1 = read.csv("../../results/data/pilot1.csv")
d2 = read.csv("../../results/data/pilot2.csv")
d = rbind(d1,d2)
#all experminet result files should be named exp+number+csv
for (dir in directories) {
	files <- list.files(path = dir ,  full.names = TRUE, "^exp\\.csv$")
	
	if (!(identical(files, character(0)))){
	newdata = read.csv(file=files)
	d = rbind(d, newdata)}
}
#all results are read into one dataframe (d) with 4487 rows
nrow(d)
d$Trial = d$slide_number_in_experiment - 4

####### STANDARD CHECKS ########
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

# separate out the practice trials
practice = d %>%
  filter(Trial < 1)

d = d %>%
  filter(Trial > 0) %>%
  droplevels()
nrow(d)
names(d)
table(d$tgrep.id)
table(d$response)

# get the majority response for each case
dd = d %>%  
  count(tgrep.id,response,sort=T) %>%
  filter(n > 3)

ggplot(dd, aes(x=n)) +
  geom_histogram()
nrow(dd)

# write the majority response, to be imported by r script in 1_butnotboth_insertion
write.csv(dd,file="../data/bestresponses.csv",row.names=F,quote=F)
