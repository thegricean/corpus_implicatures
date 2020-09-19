theme_set(theme_bw(18))
require(tidyverse)
library(stringr)

setwd("/Users/titlis/Dropbox/switchboard/experiment/1_butnotboth_insertion_pilot/results/")
source("rscripts/helpers.R")

# d = read.csv(file="data/pilot1.csv")
# d = read.csv(file="data/pilot2.csv")
d = read.csv(file="data/pilot3.csv")
nrow(d)
summary(d)

# look at comments
unique(d$comments)

# fair price
ggplot(d, aes(x=fairprice)) +
  geom_histogram()
table(d$fairprice)

# overall assessment
ggplot(d, aes(x=asses)) +
  geom_histogram(stat="count")

# enjoyment
ggplot(d, aes(x=enjoyment)) +
  geom_histogram(stat="count")

# age
ggplot(d, aes(x=age)) +
  geom_histogram()

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
  geom_histogram()

summary(d)

# separate out the practice trials
practice = d %>%
  filter(slide_number_in_experiment < 5)

View(practice)

d = d %>%
  filter(slide_number_in_experiment > 4)
nrow(d)

table(d$tgrep.id)
View(d)
d[is.na(d$entire.sentence),]$slide_number_in_experiment
d[is.na(d$entire.sentence),]$tgrep.id

# filter out NA rows
d = d %>%
  filter(!is.na(entire.sentence))
nrow(d)

# clean response variable to strip it of junk and get it ready for analysis
d$response = gsub("[u'","",as.character(d$response),fixed=T)
d$response = gsub("]","",as.character(d$response),fixed=T)
d$response = gsub("<font color=red>","",as.character(d$response),fixed=T)
d$response = gsub("</font>","",as.character(d$response),fixed=T)
d$BNBSentence = sapply(strsplit(as.character(d$response),"', "), "[", 1)
d$NoGoodLocation = sapply(strsplit(as.character(d$response),"', "), "[", 2)
d$BNBInsertion = as.factor(ifelse(d$NoGoodLocation == "True","bad",ifelse(d$NoGoodLocation=="False","good","NA")))

table(d$tgrep.id,d$BNBInsertion)

# exclude cases where for some reason the response sentence doesn't match the original sentence
d$BNBFirstWord = sapply(strsplit(as.character(d$BNBSentence)," "), "[", 1)
d$FirstWord = sapply(strsplit(as.character(d$entire.sentence)," "), "[", 1)

unique(d[d$tgrep.id == "116776:42",]$BNBSentence)

d = d %>%
  filter(BNBFirstWord == FirstWord)
table(d$tgrep.id,d$BNBInsertion)
props = as.data.frame(prop.table(table(d$tgrep.id,d$BNBInsertion),mar=c(1))) %>%
  filter(Var2 == "good" & !is.na(Freq)) 
colnames(props) = c("tgrep.id","Good","ProportionGood")
props$Good = NULL
props
d = d %>% 
  left_join(props,by=c("tgrep.id"))
head(dnew)

forms = d %>%
  group_by(tgrep.id) %>%
  summarize(TotalCases=length(BNBSentence),NumForms = length(unique(BNBSentence)))

d = d %>% 
  left_join(forms,by=c("tgrep.id"))
# get only end of sentence
d = d %>%
  rowwise() %>%
  mutate(ShortSentence = paste(strsplit(as.character(BNBSentence)," ")[[1]][(length(strsplit(as.character(BNBSentence)," ")[[1]])-11):length(strsplit(as.character(BNBSentence)," ")[[1]])],collapse=" "))

ggplot(unique(d[,c("ProportionGood","NumForms","TotalCases")]), aes(x=ProportionGood,y=NumForms)) +
  geom_point(aes(color=TotalCases)) +
  geom_smooth()

unique(paste(d[d$ProportionGood > .6 & d$ProportionGood < 1 & d$BNBInsertion == "good",]$BNBSentence,d[d$ProportionGood > .6 & d$ProportionGood < 1 & d$BNBInsertion == "good",]$ProportionGood))

# exclude cases of "but not both" right after "or"?
d = d %>%
  rowwise() %>%
  mutate(OrBut = ifelse(grepl("or but", as.character(BNBSentence),fixed=T),"bad","good")) %>%
  filter(OrBut == "good")

# by-item distributions
ggplot(d, aes(x=BNBSentence,fill=BNBInsertion)) +
  geom_histogram(stat="count") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  facet_wrap(~tgrep.id,scales="free") +
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1,size=5))
ggsave("graphs/sentence_histograms.pdf",width=14,height=20)



