theme_set(theme_bw(18))
require(tidyverse)
library(stringr)
# library(lmerTest)
library(brms)

source("helpers.R")

# load final.csv to merge sentences into tgrep ids
idmatching = read.table(file="../../../1_butnotboth_insertion/results/data/bestresponses.csv",sep="\t",header=T,quote="")
colnames(idmatching) = c("tgrep.id","sentence_original","sentence_bnb")

# load data
d1 = read.table(file="../data/pilot1.csv",sep=",",header=T)
d2 = read.table(file="../data/pilot2.csv",sep=",",header=T)
workerincrement = 5
d2$workerid = d2$workerid + workerincrement
d = rbind(d1,d2)

directories = list.dirs(path = "../../", full.names = TRUE, recursive = TRUE)
for (dir in directories) {
  files <- list.files(path = dir ,  full.names = TRUE, "^exp\\.csv$")
  
  if (!(identical(files, character(0)))){
    newdata = read.csv(file=files)
    workerincrement = workerincrement + 9
    newdata$workerid = newdata$workerid + workerincrement
    d = rbind(d, newdata)}
}

nrow(d)

d = data.frame(lapply(d, function(x) { gsub('\"','',x,fixed=T) } ))
d$Answer.time_in_minutes = as.numeric(as.character(d$Answer.time_in_minutes))
d$slide_number_in_experiment = as.numeric(as.character(d$slide_number_in_experiment))
d$Trial = d$slide_number_in_experiment - 4
nrow(d)
str(d)
summary(d)
head(d)
length(unique(d$tgrep.id))

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
  geom_histogram()


## PRACTICE TRIALS
practice = d %>%
  filter(Trial < 0) %>%
  mutate(response = as.numeric(as.character(response)))

# plot the two example answers
ggplot(practice, aes(x=response,fill=as.factor(Trial))) +
  geom_density(alpha=.5)
  
## TARGET TRIALS
d = d %>%
  filter(Trial > 0) %>%
  left_join(idmatching,by=c("tgrep.id"))
d$response_goodsentence = as.character(gsub('[{u\'strance_sentence\': ','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 1),fixed=T))
d$response_goodsentence = as.character(gsub('[{u\'strange_sentence\': ','',sapply(strsplit(as.character(d$response_goodsentence),", u\'sliderval\': "), "[", 1),fixed=T))
d$tmp = ""
d[nchar(as.character(d$response_goodsentence)) > 6,]$tmp = as.character(gsub(']','',sapply(strsplit(as.character(d[nchar(as.character(d$response_goodsentence)) > 6,]$response_goodsentence),", "), "[", 2),fixed=T))
d[nchar(as.character(d$response_goodsentence)) > 6,]$response_goodsentence = d[nchar(as.character(d$response_goodsentence)) > 6,]$tmp
d$response_goodsentence = as.factor(as.character(d$response_goodsentence))

d$tmp = ""

d = d %>%
  mutate(response_goodsentence = fct_recode(response_goodsentence, 
                                            "good bnb sentence" = "False",
                                            "bad bnb sentence" = "True"),
         sentence_length = nchar(as.character(sentence_bnb)))
d$response_val = as.numeric(as.character(gsub('}]','',sapply(strsplit(as.character(d$response),", u\'sliderval\': "), "[", 2),fixed=T)))
d$tmp = -555
d[nchar(as.character(d$response)) > 8 & nchar(as.character(d$response)) < 15,]$tmp = as.numeric(as.character(gsub('[','',sapply(strsplit(as.character(d[nchar(as.character(d$response)) > 8 & nchar(as.character(d$response)) < 15,]$response),", "), "[", 1),fixed=T)))
d[nchar(as.character(d$response)) > 8 & nchar(as.character(d$response)) < 15,]$response_val = d[nchar(as.character(d$response)) > 8 & nchar(as.character(d$response)) < 15,]$tmp
d$log_sentence_length = log(d$sentence_length)

# are some cases clearly weirder than others?
dd = d %>%  
  count(tgrep.id,response_goodsentence,sort=T)
View(dd)  

# these should be good:
unique(d[d$tgrep.id %in% c("104373:17","112263:70","112347:54","112417:64"),]$sentence_bnb)

# these should be bad:
unique(d[d$tgrep.id %in% c("166599:24","46957:32","23676:18","105458:174","106988:25"),]$sentence_bnb)

# plot overall distribution of responses -- looks like way more "dissimilar" responses, replicating Degen 2015 :)
agr = d %>%
  group_by(tgrep.id) %>%
  summarize(Mean=mean(response_val))
ggplot(agr, aes(x=Mean)) +
  geom_histogram() +
  xlab("Mean rating by item")
ggsave("../graphs/responses.pdf",height=3.5,width=6)

# plot overall distribution of responses by whether sentence was rated as weird sounding --
# -- more "similar" (implicature) responses for good than for weird sentences
# -- even for good sentences, there are more low (no implicature) than high (implicature) values
ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) 

# plot subject variability -- some show more bimodality than others
p = ggplot(d, aes(x=response_val)) +
  geom_histogram() +
  facet_wrap(~workerid)


# plot subject variability by whether they thought the sentences was weird 
# -- not everyone uses the button, but of those who do, they tend to rate those cases lower
p = ggplot(d, aes(x=response_val,fill=response_goodsentence)) +
  geom_density(alpha=.5) +
  facet_wrap(~workerid,scales="free_y")
ggsave(p,file="../graphs/subject_variability.pdf",height=40,width=40)

# implicature ratings by tgrep.id, include proportion of good sentences  
good = d %>% 
  group_by(tgrep.id,response_goodsentence) %>%
  summarize(counts = n()) %>%
  group_by(tgrep.id) %>%
  mutate(total = sum(counts),proportion_goodsentence=counts/total) %>%
  filter(response_goodsentence == "good bnb sentence") %>%
  select(tgrep.id,proportion_goodsentence) 
agr = d %>%
  group_by(sentence_bnb,tgrep.id) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,Sentence_BNB=fct_reorder(sentence_bnb,Mean)) %>%
  arrange(Mean) %>%
  left_join(good,by=c("tgrep.id"))
agr$proportion_goodsentence = replace_na(agr$proportion_goodsentence,0)
  
write.csv(agr[,c("tgrep.id","proportion_goodsentence","Sentence_BNB","Mean","YMin","YMax")],file="../data/sentence_si_means.csv",row.names=F)

write.csv(d[,c("tgrep.id","workerid","sentence_bnb","response_goodsentence","response_val","Trial","gender","age")],file="../data/data.csv",row.names=F)

ggplot(agr, aes(x=Sentence_BNB,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans.pdf",height=40,width=45)  

# implicature ratings by tgrep.id and whether or not sentence was good and include proportion of good/bad sentences
agr = d %>%
  group_by(tgrep.id,sentence_bnb,response_goodsentence) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)  %>% 
  arrange(Mean,response_goodsentence)
write.csv(agr[,c("tgrep.id","sentence_bnb","response_goodsentence","Mean","YMin","YMax")],file="../data/sentence_si_means_bygoodsentence.csv",row.names=F)

ggplot(agr, aes(x=sentence_bnb,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  facet_wrap(~response_goodsentence) +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans_bygoodsentence.pdf",height=40,width=45)  


ggplot(agr, aes(x=Sentence_BNB,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans.pdf",height=40,width=45)  

# implicature ratings by tgrep.id and whether or not sentence was good and include proportion of good/bad sentences
agr = d %>%
  group_by(tgrep.id,sentence_bnb,response_goodsentence) %>%
  summarize(Mean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)  %>% 
  arrange(Mean,response_goodsentence)
write.csv(agr[,c("tgrep.id","sentence_bnb","response_goodsentence","Mean","YMin","YMax")],file="../data/sentence_si_means_bygoodsentence.csv",row.names=F)

ggplot(agr, aes(x=sentence_bnb,y=Mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  coord_flip() +
  facet_wrap(~response_goodsentence) +
  theme(axis.text.y=element_text(size=20))
# ggsave(file="../graphs/pilot_casemeans_bygoodsentence.pdf",height=40,width=45)