library(tidyverse)
#pilot = read.csv("repos/impl35_some/pilot/pilot.csv", header = TRUE)
pilot = read.csv("pilot.csv", header = TRUE)
source("helpers.R")

#ALL TRIALS
ggplot(pilot,aes(x=rating))+
  geom_histogram()+
  labs(title = "All Ratings")+
  theme(plot.title = element_text(hjust =0.5))

#PRACTICE TRIALS
practice = pilot %>% 
  filter(str_detect(tgrep_id, "example"))

ggplot(practice, aes(x=rating,fill=as.factor(tgrep_id))) +
  geom_density(alpha=.5)

ggplot(practice,aes(x=rating))+
  geom_histogram()+
  facet_wrap(~tgrep_id)+
  labs(title = "Ratings for practice trials")+
  theme(plot.title = element_text(hjust =0.5))

#TARGET TRIALS
sentences = pilot %>% 
  filter(str_detect(tgrep_id, "example", negate = TRUE))

ggplot(sentences,aes(x=rating))+
  geom_histogram()+
  labs(title = "Ratings for target trials")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(sentences, aes(x=rating,fill=tgrep_id)) +
  geom_density(alpha=.5)

#for each item
ggplot(sentences, aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~tgrep_id)

#for each participant
ggplot(sentences, aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~workerid)

#overall distribution of responses
agr = sentences %>%
  group_by(tgrep_id) %>%
  summarize(Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,OrderedTGrep=fct_reorder(tgrep_id,Mean))

sentences = sentences %>%
  left_join(agr,by=c("tgrep_id"))

ggplot(agr, aes(x=Mean)) +
  geom_histogram() +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(agr, aes(x=OrderedTGrep,y=Mean)) +
  geom_bar(stat="identity",color="black",fill="lightblue3") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25) +
  geom_point(data=sentences, aes(y=rating),color="gray40",alpha=.2) +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

#strange sentences
ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")+
  theme(plot.title = element_text(hjust =0.5))

# how many judgments per item?
table(pilot$tgrep_id)
