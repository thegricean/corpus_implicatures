library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read.csv("pilot.csv", header = TRUE)
source("helpers.R")

# dataframes with workers excluded
# d$asses = "No"
dno = df %>% 
  filter(workerid != "11", workerid != "13")
# d$asses = "Confused"
dco = dno %>% 
  filter(workerid != "6")

# ALL TRIALS
ggplot(df,aes(x=rating))+
  geom_histogram()+
  labs(title = "All Ratings")+
  theme(plot.title = element_text(hjust =0.5))

# workers excluded
ggplot(dno,aes(x=rating))+
  geom_histogram()+
  labs(title = "All Ratings")+
  theme(plot.title = element_text(hjust =0.5))

# PRACTICE TRIALS
practice = df %>% 
  filter(str_detect(tgrep_id, "example"))

ggplot(practice, aes(x=rating,fill=as.factor(tgrep_id))) +
  geom_density(alpha=.5)

ggplot(practice,aes(x=rating))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for practice trials") +
  theme(plot.title = element_text(hjust =0.5))

# workers excluded
p = dno %>% 
  filter(str_detect(tgrep_id, "example"))

ggplot(p,aes(x=rating))+
  geom_histogram()+
  facet_wrap(~tgrep_id)+
  labs(title = "Ratings for practice trials")+
  theme(plot.title = element_text(hjust =0.5))

# TARGET TRIALS
sentences = df %>% 
  filter(str_detect(tgrep_id, "example", negate = TRUE))

ggplot(sentences,aes(x=rating))+
  geom_histogram()+
  labs(title = "Ratings for target trials")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(sentences, aes(x=rating,fill=tgrep_id)) +
  geom_density(alpha=.5)

# workers excluded
s = dno %>% 
  filter(str_detect(tgrep_id, "example", negate = TRUE))

ggplot(s,aes(x=rating))+
  geom_histogram()+
  labs(title = "Ratings for target trials - workers excluded")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(s, aes(x=rating,fill=tgrep_id)) +
  geom_density(alpha=.5)

# for each item
ggplot(sentences, aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~tgrep_id)

# for each participant
ggplot(sentences, aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~workerid)

# overall distribution of responses
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

ggsave(file = "graphs/mean_rating.pdf", width = 8, height = 3)

# overall distribution of responses - workers excluded
ag = s %>%
  group_by(tgrep_id) %>%
  summarize(Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,OrderedTGrep=fct_reorder(tgrep_id,Mean))
s = s %>%
  left_join(ag,by=c("tgrep_id"))

ggplot(ag, aes(x=OrderedTGrep,y=Mean)) +
  geom_bar(stat="identity",color="black",fill="lightblue3") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25) +
  geom_point(data=sentences, aes(y=rating),color="gray40",alpha=.2) +
  labs(title = "Mean rating by item  - workers excluded")+
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

# change in means with exclusion------------------------------FIX IT----------------
agg = df %>%
  filter(str_detect(tgrep_id, "example", negate = TRUE)) %>% 
  mutate(exclude = ifelse(workerid==11 | workerid==13, T, F)) %>% 
  group_by(tgrep_id) %>%
  summarize(Mean_all=mean(rating),
            CILow_all=ci.low(rating),
            CIHigh_all=ci.high(rating),
            Mean_exclude=mean(ifelse(!exclude,rating,NA),na.rm=TRUE),
            CILow_exclude=ci.low(ifelse(!exclude,rating,NA)),
            CIHigh_exclude=ci.high(ifelse(!exclude,rating,NA))) %>% 
  ungroup() %>% 
  gather(Mean_part, Mean, Mean_all, Mean_exclude)

agg$CILow <- ifelse(agg$Mean_part == "Mean_all", agg$CILow_all, 0)
agg$CIHigh <- ifelse(agg$Mean_part == "Mean_all", agg$CIHigh_all, 0)
agg$CILow <- ifelse(agg$Mean_part == "Mean_exclude", agg$CILow_exclude, agg$CILow)
agg$CIHigh <- ifelse(agg$Mean_part == "Mean_exclude", agg$CIHigh_exclude, agg$CIHigh)

#agg$YMin = agg$Mean-agg$CILow
#agg$YMax = agg$Mean+agg$CIHigh

agg = agg %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,OrderedTGrep=fct_reorder(tgrep_id,Mean))
agg$Mean_part

ggplot(agg, aes(x=tgrep_id,y=Mean, fill =Mean_part)) +
  geom_bar(stat="identity",color="black",fill="lightblue3", position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),width=.25) +
  geom_point(data=sentences, aes(y=rating),color="gray40",alpha=.2) +
  labs(title = "Mean rating by item  - workers excluded")+
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

###########################################################################

#strange sentences
ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")+
  theme(plot.title = element_text(hjust =0.5))

# how many judgments per item?
table(df$tgrep_id)

