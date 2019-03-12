library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')

df = read.csv("pilot.csv", header = TRUE)
theme_set(theme_bw())

#EXCLUDE WORKERS --> TIME - less than 5 minutes (4,4,2)
df <-df[!(df$workerid %in% c("0","1")),]

#EXCLUDE WORKERS --> ASSESMENT - "No"
#df <-df[!(df$workerid %in% c("4","11")),]

#EXCLUDE WORKERS  --> VARIABILITY - 2 SD away from mean in one item
df <-df[!(df$workerid %in% c("9","11","13","6")),]

#EXCLUDE WORKERS  --> ATTENTION CHECK - more than 2 wrong
#df <-df[!(df$workerid %in% c()),]

#EXCLUDE SENTENCES --> STRANGE
#df <-df[!(df$tgrep_id %in% c("")),]

#to determine first&second half of experiment                    
df$trial <- df$slide_number_in_experiment - 7
df$half <- ifelse(df$slide_number_in_experiment < 13,1,2)

#ATTENTION CHECKS 
attn = df %>% 
  filter(str_detect(tgrep_id, "control"))

ggplot(attn,aes(x=rating, fill=as.factor(workerid)))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for attention checks")

ggsave(file="graphs/attention_check_ratings.pdf",width=8,height=3)

avg = attn %>%
  group_by(tgrep_id) %>%
  summarize(Mean=mean(rating))

avg
  
wrong = attn %>% 
  mutate(WrongAnswer = (tgrep_id == "control1" & rating < 0.5 | tgrep_id == "control2" & rating > 0.5 | tgrep_id == "control3" & rating < 0.5 | tgrep_id == "control4" & rating > 0.5)) #%>% 
  group_by(workerid) %>% 
  count(WrongAnswer) %>% 
  filter(WrongAnswer == TRUE)
wrong
wrong$WrongAnswer

#to see in which half the controls appeared
ggplot(attn,aes(x=rating, fill=as.factor(half)))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for attention checks")

#####################################################################
#practice trials
practice = df %>% 
  filter(str_detect(tgrep_id, "example"))
#target trials
sentences = df %>% 
  filter(str_detect(tgrep_id,"example",negate = TRUE))
sentences = sentences %>% 
  filter(str_detect(tgrep_id,"control",negate = TRUE))
#####################################################################

#ALL TRIALS - includes practice,attention check,target
ggplot(df,aes(x=rating))+
  geom_histogram()+
  labs(title="All Ratings")

#PRACTICE TRIALS
ggplot(practice,aes(x=rating))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title="Ratings for practice trials")
  
#how many judgments per item?
table(df$tgrep_id)
table(practice$tgrep_id, practice$workerid)

#TARGET TRIALS
ggplot(sentences,aes(x=rating))+
  geom_histogram()+
  labs(title="Ratings for target trials")

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
  summarize(Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating),SD=sd(rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,OrderedTGrep=fct_reorder(tgrep_id,Mean))

sentences = sentences %>%
  left_join(agr,by=c("tgrep_id"))

ggplot(agr, aes(x=Mean)) +
  geom_histogram() +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(agr, aes(x=OrderedTGrep,y=Mean)) +
  geom_bar(stat="identity",fill = "lightblue3") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax),width=.25) +
  geom_point(data=sentences, aes(y = rating,color = as.factor(workerid)),alpha=.2) + #,color="gray40",alpha=.2) +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

#save graphs with different exclusions
ggsave(file="../graphs/mean_rating.pdf",width=8,height=3)
ggsave(file="../graphs/mean_rating_extime5.pdf",width=8, height=3)
ggsave(file="../graphs/mean_rating_exvar2.pdf",width=8, height=3)

#detect outliers that are x SD away from the mean
variableturkers = sentences %>%
  mutate(VarianceOutlier = (rating < Mean - 2*SD | rating > Mean + 2*SD)) %>%
  group_by(workerid) %>%
  count(VarianceOutlier) %>%
  filter(VarianceOutlier == TRUE & n > 0)

variableturkers

#look at outliers on selected items
sentences[sentences$tgrep_id == "1214:19" & sentences$rating < .5,] # workerid 9 and 11
sentences[sentences$tgrep_id == "2454:46" & sentences$rating < .5,] # workerid 13
sentences[sentences$tgrep_id == "4457:24" & sentences$rating > .8,] # workerid 

#strange sentences
ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")+
  theme(plot.title = element_text(hjust =0.5))
