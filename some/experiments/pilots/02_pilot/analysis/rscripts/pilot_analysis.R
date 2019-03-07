library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('..')

df = read.csv("pilot.csv", header = TRUE)
theme_set(theme_bw())

#EXCLUDE WORKERID = 0,1,2 --> THEY WEREN'T PAID
df <-df[!(df$workerid %in% c("0","1","2")),]
  
#EXCLUDE WORKERS --> TIME
df <-df[!(df$workerid %in% c("")),]

#EXCLUDE WORKERS --> ASSESMENT
df <-df[!(df$workerid %in% c("")),]

#EXCLUDE WORKERS  --> VARIABILITY
df <-df[!(df$workerid %in% c("3","11","12","16")),]

#EXCLUDE SENTENCES --> STRANGE
df <-df[!(df$tgrep_id %in% c("1111:11")),]

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

bad_turker1 = attn %>% 
  filter(tgrep_id == "control1" & rating < 0.5)
bad_turker2 = attn %>% 
  filter(tgrep_id == "control2" & rating > 0.5) 
bad_turker3 = attn %>% 
  filter(tgrep_id == "control3" & rating < 0.5)
bad_turker4 = attn %>% 
  filter(tgrep_id == "control4" & rating > 0.5)
bad_turker <- rbind(bad_turker1,bad_turker2,bad_turker3,bad_turker4)

table(bad_turker$tgrep_id, bad_turker$workerid)

#to see in which half the controls appeared
ggplot(attn,aes(x=rating, fill=as.factor(half)))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for attention checks")

#how many did each worker get wrong
bad_turker %>%
  count(workerid) %>%
  filter(n > 0)

#EXCLUDE WORKERS --> ATTENTION CHECKS
df <-df[!(df$workerid %in% c("8","15")),]

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
  labs(title="Ratings for practice trials") +
  
#how many judgments per item?
table(df$tgrep_id)
table(practice$tgrep_id, practice$workerid)

#practice and control items comparison
non_target = rbind(bad_turker,practice)
table(non_target$tgrep_id, non_target$workerid)

#TARGET TRIALS
ggplot(sentences,aes(x=rating))+
  geom_histogram()+
  labs(title="Ratings for target trials")+

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
ggsave(file="graphs/mean_rating.pdf",width=8,height=3)
ggsave(file="graphs/mean_rating_excluded.pdf",width=8, height=3)
ggsave(file="graphs/mean_rating_excluded_10.pdf",width=8, height=3)

#detect outliers that are x SD away from the mean
variableturkers = sentences %>%
  mutate(VarianceOutlier = (rating < Mean - 2*SD | rating > Mean + 2*SD)) %>%
  group_by(workerid) %>%
  count(VarianceOutlier) %>%
  filter(VarianceOutlier == TRUE & n > 1)

variableturkers

#look at outliers on selected items
sentences[sentences$tgrep_id == "3878:16" & sentences$rating < .75,] # workerid 12
sentences[sentences$tgrep_id == "1206:10" & sentences$rating < .75,] # workerid 12
sentences[sentences$tgrep_id == "5480:20" & sentences$rating < .8,] # workerid 12, 5

#strange sentences
ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")+
  theme(plot.title = element_text(hjust =0.5))
