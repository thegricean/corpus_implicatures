library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())
df = read.csv("results_formatted.csv", header = TRUE)

#LANGUAGE
s = read.csv("subject-info_merged.csv", header = TRUE)

lang = s %>%
  select(workerid, language) %>%
  unique()

#  What about "x and English", "canada", "texas" -- FIX
non_native = lang %>%
  filter(!(language %in% c("English","english","ENGLISH","English language","Englsih", "United States","englsh","ENG","enhlish","englsi","englishjm", "english ","English ","ENGLISH ", "American English", "English  "))) %>%
  select(workerid)

non_native

#EXCLUDE WORKERS --> LANGUAGE - non-native (33 people!!!)
df <-df[!(df$workerid %in% non_native$workerid),]

#TIME
times = df %>%
  select(workerid, Answer.time_in_minutes, speaker_cond) %>%
  unique()

ggplot(times, aes(x=Answer.time_in_minutes)) +
  geom_histogram()

fast_workers = times %>%
  filter(Answer.time_in_minutes<4) %>%
  select(workerid)
fast_workers

ggplot(times %>% filter(speaker_cond == "exp15"), aes(x=Answer.time_in_minutes, fill=speaker_cond))+
  geom_histogram()+
  theme(legend.position="none")

times %>% 
  filter(Answer.time_in_minutes<6)  %>%
  count(speaker_cond) %>%
  arrange(desc(n))

ms = times  %>%
  group_by(speaker_cond) %>%
  mutate(Mean = mean(Answer.time_in_minutes),  Median = median(Answer.time_in_minutes))

ggplot(ms, aes(Mean)) +
  geom_histogram()

ggplot(ms, aes(Median)) +
  geom_histogram()

ggplot(ms, aes(x=Mean,y=Median)) +
  geom_point() +
  geom_text(aes(label=speaker_cond),nudge_x = .3, nudge_y= -.3)

#EXCLUDE WORKERS --> TIME - less than 6 minutes
df <-df[!(df$workerid %in% fast_workers$workerid),]

#ATTENTION CHECKS                   
df$trial <- df$slide_number_in_experiment - 7
df$half <- ifelse(df$slide_number_in_experiment < 13,1,2)

attn = df %>% 
  filter(str_detect(tgrep_id, "control"))

ggplot(attn,aes(x=rating)) +
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for attention checks")

ggsave(file="../graphs/attention_check_ratings.pdf",width=8,height=3)

wrong_control = attn %>% 
  mutate(WrongAnswer = (tgrep_id == "control1" & rating < 0.5 | tgrep_id == "control2" & rating > 0.5 | tgrep_id == "control3" & rating < 0.5 | tgrep_id == "control4" & rating > 0.5)) %>% 
  group_by(workerid) %>% 
  count(WrongAnswer) %>% 
  filter(WrongAnswer == TRUE & n>2) %>%
  select(workerid)
wrong_control

#to see in which half the controls appeared
ggplot(attn,aes(x=rating, fill=as.factor(half)))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title = "Ratings for attention checks")

#EXCLUDE WORKERS  --> ATTENTION CHECK - wrong answer to more than 2 items
df <-df[!(df$workerid %in% wrong_control$workerid),]

#####################################################################
#target trials
sentences = df %>% 
  filter(str_detect(tgrep_id,"example",negate = TRUE)) %>% 
  filter(str_detect(tgrep_id,"control",negate = TRUE))
#####################################################################

#VARIANCE
# collect information on Turkers' overall response distribution variance
variances = sentences %>%
  group_by(workerid)  %>%
  summarize(Var = var(rating)) %>%
  mutate(SmallVariance = Var < mean(Var) - 2*sd(Var)) #%>%
  filter(SmallVariance == TRUE) %>%
  select(workerid)
variances

summary(variances)

#EXCLUDE WORKERS  --> VARIANCE - variance smaller than mean(variance)-2sd
df <-df[!(df$workerid %in% variances$workerid),]

sentences = df %>% 
  filter(str_detect(tgrep_id,"example",negate = TRUE)) %>% 
  filter(str_detect(tgrep_id,"control",negate = TRUE))

#MEDIAN VARIABILITY
#overall distribution of responses
agr = sentences %>%
  group_by(tgrep_id) %>%
  summarize(Median=median(rating),Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating),SD=sd(rating),Var=var(rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh,OrderedTGrep=fct_reorder(tgrep_id,Median))

sentences = sentences %>%
  left_join(agr,by=c("tgrep_id"))

#Create CSV with 4 columns: tgrep_id, mean, median, variance
d = agr %>% 
  select(tgrep_id, Mean, Median, Var)

write.csv(d, file = "summary.csv")

ggplot(agr, aes(x=Mean)) +
  geom_histogram() +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5))

ggplot(agr, aes(x=OrderedTGrep,y=Mean)) +
  geom_bar(stat="identity",fill = "lightblue3") +
  geom_errorbar(aes(ymin = YMin, ymax = YMax),width=.25) +
  #geom_point(data=sentences, aes(y = rating,color = as.factor(workerid)),alpha=.2) + #,color="gray40",alpha=.2) +
  geom_point(aes(y=Median),color="orange",size=2) +
  labs(title = "Mean rating by item")+
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

lowmedians = as.character(agr[agr$Median < .1,]$tgrep_id)
lowmedians
lowmeans = as.character(agr[agr$Mean <.28,]$tgrep_id)
lowmeans 

highmedians = as.character(agr[agr$Median > .95,]$tgrep_id)
highmedians
highmeans = as.character(agr[agr$Mean >.78,]$tgrep_id)
highmeans

#save graphs with different exclusions
ggsave(file="../graphs/mean_rating.pdf",width=35,height=3)
ggsave(file="../graphs/mean_rating_extime7.pdf",width=8, height=3)
ggsave(file="../graphs/mean_rating_exvar2.pdf",width=8, height=3)

#detect outliers that are x SD away from the median
variableturkers = sentences %>%
  mutate(VarianceOutlier = (rating < Median - 2*SD | rating > Median + 2*SD)) %>%
  group_by(workerid) %>%
  count(VarianceOutlier) %>%
  filter(VarianceOutlier == TRUE & n > 2) %>%
  select(workerid)
variableturkers

#look at outliers on selected items
sentences[sentences$tgrep_id == "3878:16" & sentences$rating < .5,]$workerid # 
sentences[sentences$tgrep_id == "2454:46" & sentences$rating < .5,]$workerid #
sentences[sentences$tgrep_id == "4457:24" & sentences$rating > .8,]$workerid #

#EXCLUDE WORKERS  --> VARIABILITY - 2 SD away from median in more than two items
df <-df[!(df$workerid %in% variableturkers$workerid),]

sentences = df %>% 
  filter(str_detect(tgrep_id,"example",negate = TRUE)) %>% 
  filter(str_detect(tgrep_id,"control",negate = TRUE))

#ALL TRIALS - includes practice,attention check,target
ggplot(df,aes(x=rating))+
  geom_histogram()+
  labs(title="All Ratings")

#PRACTICE TRIALS
practice = df %>% 
  filter(str_detect(tgrep_id, "example")) %>%
  filter(str_detect(tgrep_id, "examplecheckbox", negate = TRUE))

ggplot(practice,aes(x=rating, fill=as.factor(workerid)))+
  geom_histogram()+
  facet_wrap(~tgrep_id) +
  labs(title="Ratings for practice trials")

#how many judgments per item?
table(df$tgrep_id)

#wrong answers to practice trials
wrong_practice = practice %>% 
  mutate(WrongAnswer = (tgrep_id == "example1" & rating < 0.5 | tgrep_id == "example2" & rating > 0.5 | tgrep_id == "example3" & rating < 0.5 | tgrep_id == "example4" & rating < 0.5 | tgrep_id == "example5" & rating > 0.5))  #%>% 
  group_by(tgrep_id) %>% 
  count(WrongAnswer) %>% 
  filter(WrongAnswer == TRUE)
wrong_practice

#wrong answers and true/false/not_sure
ggplot(wrong_practice,aes(x=strange, fill=as.factor(WrongAnswer)))+
  geom_bar() +
  facet_wrap(~tgrep_id) +
  labs(title="Ratings for practice trials") 

#wrong answers to practice trials and controls 
wrong = rbind(wrong_practice,wrong_control)
table(wrong$workerid,wrong$tgrep_id,wrong$WrongAnswer)

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

#strange sentences
ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")
