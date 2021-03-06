library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())
df = read.csv("results_merged.csv", header = TRUE) 
# 98 people total

times = df %>%
  select(workerid,Answer.time_in_minutes,speaker_cond) %>%
  unique()

ggplot(times, aes(x=Answer.time_in_minutes)) +
  geom_histogram()

fast_workers = times %>%
  filter(Answer.time_in_minutes<6) %>%
  select(workerid)

#EXCLUDE WORKERS --> TIME - less than 6 minutes --> 24 people
df <-df[!(df$workerid %in% fast_workers$workerid),]

#EXCLUDE WORKERS  --> VARIANCE - 2 SD less than mean variance
df <-df[!(df$workerid %in% variableturkers$workerid),]

#EXCLUDE WORKERS  --> VARIABILITY(MEDIAN) - 2 SD away from median in more than 2 items
df <-df[!(df$workerid %in% variableturkers$workerid),]

#EXCLUDE SENTENCES --> STRANGE - 
#df <-df[!(df$tgrep_id %in% c()),]

#####################################################################

#ALL TRIALS - includes practice,attention check,target
ggplot(df,aes(x=rating))+
  geom_histogram()+
  labs(title="All Ratings")

#TARGET TRIALS
sentences = df %>% 
  filter(str_detect(tgrep_id,"example",negate = TRUE))
sentences = sentences %>% 
  filter(str_detect(tgrep_id,"control",negate = TRUE))

ggplot(sentences,aes(x=rating))+
  geom_histogram()+
  labs(title="Ratings for target trials")

ggplot(sentences, aes(x=rating,fill=tgrep_id)) +
  geom_density(alpha=.5)

#for each item
ggplot(sentences, aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~tgrep_id)

# collect information on Turkers' overall response distribution variance
variances = sentences %>%
  group_by(workerid)  %>%
  summarize(Var = var(rating)) %>%
  mutate(SmallVariance = Var < mean(Var) - 2*sd(Var)) #%>%
  filter(SmallVariance == TRUE)

summary(variances)

#overall distribution of responses
agr = sentences %>%
  group_by(tgrep_id) %>%
  summarize(Median=median(rating),Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating),SD=sd(rating)) %>%
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
  # geom_point(data=sentences, aes(y = rating,color = as.factor(workerid)),alpha=.2) + 
  geom_point(aes(y=Median),color="orange",size=4) +
  labs(title = "Mean rating by item") +
  theme(plot.title = element_text(hjust =0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=1))

#save graphs with different exclusions
ggsave(file="../graphs/mean_rating.pdf",width=8,height=3)
ggsave(file="../graphs/mean_rating_extime6.pdf",width=8, height=3)
ggsave(file="../graphs/mean_rating_exvar2.pdf",width=8, height=3)

#detect outliers that are x SD away from the median
variableturkers = sentences %>%
  mutate(VarianceOutlier = (rating < Median - 2*SD | rating > Median + 2*SD)) %>%
  group_by(workerid) %>%
  count(VarianceOutlier) %>%
  filter(VarianceOutlier == TRUE & n > 1) #%>%
  select(workerid)

#strange sentences
sentences$strange = str_replace(sentences$strange,"true","True")
sentences$strange = str_replace(sentences$strange,"false","False")

ggplot(sentences,aes(x=tgrep_id, fill=strange))+
  geom_bar(position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(title = "\"This sentence sounds strange\"",
       x = "TGrep ID")
