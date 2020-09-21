library(tidyverse)
source("helpers.R")
theme_set(theme_bw())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df = read.csv("../data/trials_all.csv", header = TRUE)

# load original context dataset
d = read.csv("../../../meta_analysis/data/some_data.csv")

#get rid of bot check and example trials
df = df %>%
  filter(tgrep_id != "bot_check") %>%
  filter(tgrep_id != "example1") %>%
  filter(tgrep_id != "example2")

agr = df %>%
  filter(!tgrep_id == "bot_check") %>%
  filter(!tgrep_id == "example1") %>%
  filter(!tgrep_id == "example2") %>%
  mutate(rating = as.numeric(as.character(rating)))%>%
  group_by(tgrep_id) %>%
  summarise(Mean=mean(rating),CILow=ci.low(rating),CIHigh=ci.high(rating),SD=sd(rating),Var=var(rating),count=n()) %>%
  ungroup()%>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  select(tgrep_id,Mean,Var,YMin,YMax)

nrow(agr)
write_csv(agr, path="../data/some_without_context_means.csv")

# join item info from old dataset
d = d %>%
  select(StrengthSome,logSentenceLength,BinaryGF,redInfoStatus,Modification,Partitive,Item) %>%
  unique() %>%
  rename(tgrep_id="Item")

#re-run regression model on no-context means
df = df %>%
  left_join(d,by=c('tgrep_id')) %>%
  mutate(rating = as.numeric(as.character(rating)))

centered = cbind(df, myCenter(df[,c("StrengthSome","logSentenceLength","BinaryGF","Modification","Partitive","redInfoStatus")]))

m.fixed.nocontext = lmer(rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credInfoStatus|workerid) + (0 + cBinaryGF|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|tgrep_id), data=centered)
summary(m.fixed.nocontext)
