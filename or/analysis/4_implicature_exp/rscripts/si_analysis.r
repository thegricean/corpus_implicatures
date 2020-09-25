require(tidyverse)
library(stringr)
library(magrittr)
library(lmerTest)
library(MuMIn)
# library(brms) # run later

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw(18))

source("helpers.R")

# load data
# 9-13 data points per tgrep_id
d = read_csv("../../../experiments/4_implicature_exp/results/data/data.csv") %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(workerid = as.factor(as.character(workerid))) %>%
  filter(!tgrep.id %in% c("bot_check","example1","example2")) %>% 
  droplevels()
nrow(d) # 14327 data points; 12500 without practice trials; 609 participants if you divide 1827 removed practice trials by 3; but 420 participants if you ask for unique(d$workerid) -- why the discrepancy?

# merge in additional annotation data (automatically extracted features plus cindy & tru & neele's & lexi & jane & leyla's annotations, see annotation_manual.docx for details)
cases = read_csv("../../../discourse_function_annotation/data/case-features.csv")

d %<>%
  left_join(cases,by=c("tgrep.id")) %>%
  select(-n,-Cluster,-cluster,-PrototypicalSentence8) %>% 
  # na.omit() %>%
  # filter(And_test != "idiom" & gender != "Other") %>%
  droplevels()

# merge in additional annotation data (elissa's sentence weirdness annotation)
weird_cases = cases = read_csv("../../../discourse_function_annotation/annotation_el/hand_annotated_v1.csv") %>% 
  select(tgrep.id, Opt_BNB,Weird,Better_BNB,Weird_Category) # better BNB sentence already included!

d %<>%
  left_join(weird_cases,by=c("tgrep.id"))

# downward-entailing operators: group antecedents of conditionals, negation, negative predicates, relative clauses, and questions together
d = d %>%
  mutate(DE = as.factor(case_when(
    DE_operator == "antecedent" ~ "DE",
    DE_operator == "negation" ~ "DE",
    DE_operator == "negpredicate" ~ "DE",
    # DE_operator == "relclause" ~ "DE",
    # DE_operator == "wh" ~ "DE",
    TRUE ~ "UE"
  )))
table(d$DE_operator)
table(d$DE)

# collapse relevant sentence types into declarative and non-declarative
d$redSentenceType = as.factor(ifelse(d$Sentence_type == "declarative","declarative","not-declarative"))
table(d$Sentence_type)
table(d$redSentenceType)
d$logSentenceLength = log(d$SentenceLength)

# calculate means and write to file; write full dataset to file
# TODO

write_csv(d, path="../data/raw_data_full.csv")
# write_csv(means, path="../data/means_full.csv")

#### ANALYSIS
# visual exploration of feature effects on SI rating
cbPalette <- c("#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73","#56B4E9", "#D55E00", "#009E73","#999999", "#E69F00","#009E73")

# categorical variables, raw judgments
for (p in c("redSentenceType","Numeral","And_test","Or_both_test","Actually_test","Disjunct_relation","Disjunct_individuation","eitherPresent","orNotPresent","DE","DE_operator","Weird","Weird_Category")) {
  
  toplot = d %>%
    group_by(.dots=p) %>%
    summarize(SIMean=mean(response_val),CILow=ci.low(response_val),CIHigh=ci.high(response_val)) %>% 
    ungroup() %>% 
    mutate(YMin=SIMean-CILow,YMax=SIMean+CIHigh) %>% 
    filter(!is.na(SIMean)) %>% 
    droplevels()
  
  tmp = d[!is.na(d[,p]),]
  
  ggplot(tmp %>% droplevels(), aes_string(x="response_val",fill=p)) +
    # geom_histogram(position="dodge") +#,alpha=.5) +
    geom_density(alpha=.5) +
    # geom_point(data=toplot_means,y=20,aes(x=Mean,color=Value)) +
    # geom_errorbarh(data=toplot_means,aes(xmin=YMin,xmax=YMax,x=Mean,y=20,color=Value)) +
    ylab("Density") +
    xlab("Inference strength rating") +
    geom_point(data=toplot, aes_string(x="SIMean",y=1,color=p)) +
    geom_errorbarh(data=toplot, aes(x=SIMean,xmin=YMin,xmax=YMax,y=1),height=.25) +
    scale_fill_manual(values=cbPalette) +
    scale_color_manual(values=cbPalette)
    # scale_color_manual(values=cbPalette)
  ggsave(file=paste("../graphs/simeans/simeans_",p,"_density.pdf",sep=""),height=3.5,width=7)
}

# categorical variables, by-item judgments
for (p in c("redSentenceType","Numeral","And_test","Or_both_test","Actually_test","Disjunct_relation","Disjunct_individuation","eitherPresent","orNotPresent","DE","DE_operator","Weird","Weird_Category")) {
  
  id = "tgrep.id"
  
  agr = d %>%
    group_by(.dots=c(id,p)) %>% 
    summarize(MeanByItem = mean(response_val)) %>% 
    ungroup()
    
  toplot = agr %>% 
    group_by(.dots=p) %>%
    summarize(SIMean=mean(MeanByItem),CILow=ci.low(MeanByItem),CIHigh=ci.high(MeanByItem)) %>% 
    ungroup() %>% 
    mutate(YMin=SIMean-CILow,YMax=SIMean+CIHigh) %>% 
    filter(!is.na(SIMean)) %>% 
    droplevels()
  
  tmp = agr[!is.na(agr[,p]),]
  
  ggplot(tmp %>% droplevels(), aes_string(x="MeanByItem",fill=p)) +
    geom_histogram(position="dodge") +#,alpha=.5) +
    # geom_density(alpha=.5) +
    # geom_point(data=toplot_means,y=20,aes(x=Mean,color=Value)) +
    # geom_errorbarh(data=toplot_means,aes(xmin=YMin,xmax=YMax,x=Mean,y=20,color=Value)) +
    ylab("Number of cases") +
    xlab("Mean by-item rating") +
    geom_point(data=toplot, aes_string(x="SIMean",y=10,color=p)) +
    geom_errorbarh(data=toplot, aes(x=SIMean,xmin=YMin,xmax=YMax,y=10),height=.25) +
    scale_fill_manual(values=cbPalette) +
    scale_color_manual(values=cbPalette)
  # scale_color_manual(values=cbPalette)
  ggsave(file=paste("../graphs/simeans/simeans_",p,"_byitem.pdf",sep=""),height=3.5,width=7)
}

agr = d %>%
  select(SIMean,PropGoodSentence,logSentenceLength,Trial) %>%
  gather(Predictor,Value, -SIMean)

ggplot(agr, aes(x=Value,y=SIMean)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~Predictor,scales="free_x") 
ggsave(file="../graphs/simeans/simeans_continuous.pdf",height=4,width=10)

## REGRESSION ANALYSIS
d %<>% 
  mutate_if(is.character,as.factor) 

d$Or_both_test = relevel(d$Or_both_test,"nochange")
contrasts(d$Or_both_test)
contrasts(d$Weird) = c(1,0)
contrasts(d$Disjunct_relation)

d = as.data.frame(d)

centered = cbind(d,myCenter(d[,c("response_goodsentence","redSentenceType", "DE", "Numeral", "And_test", "Actually_test", "eitherPresent", "orNotPresent", "logSentenceLength", "age", "Trial", "gender","Disjunct_individuation","Weird")])) # fix weird centering (NA values)

table(centered$And_test,centered$cAnd_test) # positive: no change
table(centered$Numeral,centered$cNumeral) # positive: numeral
table(centered$redSentenceType,centered$credSentenceType) # positive: not-declarative
table(centered$Actually_test,centered$cActually_test) # positive: no change
table(centered$Disjunct_individuation,centered$cDisjunct_individuation) # positive: same
table(centered$DE,centered$cDE) # positive: UE
table(centered$eitherPresent,centered$ceitherPresent) # positive: present
table(centered$orNotPresent,centered$corNotPresent) # positive: present


# start with only subject intercept model
m.0 = lmer(response_val ~  (1|workerid), data=centered)
summary(m.0)
d$FittedOnlySubjectVar = fitted(m.0)

means = d %>%
  group_by(tgrep.id) %>%
  summarize(MeanPredicted = mean(FittedOnlySubjectVar),MeanEmpirical = mean(response_val))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(1,7) +
  ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_0.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)

# add fixed effects of interest
m.noitems = lmer(response_val ~  cDE + cAnd_test + Or_both_test + cNumeral + Disjunct_relation + ceitherPresent + corNotPresent + cActually_test + credSentenceType +  cresponse_goodsentence + clogSentenceLength + cTrial + (1|workerid), data=centered) # cDisjunct_individuation
summary(m.noitems)
d$FittedNoCluster = fitted(m.noitems)

means = d %>%
  group_by(tgrep.id) %>%
  summarize(MeanPredicted = mean(FittedNoCluster),MeanEmpirical = mean(response_val))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(1,7) +
  ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)
r.squaredGLMM(m.noitems) 
vif.mer(m.noitems) # no relevant collinearity

# add fixed effects of interest plus random item variability
m.items = lmer(response_val ~  cDE + cAnd_test + Or_both_test + cNumeral + Disjunct_relation + ceitherPresent + corNotPresent + cActually_test + credSentenceType +  cresponse_goodsentence + clogSentenceLength + cTrial + (1|workerid) + (1|tgrep.id), data=centered) 
summary(m.items)
d$FittedNoClusterItem = fitted(m.items)

means = d %>%
  group_by(tgrep.id) %>%
  summarize(MeanPredicted = mean(FittedNoClusterItem),MeanEmpirical = mean(response_val))
ggplot(means, aes(x=MeanPredicted,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(1,7) +
  ylim(1,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_full.pdf",width=5,height=4)
cor(means$MeanEmpirical,means$MeanPredicted)
