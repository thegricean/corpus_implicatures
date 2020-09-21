library(tidyverse)
library(Hmisc)
library(gridExtra)
library(MuMIn)
library(ggExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# load the main data
d = read_tsv("../data/some_data.csv")

# load the no-context means
d_nocontext = read.csv("../../main_experiments/some_without_context/data/some_without_context_means.csv")
colnames(d_nocontext) = c("Item","Mean_nocontext","Var_nocontext","YMin_nocontext","YMax_nocontext")

d = d %>%
  left_join(d_nocontext,by=c("Item"))

# load the speaker knowledge ratings -- these only exist for the cases where independent annotators (Leyla & Lexi) decided that "some" can be replaced by "all" and not just by "a lot"
d_knowledge = read.csv("../../main_experiments/speaker_knowledge/data/means_speakerknowledge.csv")
colnames(d_knowledge) = c("X","Item","Mean_knowledge","Median_knowledge","Var_knowledge")
d_knowledge$X = NULL

d = d %>%
  left_join(d_knowledge,by=c('Item')) %>%
  mutate(Mean_knowledge = replace_na(Mean_knowledge, -.5),Median_knowledge = replace_na(Median_knowledge, -.5),Var_knowledge = replace_na(Var_knowledge, -.5)) %>%
  mutate(Item = as.factor(as.character(Item))) %>%
  mutate(AllAlternative=as.factor(case_when(
    Mean_knowledge == -.5 ~ "all_notalt",
    TRUE ~"all_alt")))
summary(d)

# writing needs to happen only once (or every time additional annotations are added above):
# write_tsv(d,path="../data/some_data_allannotations.tsv")

itemmeans = d %>%
  group_by(Item,Sentence,AllAlternative) %>%
  summarise(Mean=mean(r_SNA))

# get item with low mean but "all" is alternative
itemmeans[itemmeans$Mean < 2.5 & itemmeans$AllAlternative == "all_alt",] %>%
  arrange(Mean) %>%
  select(Sentence)
itemmeans[itemmeans$Item == "6686:61",]$Sentence


# plot mean inference strength as a function of whether or not "some" can be replaced by "all"
means = d %>%
  group_by(AllAlternative) %>%
  summarise(Mean = mean(Rating),CILow=ci.low(Rating),CIHigh=ci.high(Rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

item_means = d %>%
  group_by(AllAlternative,Item) %>%
  summarise(Mean = mean(Rating))

p = ggplot(means,aes(x=AllAlternative,y=Mean,fill=AllAlternative,color=AllAlternative)) +
  geom_jitter(data=item_means,alpha=.5) +
  geom_point(color="black") +
  scale_y_continuous(limits=c(1,7),breaks=seq(1,7),name="Mean inference rating") +
  scale_x_discrete("Is 'all' an alternative?",labels=c("yes","no")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.1,color="black") +
  theme(legend.position="none")
pfull <- ggExtra::ggMarginal(p, type = "density", margins = "y", groupColour = TRUE, groupFill = TRUE)
ggsave(pfull,file="../graphs/means_by_alternative.pdf",height=3,width=6)

# get mean ratings for example sentences
mean(d[d$Sentence == "that would take some planning",]$r_SNA)
mean(d[d$Sentence == "you sound like you've got some small ones in the background.",]$r_SNA)
mean(d[d$Sentence == "and i've got some broccoli, uh, onions some, some radishes and, uh, uh, beets.",]$r_SNA)

# re-generate histogram of all means and then color by whether or not "some" can be replaced by "all"
means = d %>%
  group_by(Item,AllAlternative) %>%
  summarise(Mean = mean(r_SNA))

totalmeans = d %>%
  group_by(AllAlternative) %>%
  summarise(Mean = mean(r_SNA),CILow=ci.low(r_SNA),CIHigh=ci.high(r_SNA)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

ggplot(means,aes(x=Mean)) +
  geom_histogram() +
  scale_x_continuous(name="Mean by-item inference rating",breaks=seq(1,7)) +
  ylim(0,80) +
  ylab("Number of cases")
ggsave(file="../graphs/histogram.pdf",width=4.9,height=3.2)  

ggplot(means,aes(x=Mean,fill=AllAlternative)) +
  geom_histogram(position="identity",alpha=.6) +
  geom_point(data=totalmeans, aes(x=Mean,fill=AllAlternative),y=10, pch = 21, colour = "black", size = 5) +
  # geom_errorbarh(data=totalmeans, aes(xmin=YMin,xmax=YMax,y=10), colour = "black", height=.25) +
  scale_x_continuous(name="Mean by-item inference rating",breaks=seq(1,7)) +
  ylim(0,80) +
  ylab("Number of cases") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"), 
                    name="Is 'all' an\nalternative?",
                    breaks=c("all_alt", "all_notalt"),
                    labels=c("yes", "no"))
ggsave(file="../graphs/histogram_byalt.pdf",width=6,height=3.2)  

agr = d %>%
  mutate(HighLowKnowledge=case_when(
    Median_knowledge > .5 ~ "high",
    Median_knowledge == -.5 ~ "'all' no alt",
    TRUE ~ "low"
  )) %>%#,labels=c("'all' no alt","low","high"))) %>%
  group_by(Item,Median_knowledge,HighLowKnowledge) %>%
  summarise(Mean = mean(Rating)) 
  
p = ggplot(agr, aes(x=Median_knowledge,y=Mean,color=HighLowKnowledge)) +
    geom_point() +
    geom_smooth(method="lm")
pfull = ggExtra::ggMarginal(p, type = "density",groupColour = TRUE, groupFill = TRUE)
ggsave(pfull,file="../graphs/means_by_alternative.pdf",height=3,width=6)

d %>%
  select(AllAlternative,Item) %>%
  unique() %>%
  count(AllAlternative)

# re-create partitive bar plot separately for when "all" is and isn't an alternative
pmeans = d %>%
  group_by(Partitive,AllAlternative) %>%
  summarise(Mean = mean(r_SNA),CILow=ci.low(r_SNA),CIHigh=ci.high(r_SNA)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(AllAlternative = fct_recode(AllAlternative,"'all' is alternative"="all_alt","'all' is not alternative"="all_notalt")) %>%
  mutate(AllAlternative = fct_relevel(AllAlternative,"'all' is not alternative"))
dodge = position_dodge(.9)

ggplot(pmeans,aes(x=Partitive,y=Mean,fill=AllAlternative)) +
  geom_bar(stat="identity",color="black",width=.6) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25,color="black") +
  scale_fill_manual(values=c("#56B4E9","#E69F00")) +
  scale_x_discrete(breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  facet_wrap(~AllAlternative) +
  # ylim(0,80) +
  ylab("Mean inference rating") +
  theme(legend.position="none")
ggsave(file="../graphs/partitive_byalt.pdf",width=4,height=3.2)

# re-create linguistic mention bar plot separately for when "all" is and isn't an alternative
lmeans = d %>%
  group_by(InfoStatus,AllAlternative) %>%
  summarise(Mean = mean(r_SNA),CILow=ci.low(r_SNA),CIHigh=ci.high(r_SNA)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(AllAlternative = fct_recode(AllAlternative,"'all' is alternative"="all_alt","'all' is not alternative"="all_notalt")) %>%
  mutate(AllAlternative = fct_relevel(AllAlternative,"'all' is not alternative")) %>%
  mutate(InfoStatus = fct_relevel(InfoStatus,"new","med")) 

ggplot(lmeans,aes(x=InfoStatus,y=Mean,fill=AllAlternative)) +
  geom_bar(stat="identity",color="black",position=dodge,width=.6) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25,color="black") +
  scale_fill_manual(values=c("#56B4E9","#E69F00")) +
  # scale_x_discrete(breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  facet_wrap(~AllAlternative) +
  xlab("Linguistic mention") +
  # ylim(0,80) +
  ylab("Mean inference rating") +
  theme(legend.position="none")
ggsave(file="../graphs/infostatus_byalt.pdf",width=5,height=3.2)

# re-create subjecthood bar plot separately for when "all" is and isn't an alternative
smeans = d %>%
  group_by(BinaryGF,AllAlternative) %>%
  summarise(Mean = mean(r_SNA),CILow=ci.low(r_SNA),CIHigh=ci.high(r_SNA)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(AllAlternative = fct_recode(AllAlternative,"'all' is alternative"="all_alt","'all' is not alternative"="all_notalt")) %>%
  mutate(AllAlternative = fct_relevel(AllAlternative,"'all' is not alternative")) %>%
  mutate(Subjecthood = as.factor(as.character(fct_recode(as.character(BinaryGF),other="0",subject="1")))) %>%
  mutate(Subjecthood = fct_relevel(Subjecthood,"other")) 

ggplot(smeans,aes(x=Subjecthood,y=Mean,fill=AllAlternative)) +
  geom_bar(stat="identity",color="black",position=dodge,width=.6) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25,color="black") +
  scale_fill_manual(values=c("#56B4E9","#E69F00")) +
  # scale_x_discrete(breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  facet_wrap(~AllAlternative) +
  # ylim(0,80) +
  ylab("Mean inference rating") +
  theme(legend.position="none")
ggsave(file="../graphs/subjecthood_byalt.pdf",width=4,height=3.2)

# re-create strength scatterplot separately for when "all" is and isn't an alternative
strmeans = d %>%
  group_by(StrengthSome,AllAlternative,Item) %>%
  summarise(Mean = mean(r_SNA)) %>%
  ungroup() %>%
  mutate(Strength = 7-StrengthSome) %>%
  mutate(AllAlternative = fct_recode(AllAlternative,"'all' is alternative"="all_alt","'all' is not alternative"="all_notalt")) %>%
  mutate(AllAlternative = fct_relevel(AllAlternative,"'all' is not alternative"))

ggplot(strmeans,aes(x=Strength,y=Mean,color=AllAlternative)) +
  geom_point() +
  scale_color_manual(values=c("#56B4E9","#E69F00")) +
  geom_smooth(method="lm") +
  facet_wrap(~AllAlternative) +
  xlab("Mean strength of 'some") +
  ylab("Mean inference rating") +
  theme(legend.position="none")
ggsave(file="../graphs/strength_byalt.pdf",width=6,height=3.2)


# To run the regression reported in Degen 2015:
library(lmerTest)
centered = cbind(d, myCenter(d[,c("StrengthSome","logSentenceLength","Pronoun","BinaryGF","InfoStatus","DAModification","Modification","Partitive","redInfoStatus","numDA","AllAlternative")]))

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

m = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credInfoStatus|workerid) + (0 + cBinaryGF|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|Item), data=centered)
summary(m)
msummary = summary(m)
coefs = as.data.frame(msummary$coefficients)
summary(coefs)
head(coefs)



m.fixed.allalt = lmer(Rating ~ cAllAlternative*(cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength) + (1|workerid) +  (0 + cAllAlternative|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credInfoStatus|workerid) + (0 + cBinaryGF|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid), data=centered)
masummary = summary(m.fixed.allalt)
coefs = as.data.frame(masummary$coefficients)
summary(coefs)
head(coefs)

createLatexTableLinear(coefs,predictornames=c("Intercept","AllAlternative","Partitive","Strength","Linguistic mention","Subjecthood","Modification","Sentence length","Partitive:Strength","Linguistic mention:Subjecthood","Linguistic mention:Modification","Subjecthood:Modification","AllAlternative:Partitive","AllAlternative:Strength","AllAlternative:Linguistic mention","AllAlternative:Subjecthood","AllAlternative:Modification","AllAlternative:Sentence length","Linguistic mention:Subjecthood:Modification","AllAlternative:Partitive:Strength","AllAlternative:Linguistic mention:Subjecthood","AllAlternative:Linguistic mention:Modification","AllAlternative:Subjecthood:Modification","AllAlternative:Linguistic mention:Subjecthood:Modification"))

# CONTINUE HERE AND BELOW: THE CONFUSING THING IS THAT SPEAKER KNOWLEDGE HAS AN OVERALL NEGATIVE EFFECT ON IMPLICATURE STRENGTH, WHICH IS INTUITIVELY THE OPPOSITE OF WHAT WE SHOULD EXPECT. IE, THE MORE THE SPEAKER IS EXPECTED TO KNOW WHETHER OR NOT THE STRONGER ALTERNATIVE IS TRUE, THE LESS SIMILAR THE COMPARISON UTTERANCE IS TO THE ORIGINAL UTTERANCE?? INTUITIVELY, IN ORDER TO GET A SCALAR IMPLICATURE, THE SPEAKER NEEDS TO KNOW WHETHER THE STRONGER ALTERNATIVE IS TRUE. IF THE SPEAKER DOESN'T KNOW ABOUT TEH STRENGTH OF THE STRONGER ALTERNATIVE, THEN YOU JUST HAVE TO RELY ON YOUR PRIOR TO KNOW WHAT THE LIKELY STATE OF THE WORLD IS, AND THAT COULD OF COURSE GO ANY WHICH WAY. SO ON AVERAGE WE SHOULD EXPECT AMONG THE HIGHER SPEAKER-KNOWLEDGE ITEMS TO FIND VARIABILITY IN IMPLICATURE STRENGTH BECAUSE THE SPEAKER MAY INTEND ONE OR THE OTHER INTERPRETATION; AMONG THE LOWER SPEAKER-KNOWLEDGE ITEMS WE SHOULD FIND VARIABILITY BECAUSE OF THE PRIOR. SO VARIABILITY EITHER WAY -- BUT WHO WOULD HAVE PREDICTED THAT YOU'LL GET WEAKER IMPLICATURES WITH HIGH SPEAKER KNOWLEDGE?!



# test speaker knowledge rating effect. first rerun analysis on only the subset of the data where "all" was annotated as a viable alternative to "some"
subd = d %>%
  filter(AllAlternative == "all_alt") %>% #5830 data points left (of 13630), ie 43% of the data
  droplevels()

pdf(file="../graphs/correlations.pdf",width=10,height=10)
pairscor.fnc(subd[,c("StrengthSome","logSentenceLength","BinaryGF","InfoStatus","Modification","Partitive","redInfoStatus","Median_knowledge","Mean_knowledge","Var_knowledge")])
dev.off()

centered = cbind(subd, myCenter(subd[,c("StrengthSome","logSentenceLength","Pronoun","BinaryGF","InfoStatus","DAModification","Modification","Partitive","redInfoStatus","numDA","Median_knowledge","Mean_knowledge","Var_knowledge")]))

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

m.knowledge = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + cMean_knowledge+cVar_knowledge + clogSentenceLength + (1|workerid), data=centered)
summary(m.knowledge)

anova(m.fixed,m.knowledge)


# plot means with context against means without context
agr = d %>%
  group_by(Item,Sentence,Mean_nocontext,YMin_nocontext,YMax_nocontext) %>%
  summarise(Mean_context = mean(r_SNA), CILow=ci.low(r_SNA),CIHigh=ci.high(r_SNA)) %>%
  ungroup() %>%
  mutate(YMin_context=Mean_context-CILow,YMax_context=Mean_context+CIHigh)

cor(agr$Mean_context,agr$Mean_nocontext)

ggplot(agr, aes(x=Mean_context,y=Mean_nocontext)) +
  geom_point() +
  geom_abline(intercept=0,slope=1,linetype = "dashed",color="gray60") +
  # geom_smooth(method="lm") +
  ylim(1,7) +
  xlim(1,7) +
  xlab("Mean rating with context") +
  ylab("Mean rating without context") 
ggsave(file="../graphs/scatterplot-context-nocontext.pdf",width=4,height=3.5)

write_csv(agr, path="../data/means_c_noc.csv")

# plot overlaid histogram of context/no-context ratings

long = agr %>%
  select(Item,Mean_context,Mean_nocontext) %>%
  gather(Context,Mean,-Item) %>%
  mutate(Context=fct_recode(Context,"context"="Mean_context","no context"="Mean_nocontext"))

ggplot(long, aes(x=Mean,fill=Context)) +
  geom_histogram(position="identity",alpha=.5) +
  xlab("Mean by-item inference rating") +
  scale_fill_manual(values=cbPalette[c(2,5)]) +
  ylim(0,100) 
ggsave(file="../graphs/context_nocontext_ratings.pdf",width=5.3,height=3)

ggplot(long %>% filter(Context == "context"), aes(x=Mean,fill=Context)) +
  geom_histogram(position="identity",alpha=.5) +
  xlab("Mean by-item inference rating") +
  scale_fill_manual(values=cbPalette[c(2,5)]) +
  # scale_x_continuous(breaks=seq(1,7,by=1))+#limits=c(1,7)) +
  ylim(0,100)
ggsave(file="../graphs/context_ratings.pdf",width=5,height=3)

# plot pairwise
pairs = agr %>%
  select(-CILow,-CIHigh) %>%
  gather(Context,Value,-Item,-Sentence) %>%
  separate(Context,into=c("ValType","Context"),"_",remove=F) %>%
  spread(ValType,Value)

context = pairs %>%
  filter(Context == "context") %>%
  mutate(Item = fct_reorder(Item,Mean))

pairs = pairs %>%
  mutate(Item = fct_relevel(Item,levels(context$Item))) %>%
  arrange(Item) %>%
  mutate(Block = rep(seq(1,6),each=454))

dodge = position_dodge(.9)

p = ggplot(pairs, aes(x=Item,y=Mean,color=Context)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Block,nrow=6,scales="free") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave(p, file="../graphs/context-nocontext.pdf",width=25,height=20)



