library(Hmisc)
library(gridExtra)
library(MuMIn)

load("../data/complete_md.RData")
source("helpers.R")

# To run the regression reported in Degen 2015:
centered = cbind(md, myCenter(md[,c("StrengthSome","logSentenceLength","Pronoun","BinaryGF","InfoStatus","DAModification","Modification","Partitive","redInfoStatus","numDA")]))

m.random = lmer(Rating ~  (1|workerid), data=centered)
summary(m.random)

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

anova(m.random,m.fixed)


m = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credInfoStatus|workerid) + (0 + cBinaryGF|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|Item), data=centered)
summary(m)
msummary = summary(m)
coefs = as.data.frame(msummary$coefficients)
summary(coefs)
head(coefs)

createLatexTableLinear(coefs,predictornames=c("Intercept","Partitive","Strength","Linguistic mention","Topicality","Modification","Sentence length","Partitive:Strength","Linguistic mention:Topicality","Linguistic mention:Modification","Topicality:Modification","Linguistic mention:Topicality:Modification"))


# Add Yuxing's vector representations of each item
vheaders = paste("V",seq(1,100),sep="")
embs = read_tsv("../data/embs_single.csv") %>%
  separate(Vector_Representation,vheaders,sep=",") %>%
  rename(Item = Item_ID)

tmp = embs %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(VectorSum = rowSums(.[2:101]))
tmp$Item = embs$Item
head(embs)

d = md %>%
  left_join(tmp) 
head(d,20)

write.csv(d, "../data/some_data_withembs.tsv",row.names=F)

# Add Yuxing's predictions
preds = read_tsv("../data/predictions.csv") %>%
  rename(Item = Item_ID)

d = d %>%
  left_join(preds)
  
# run the models to compare vectorsum vs hand mined features
centered = cbind(d, myCenter(d[,c("StrengthSome","logSentenceLength","Pronoun","BinaryGF","InfoStatus","DAModification","Modification","Partitive","redInfoStatus","numDA")]))

m.vectorpred = lmer(Rating ~ predicted + (1|workerid), data=centered)
summary(m.vectorpred) 

m.vector = lmer(formula(paste("Rating ~ ",paste("V",seq(1,100),sep="",collapse="+"),"+ (1|workerid)")), data=centered)
summary(m.vector) 

m.fixedandvector = lmer(formula(paste("Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength +",paste("V",seq(1,100),sep="",collapse="+"),"+ (1|workerid)")), data=centered)
summary(m.fixedandvector) 

m.fixedandvectorpred = lmer(Rating ~ predicted + cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixedandvectorpred)

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credInfoStatus*cBinaryGF*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

anova(m.fixed,m.fixedandvectorpred) # chi squared (1) = 3086.7, p < .0001
anova(m.vectorpred,m.fixedandvectorpred) # chi squared (11) = 74.6, p < .0001

centered$PredictedVector = fitted(m.vector)
centered$PredictedVectorPred = fitted(m.vectorpred)
centered$PredictedFixed = fitted(m.fixed)
centered$PredictedFV = fitted(m.fixedandvector)
centered$PredictedFVP = fitted(m.fixedandvectorpred)

agr = centered %>%
  group_by(Item,Sentence) %>%
  summarise(MeanEmpirical = mean(Rating), MeanPredictedVector = mean(PredictedVector),MeanPredictedVectorPred = mean(PredictedVectorPred),MeanPredictedFixed = mean(PredictedFixed), MeanPredictedFV = mean(PredictedFV),MeanPredictedFVP = mean(PredictedFVP)) 

write.csv(agr,"../data/some_means_emppred.csv",row.names=F)
 
cor(agr$MeanPredictedVector, agr$MeanEmpirical) # corr: .62
cor(agr$MeanPredictedFixed, agr$MeanEmpirical) # corr: .66
cor(agr$MeanPredictedFV, agr$MeanEmpirical) # corr: .73
cor(agr$MeanPredictedVectorPred, agr$MeanEmpirical) # corr: .90
cor(agr$MeanPredictedFVP, agr$MeanEmpirical) # corr: .91

r.squaredGLMM(m.vector)
r.squaredGLMM(m.vectorpred)
r.squaredGLMM(m.fixed)
r.squaredGLMM(m.fixedandvector)
r.squaredGLMM(m.fixedandvectorpred)


ggplot(agr, aes(x=MeanPredictedFixed,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(0,7) +
  ylim(0,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fixed.pdf",width=5,height=4)

ggplot(agr, aes(x=MeanPredictedVector,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(0,7) +
  ylim(0,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_vector.pdf",width=5,height=4)

ggplot(agr, aes(x=MeanPredictedFV,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(0,7) +
  ylim(0,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fv.pdf",width=5,height=4)

ggplot(agr, aes(x=MeanPredictedVectorPred,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(0,7) +
  ylim(0,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_vectorpred.pdf",width=5,height=4)

ggplot(agr, aes(x=MeanPredictedFVP,y=MeanEmpirical)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlim(0,7) +
  ylim(0,7) +
  ylab("Empirical rating") +
  xlab("Predicted rating")
ggsave("../graphs/model_fit_fvp.pdf",width=5,height=4)
