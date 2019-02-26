theme_set(theme_bw(18))
require(tidyverse)
library(irr)

source("helpers.R")

dc = read.table("../annotation_cp/or_cindy_initial.txt", sep="\t", quote="",header=T)
dc = dc[,1:10]

dp = read.table("../annotation_cp/or_pratyusha_initial.txt", sep="\t", quote="",header=T)
dp = dp[,1:10]
dp = dp %>%
  mutate(Disjunct_relation = as.factor(as.character(gsub("~","-",as.character(Disjunct_relation)))),
         Disjunct_individuation = as.factor(as.character(gsub("diff$","different",as.character(Disjunct_individuation)))))

nrow(dp)
nrow(dc)

head(dc)
head(dp)

# check whether both annotators used the same categorization scheme for each feature:
names(dc)[3:length(names(dc))] %>%
  map(function(x) {
    unique(dc[,x])
  })

names(dc)[3:length(names(dc))] %>%
  map(function(x) {
    unique(dp[,x])
  })

results = names(dc)[3:length(names(dc))] %>%
            map_dfr(function(x) {
              return(data.frame(Feature=x,Kappa=kappa2(data.frame(rater1=dc[,x],rater2=dp[,x]),"unweighted")$value))
            })

results # looks like there's reasonably high agreement (> .9) only for Sentence_type and Numeral. Medium agreement (>.5) for DE_operator, And_test, and Disjunct_individuation. Low agreement (> .25) for Or_both_test and Disjunct_relation. And abysmal for Actually_test (.04)         

names(dc)[3:length(names(dc))] %>%
  map(function(x) {
    print(x)
    table(dc[,x],dp[,x])
  })

# get only cases where sentence type values differ
sentence_type = dc %>%
  select(tgrep.id,Sentence_type) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Sentence_type")],by=c("tgrep.id")) %>%
  filter(Sentence_type.x!=Sentence_type.y)
nrow(sentence_type)
sentence_type = sentence_type[,c(1,3,2,4)]
write.csv(sentence_type,file="../agreement_annotation/sentence_type.csv",row.names=F)

de_operator = dc %>%
  select(tgrep.id,DE_operator) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","DE_operator")],by=c("tgrep.id")) %>%
  filter(as.character(DE_operator.x) != as.character(DE_operator.y))
nrow(de_operator)
de_operator = de_operator[,c(1,3,2,4)]
write.csv(de_operator,file="../agreement_annotation/de_operator.csv",row.names=F)

numeral = dc %>%
  select(tgrep.id,Numeral) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Numeral")],by=c("tgrep.id")) %>%
  filter(as.character(Numeral.x) != as.character(Numeral.y))
nrow(numeral)
numeral = numeral[,c(1,3,2,4)]
write.csv(numeral,file="../agreement_annotation/numeral.csv",row.names=F)

and_test = dc %>%
  select(tgrep.id,And_test) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","And_test")],by=c("tgrep.id")) %>%
  filter(as.character(And_test.x) != as.character(And_test.y))
nrow(and_test)
and_test = and_test[,c(1,3,2,4)]
write.csv(and_test,file="../agreement_annotation/and_test.csv",row.names=F)

or_both_test = dc %>%
  select(tgrep.id,Or_both_test) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Or_both_test")],by=c("tgrep.id")) %>%
  filter(as.character(Or_both_test.x) != as.character(Or_both_test.y))
nrow(or_both_test)
or_both_test = or_both_test[,c(1,3,2,4)]
write.csv(or_both_test,file="../agreement_annotation/or_both_test.csv",row.names=F)

actually_test = dc %>%
  select(tgrep.id,Actually_test) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Actually_test")],by=c("tgrep.id")) %>%
  filter(as.character(Actually_test.x) != as.character(Actually_test.y))
nrow(actually_test)
actually_test = actually_test[,c(1,3,2,4)]
write.csv(actually_test,file="../agreement_annotation/actually_test.csv",row.names=F)

disjunct_relation = dc %>%
  select(tgrep.id,Disjunct_relation) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Disjunct_relation")],by=c("tgrep.id")) %>%
  filter(as.character(Disjunct_relation.x) != as.character(Disjunct_relation.y))
nrow(disjunct_relation)
disjunct_relation = disjunct_relation[,c(1,3,2,4)]
write.csv(disjunct_relation,file="../agreement_annotation/disjunct_relation.csv",row.names=F)

disjunct_individuation = dc %>%
  select(tgrep.id,Disjunct_individuation) %>%
  left_join(dp[,c("tgrep.id","entire.sentence","Disjunct_individuation")],by=c("tgrep.id")) %>%
  filter(as.character(Disjunct_individuation.x) != as.character(Disjunct_individuation.y))
nrow(disjunct_individuation)
disjunct_individuation = disjunct_individuation[,c(1,3,2,4)]
write.csv(disjunct_individuation,file="../agreement_annotation/disjunct_individuation.csv",row.names=F)





