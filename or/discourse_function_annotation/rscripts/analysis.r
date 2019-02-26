set.seed(42)

theme_set(theme_bw(18))
require(tidyverse)
library(cluster)
library(stringr)

source("helpers.R")

d = read.table("../annotation_cp/or_cindy_initial.txt", sep="\t", quote="",header=T)
d = d[,1:10]

# add resolved cases
dd = d %>%
  mutate_all(as.character)
row.names(dd) = dd$tgrep.id
# row.names(dd) = dd$entire.sentence
files = dir(path = "../agreement_annotation/resolved/")
for (f in files) {
  # get the right column name
  feature  = gsub(".csv","",f)
  # read the resolution file
  tmp = read_csv(paste("../agreement_annotation/resolved/",f,sep="")) %>%
    select(tgrep.id,Resolved) %>%
    # select(entire.sentence,Resolved) %>%
    mutate_all(as.character)
  tmp[,c(feature)] = tmp$Resolved
  tmp = tmp %>%
    select(-Resolved)
  print(feature)
  print(unique(tmp[,feature]))
  print(unique(dd[,feature]))
  print(nrow(dd))
  dd[tmp$tgrep.id,feature] = tmp[,feature]
  # dd[tmp$entire.sentence,feature] = tmp[,feature]
  print(nrow(dd))
}
dd = dd %>%
  mutate_all(as.factor)
complete = dd[complete.cases(dd),]
dd = dd[complete.cases(dd),]
summary(dd)
nrow(dd)

d = dd

# add Leyla and Lexi's situation type annotation (habituality of smallest sentence around "or", genericity of main referent)
ll = read.csv("../annotation_ll/data/annotation_resolved.csv") %>%
  select(tgrep.id,MainReferent=Resolved.MainReferent,Habituality=Resolved.Habituality) %>%
  mutate(MainReferent = as.factor(str_trim(MainReferent)), Habituality = as.factor(str_trim(Habituality)))

d = d %>%
  left_join(ll,by=c("tgrep.id"))

# add SI means from experiment:
idmeans = read.csv("../../experiment/3_implicature_exp/results/data/sentence_si_means.csv")
idmeans$SIMean = idmeans$Mean
idmeans$PropGoodSentence = idmeans$proportion_goodsentence

d = d %>%
  left_join(idmeans[,c("tgrep.id","SIMean","PropGoodSentence")],by=c("tgrep.id"))
nrow(d)
d$SentenceLength = nchar(as.character(d$entire.sentence))

# add info from final database that Neele created
db = read.csv("../../database/final_database.csv",sep="\t")
head(db)
db = db %>%
  mutate(tgrep.id=Item_ID) %>%
  select(tgrep.id,eitherPresent,orNotPresent)

d = d %>%
  left_join(db,by=c("tgrep.id"))
nrow(d)
summary(d)

#######################################
# CLUSTERING ANALYSIS ON ALL FEATURES #
#######################################

gower_dist <- daisy(d[, c(-1,-2)],
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)

# output most similar pair:
gower_mat <- as.matrix(gower_dist)
d[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# output most dissimilar pair:
d[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:60){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


# save(pam_fit,file="pam_fit.RData")

toplot = data.frame(NumClusters=seq(1,40),SilhouetteWidth=sil_width[1:40])
ggplot(toplot,aes(x=NumClusters,y=SilhouetteWidth)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0,40,by=5)) +
  xlab("Number of clusters") +
  ylab("Silhouette width")
ggsave("../graphs/silhouette_summary.pdf",width=8,height=4)


##############################################################
# the plot looks like 8 clusters are appropriate. let's select 8 then:
pam_fit <- pam(gower_dist, diss = TRUE, k = 8)

# get prototypical sentences for each cluster
d[pam_fit$medoids, ]$entire.sentence

pam_summary <- d %>%
  dplyr::select(-tgrep.id,-entire.sentence) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

d1 = d %>%
  mutate(cluster = pam_fit$clustering, PrototypicalSentence8="no") 
d1[pam_fit$medoids, ]$PrototypicalSentence8 = "yes"

cases = d1 %>%
  group_by(cluster) %>%
  count()

d1 = d1 %>%
  left_join(cases,by=c("cluster")) #%>%
  # mutate(Cluster=paste(cluster,n,sep="--"))
d1$Cluster = paste(d1$cluster,d1$n,sep="--")

head(d1)
View(d1[d1$PrototypicalSentence8 == "yes",c("Cluster","entire.sentence")])

# joint distribution of habituality and main referent and cluster
prop.table(table(d1$MainReferent,d1$Habituality,d1$Cluster),mar=c(3))

agr = d1 %>%
  select(-cluster) %>%
  gather(Feature,Value,-Cluster,-tgrep.id,-entire.sentence,-n) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  mutate(Cluster = reorder(Cluster,-n))

toplot1 = agr %>% 
  filter(!Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
  group_by(Cluster,Feature,Value) %>%
  summarise (n = n()) %>%
  mutate(Proportion = n / sum(n)) %>%
  select(-n)
nrow(toplot)

toplot2 = agr %>% 
  filter(Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
  mutate(Proportion = as.numeric(as.character(Value))) %>%
  select(Cluster, Feature, Value, Proportion)

toplot= left_join(toplot1,toplot2)

means = toplot2 %>%
  group_by(Cluster,Feature) %>%
  summarize(Mean=mean(as.numeric(as.character(Value))),CILow=ci.low(as.numeric(as.character(Value))),CIHigh=ci.high(as.numeric(as.character(Value)))) %>%
  mutate(XMin=Mean-CILow,XMax=Mean+CIHigh)

ggplot(toplot, aes(fill=Cluster)) +
  geom_bar(data=toplot1,aes(x=Value,y=Proportion),position="dodge",stat="identity",color="black") +
  geom_density(data=toplot2,aes(x=as.numeric(as.character(Value)),y=..scaled..)) +
  geom_point(data=means, aes(x=Mean,y=.5),color="gray80") +
  geom_errorbarh(data=means, aes(xmin=XMin,xmax=XMax,y=.5,x=Mean),color="gray80",height=.25) +
  facet_grid(Cluster~Feature,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/cluster_summary_8.pdf",width=18,height=6)

ggplot(toplot %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence8","Habituality","MainReferent","eitherPresent","SIMean")), aes(fill=Cluster)) +
  geom_bar(data=toplot1 %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence8","Habituality","MainReferent","eitherPresent","SIMean")),aes(x=Value,y=Proportion),position="dodge",stat="identity",color="black") +
  geom_density(data=toplot2 %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence8","Habituality","MainReferent","eitherPresent","SIMean")),aes(x=as.numeric(as.character(Value)),y=..scaled..)) +
#  geom_point(data=means  %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence8","Habituality","MainReferent","eitherPresent","SIMean")) , aes(x=Mean,y=.5),color="gray80") +
#  geom_errorbarh(data=means  %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence8","Habituality","MainReferent","eitherPresent","SIMean")), aes(xmin=XMin,xmax=XMax,y=.5,x=Mean),color="gray80",height=.25) +
  facet_grid(Cluster~Feature,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),legend.position="none")
ggsave("../graphs/cluster_summary_8_reduced.pdf",width=13,height=10)

# plot SI means by cluster
si_means_cluster = d1 %>%
  select(Cluster,SIMean,n) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  mutate(Cluster = reorder(Cluster,-n)) %>%
  group_by(Cluster) %>%
  summarize(Mean=mean(SIMean),CILow=ci.low(SIMean),CIHigh=ci.high(SIMean)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

ggplot(si_means_cluster,aes(x=Cluster,y=Mean,fill=Cluster)) +
  geom_bar(stat="identity",color="black") +
  geom_jitter(data=d1,aes(y=SIMean),color="gray50",alpha=.2) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/simeans_8.pdf")

# write the dataset so it can be merged back into SI analysis
towrite = d1
write.csv(towrite, "../data/case-features-si.csv",row.names=F)

# ANALYSIS OF INDIVIDUAL CASES

# sample 5 random sentences from numerical approximators
sample(d1[d1$Cluster == "5--381" & d1$Numeral == "num",]$entire.sentence,5)
# cluster 5--381, nonum
d1[d1$Cluster == "5--381" & d1$Numeral == "nonum",]$entire.sentence

# sample cases for non-"or not" analytically exclusive cases
sample(d1[d1$Cluster == "2--147" & d1$orNotPresent == "no",]$entire.sentence,5)

# sample 5 random sentences from conjunctive cases
sample(d1[d1$Cluster == "3--120" & d1$DE_operator == "not available",]$entire.sentence,5)

# sample 5 random sentences from correction cases
sample(d1[d1$Cluster == "4--98" & d1$Actually_test == "nochange" & d1$Disjunct_individuation == "same",]$entire.sentence,5)

# sample 5 random sentences from pragmatically exclusive cases
sample(d1[d1$Cluster == "7--90",]$entire.sentence,5)

# sample 5 random sentences from pragmatically exclusive cases
sample(d1[d1$Cluster == "6--169" & d1$Or_both_test == "change" & d1$SIMean > .6,]$entire.sentence,5)

# sample 5 random sentences from inclusive cases
sample(d1[d1$Cluster == "8--126" & d1$Or_both_test == "nochange",]$entire.sentence,5)

# sample 5 random sentences from gunk cases
sample(d1[d1$Cluster == "1--113",]$entire.sentence,5)

#   ###############################################
# # now let's select the global max (19) optimal clusters
# pam_fit <- pam(gower_dist, diss = TRUE, k = 19)
# 
# # get prototypical sentences for each cluster
# d[pam_fit$medoids, ]$entire.sentence
# 
# pam_summary <- d %>%
#   dplyr::select(-tgrep.id,-entire.sentence) %>%
#   mutate(cluster = pam_fit$clustering) %>%
#   group_by(cluster) %>%
#   do(the_summary = summary(.))
# 
# d2 = d %>%
#   mutate(cluster = pam_fit$clustering, PrototypicalSentence19="no") 
# d2[pam_fit$medoids, ]$PrototypicalSentence19 = "yes"
# 
# cases = d2 %>%
#   group_by(cluster) %>%
#   count()
# 
# d2 = d2 %>%
#   left_join(cases,by=c("cluster")) #%>%
# # mutate(Cluster=paste(cluster,n,sep="--"))
# d2$Cluster = paste(d2$cluster,d2$n,sep="--")
# 
# head(d2)
# 
# # get prototypical sentences per cluster, labeled
# d2 %>%
#   filter(d2$PrototypicalSentence == "yes") %>%
#   select(Cluster,entire.sentence)
# 
# agr = d2 %>%
#   select(-cluster) %>%
#   gather(Feature,Value,-Cluster,-tgrep.id,-entire.sentence,-n) %>%
#   mutate(Cluster = as.factor(Cluster)) %>%
#   mutate(Cluster = reorder(Cluster,-n))
# 
# toplot1 = agr %>% 
#   filter(!Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
#   group_by(Cluster,Feature,Value) %>%
#   summarise (n = n()) %>%
#   mutate(Proportion = n / sum(n)) %>%
#   select(-n)
# nrow(toplot)
# 
# toplot2 = agr %>% 
#   filter(Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
#   mutate(Proportion = as.numeric(as.character(Value))) %>%
#   select(Cluster, Feature, Value, Proportion)
# 
# toplot= left_join(toplot1,toplot2)
# 
# means = toplot2 %>%
#   group_by(Cluster,Feature) %>%
#   summarize(Mean=mean(as.numeric(as.character(Value))),CILow=ci.low(as.numeric(as.character(Value))),CIHigh=ci.high(as.numeric(as.character(Value)))) %>%
#   mutate(XMin=Mean-CILow,XMax=Mean+CIHigh)
# 
# ggplot(toplot, aes(fill=Cluster)) +
#   geom_bar(data=toplot1,aes(x=Value,y=Proportion),position="dodge",stat="identity",color="black") +
#   geom_density(data=toplot2,aes(x=as.numeric(as.character(Value)),y=..scaled..)) +
#   geom_point(data=means, aes(x=Mean,y=.5),color="gray80") +
#   geom_errorbarh(data=means, aes(xmin=XMin,xmax=XMax,y=.5,x=Mean),color="gray80",height=.25) +
#   facet_grid(Cluster~Feature,scales="free_x") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave("../graphs/cluster_summary_19.pdf",width=18,height=20)

# write the dataset with both local and global cluster optima so it can be merged back into SI analysis
# d1 = d1 %>%
#   rename(cluster5 = cluster, n5=n, Cluster5=Cluster)
# d2 = d2 %>%
#   rename(cluster19 = cluster, n19=n, Cluster19=Cluster)

# towrite = d1 %>%
  # left_join(d2)





#######################################################
# CLUSTERING ANALYSIS ON ALL FEATURES EXCEPT SI MEANS #
#######################################################

gower_dist <- daisy(d[, c(-1,-2,-13)],
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)

# output most similar pair:
gower_mat <- as.matrix(gower_dist)
d[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# output most dissimilar pair:
d[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:60){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


# save(pam_fit,file="pam_fit.RData")

toplot = data.frame(NumClusters=seq(1,60),SilhouetteWidth=sil_width)
ggplot(toplot,aes(x=NumClusters,y=SilhouetteWidth)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0,60,by=5))
ggsave("../graphs/silhouette_summary_no-si.pdf",width=8,height=4)

##############################################################
# the plot looks like 7 clusters are appropriate. let's select 7 then:
pam_fit <- pam(gower_dist, diss = TRUE, k = 7)

# get prototypical sentences for each cluster
d[pam_fit$medoids, ]$entire.sentence

pam_summary <- d %>%
  dplyr::select(-tgrep.id,-entire.sentence) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

d1 = d %>%
  mutate(cluster = pam_fit$clustering, PrototypicalSentence7="no") 
d1[pam_fit$medoids, ]$PrototypicalSentence7 = "yes"

cases = d1 %>%
  group_by(cluster) %>%
  count()

d1 = d1 %>%
  left_join(cases,by=c("cluster")) #%>%
# mutate(Cluster=paste(cluster,n,sep="--"))
d1$Cluster = paste(d1$cluster,d1$n,sep="--")

head(d1)
View(d1[d1$PrototypicalSentence7 == "yes",c("Cluster","entire.sentence")])

agr = d1 %>%
  select(-cluster) %>%
  gather(Feature,Value,-Cluster,-tgrep.id,-entire.sentence,-n) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  mutate(Cluster = reorder(Cluster,-n))

toplot1 = agr %>% 
  filter(!Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
  group_by(Cluster,Feature,Value) %>%
  summarise (n = n()) %>%
  mutate(Proportion = n / sum(n)) %>%
  select(-n)
nrow(toplot)

toplot2 = agr %>% 
  filter(Feature %in% c("PropGoodSentence","SIMean","SentenceLength")) %>%
  mutate(Proportion = as.numeric(as.character(Value))) %>%
  select(Cluster, Feature, Value, Proportion)

toplot= left_join(toplot1,toplot2)

means = toplot2 %>%
  group_by(Cluster,Feature) %>%
  summarize(Mean=mean(as.numeric(as.character(Value))),CILow=ci.low(as.numeric(as.character(Value))),CIHigh=ci.high(as.numeric(as.character(Value)))) %>%
  mutate(XMin=Mean-CILow,XMax=Mean+CIHigh)

ggplot(toplot, aes(fill=Cluster)) +
  geom_bar(data=toplot1,aes(x=Value,y=Proportion),position="dodge",stat="identity",color="black") +
  geom_density(data=toplot2,aes(x=as.numeric(as.character(Value)),y=..scaled..)) +
  geom_point(data=means, aes(x=Mean,y=.5),color="gray80") +
  geom_errorbarh(data=means, aes(xmin=XMin,xmax=XMax,y=.5,x=Mean),color="gray80",height=.25) +
  facet_grid(Cluster~Feature,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/cluster_summary_7_no-si.pdf",width=18,height=6)

ggplot(toplot %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence7")), aes(fill=Cluster)) +
  geom_bar(data=toplot1 %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence7")),aes(x=Value,y=Proportion),position="dodge",stat="identity",color="black") +
  geom_density(data=toplot2 %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence7")),aes(x=as.numeric(as.character(Value)),y=..scaled..)) +
  geom_point(data=means  %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence7")) , aes(x=Mean,y=.5),color="gray80") +
  geom_errorbarh(data=means  %>% filter(!Feature %in% c("PropGoodSentence","Sentence_type","SentenceLength","PrototypicalSentence7")), aes(xmin=XMin,xmax=XMax,y=.5,x=Mean),color="gray80",height=.25) +
  facet_grid(Cluster~Feature,scales="free_x") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),legend.position="top")
ggsave("../graphs/cluster_summary_7_reduced_no-si.pdf",width=14,height=7)

# plot SI means by cluster
si_means_cluster = d1 %>%
  select(Cluster,SIMean,n) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  mutate(Cluster = reorder(Cluster,-n)) %>%
  group_by(Cluster) %>%
  summarize(Mean=mean(SIMean),CILow=ci.low(SIMean),CIHigh=ci.high(SIMean)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

ggplot(si_means_cluster,aes(x=Cluster,y=Mean,fill=Cluster)) +
  geom_bar(stat="identity",color="black") +
  geom_jitter(data=d1,aes(y=SIMean),color="gray50",alpha=.2) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("../graphs/simeans_7_no-si.pdf")

towrite = d1
write.csv(towrite, "../data/case-features-no-si.csv",row.names=F)
