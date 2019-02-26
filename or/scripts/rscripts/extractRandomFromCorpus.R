# Extract random sample of 200 cases from most inclusive "or" database for detailed annotation by Neele, Michael, and Judith
# Created by jdegen on May 23, 2017

setwd("/Users/titlis/Dropbox/switchboard/")

# Set seed to be able to recreate the random sample if we need to.
set.seed(42)

# Load dataset.
d = read.table("tdtlite/or/results/swbdext.tab", sep="\t", header=T,quote="")
head(d)

# 927 cases start with "or"; 419 cases are in disfluencies; 174 cases end in "or"
summary(d)
table(d[,c("HasPrecedingContent","IsInDisfluency","HasFollowingContent")])
d[d$HasPrecedingContent == "no",]$Sentence
d[d$HasFollowingContent == "no",]$Sentence

# good cases: 2778
nrow(d[d$HasPrecedingContent=="yes" & d$IsInDisfluency =="no" & d$HasFollowingContent == "yes",])

# sample 200 random cases and write to file for annotation
randsample = sample_n(d[d$HasPrecedingContent=="yes" & d$IsInDisfluency =="no" & d$HasFollowingContent == "yes",], 200)
summary(randsample)
randsample$Sentence

write.table(randsample,file="tdtlite/random200_to_annotate.tab",sep="\t",quote=F,col.names=T,row.names=F)

write.table(d[d$IsInDisfluency =="no",c("Item_ID","Sentence")],file="tdtlite/nodisfl.tab",sep="\t",quote=F,col.names=T,row.names=F)
