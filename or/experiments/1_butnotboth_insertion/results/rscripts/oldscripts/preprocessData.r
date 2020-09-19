library(tidyverse)
library(stringr)


#read in all data, path should point to the directory with all experiment folders
setwd("/home/neele/XPRAG/liveHITS")
setwd("/Users/judithdegen/Dropbox/switchboard/experiment/1_butnotboth_insertion/liveHITS")
source("./helpers.R")
directories = list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

d = read.csv("./pilot3/pilot.csv")
#all experminet result files should be named exp+number+csv
for (dir in directories) {
	files <- list.files(path = dir ,  full.names = TRUE, "^exp[0-9]+\\.csv$")
	
	if (!(identical(files, character(0)))){
	newdata = read.csv(file=files)
	d = rbind(d, newdata)}
}
#all results are read into one dataframe (d) with 11376 rows
nrow(d)

#summary(d)

# separate out the practice trials
practice = d %>%
  filter(slide_number_in_experiment < 5)

d = d %>%
  filter(slide_number_in_experiment > 4)
nrow(d)
names(d)
table(d$tgrep.id)

d[is.na(d$entire.sentence),]$slide_number_in_experiment
d[is.na(d$entire.sentence),]$tgrep.id

# filter out NA rows
d = d %>%
  filter(!is.na(entire.sentence))
nrow(d)

# clean response variable to strip it of junk and get it ready for analysis
d$response = gsub("[u'","",as.character(d$response),fixed=T)
d$response = gsub("]","",as.character(d$response),fixed=T)
d$response = gsub("&quotechar","'",as.character(d$response),fixed=T)
d$response = gsub("<font color=red>","",as.character(d$response),fixed=T)
d$response = gsub("</font>","",as.character(d$response),fixed=T)

d$entire.sentence = gsub("[u'","",as.character(d$entire.sentence),fixed=T)
d$entire.sentence = gsub("]","",as.character(d$entire.sentence),fixed=T)
d$entire.sentence = gsub("&quotechar","'",as.character(d$entire.sentence),fixed=T)
d$entire.sentence = gsub("<font color=red>","",as.character(d$entire.sentence),fixed=T)
d$entire.sentence = gsub("</font>","",as.character(d$entire.sentence),fixed=T)
d$entire.sentence = gsub(" (--n)+\\d+[a-z]*\\d*","",as.character(d$entire.sentence))
d$BNBSentence = sapply(strsplit(as.character(d$response),"', "), "[", 1)
d$NoGoodLocation = sapply(strsplit(as.character(d$response),"', "), "[", 2)
d$BNBInsertion = as.factor(ifelse(d$NoGoodLocation == "True","bad",ifelse(d$NoGoodLocation=="False","good","NA")))

table(d$tgrep.id,d$BNBInsertion)

# get number of different BNBSentences per ID
forms = d %>%
  group_by(tgrep.id) %>%
  summarize(TotalCases=length(BNBSentence),Num_different_BNBForms = length(unique(BNBSentence)))
nrow(d$BNBSentence)
d = d %>% 
  left_join(forms,by=c("tgrep.id"))
d$BNBFirstWord = sapply(strsplit(as.character(d$BNBSentence)," "), "[", 1)
d$FirstWord = sapply(strsplit(as.character(d$entire.sentence)," "), "[", 1)

unique(d[d$tgrep.id == "116776:42",]$BNBSentence)


#get the proportion of good locations per BNBSentence

table(d$BNBSentence,d$BNBInsertion)
props = as.data.frame(prop.table(table(d$BNBSentence,d$BNBInsertion),mar=c(1))) %>%
  filter(Var2 == "good" & !is.na(Freq)) 
colnames(props) = c("BNBSentence","Good","Proportion_goodLoc_perBNB")
props$Good = NULL
props
d = d %>% 
  left_join(props,by=c("BNBSentence"))

#get the general frequencies for each BNBSentence
d  %<>% group_by(BNBSentence) %>% mutate(frequency_BNBSentence = n())


#get absolute the frequency of 'good locations' per BNBSentence
d  %<>% group_by(BNBSentence, BNBInsertion) %>% mutate(frequency_BNBInsertion = n())

#number of cases where people used but not both directly after or
da = which(d$Num_different_BNBForms>1)
bnb_respo_butnotboth_afteror = c()
for (i in da){
  bnb = d[i, ]$BNBSentence
  result = str_match(bnb," or,? (?! but not both|, but not both).* but not both")
  if (is.na(result)){
    bnb_respo_butnotboth_afteror = c(i, bnb_respo_butnotboth_afteror)
  }
}
print(length(bnb_respo_butnotboth_afteror))
d = d[-bnb_respo_butnotboth_afteror,]

#create three subtables for each category to be extracted. 
sub1 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))
sub2 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))
sub3 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))


#subtable for getting only those BNBSentences with the highes frequency per ID (modal response)
sub1 %<>%
  group_by(tgrep.id) %>%
  slice(which(frequency_BNBSentence == max(frequency_BNBSentence)))

#subtable for getting only those BNBSentences with the highest proportion of good location per ID (relative frequency of good loc. per BNBSentence)
sub2 %<>%
  group_by(tgrep.id) %>%
  slice(which(Proportion_goodLoc_perBNB == max(Proportion_goodLoc_perBNB)))

#subtable for getting only those BNBSentences with the highest absolute frequency of good location sentences per ID (absolute frequency of good loc. per BNBSentence)
sub3 %<>%
  group_by(tgrep.id) %>%
  slice(which(frequency_BNBInsertion == max(frequency_BNBInsertion)))



#filter out all duplicated BNBSentences per ID
sub1 = sub1[!duplicated(sub1[,"BNBSentence",]),]
sub2 = sub2[!duplicated(sub2[,"BNBSentence",]),]
sub3 = sub3[!duplicated(sub3[,"BNBSentence",]),]


alld = d[!duplicated(d[,"BNBSentence"]),]
print(nrow(alld))
d = d[!duplicated(d[,"tgrep.id",]),]
d$SentenceOverlap = "" 
d$ResponseModal = ""
d$ResponseFreqGoodLocation = ""
d$ResponsePropGoodLocation = ""
d$AllBnBs = ""
for (index in 1:nrow(d)){
  id = d[index,]$tgrep.id
  print(index)
	target_modal = droplevels(sub1[sub1$tgrep.id == id,])
	target_freq = droplevels(sub3[sub3$tgrep.id == id,])
	target_prop = droplevels(sub2[sub2$tgrep.id == id,])
	target = droplevels(alld[alld$tgrep.id == id,])
	if(target_modal$BNBSentence == target_freq$BNBSentence && target_modal$BNBSentence == target_prop$BNBSentence)
	{
	  d[index,]$SentenceOverlap = TRUE
	}
	if (target_modal$BNBSentence != target_freq$BNBSentence || target_modal$BNBSentence != target_prop$BNBSentence)
		{ 
		d[index,]$SentenceOverlap = FALSE
	}
	if (nrow(target_modal)>1) {
		 modsen = paste(target_modal$BNBSentence, collapse = "  |  ")
			}
	if (nrow(target_freq)>1) {
		 freqsen = paste(target_freq$BNBSentence, collapse = "  |  ")
			}
	if (nrow(target_prop)>1) {
		 propsen = paste(target_prop$BNBSentence, collapse = "  |  ")
			}
	if (nrow(target_modal)==1) {
		 modsen = target_modal$BNBSentence
			}
	if (nrow(target_freq)==1) {
		 freqsen = target_freq$BNBSentence
			}
	if (nrow(target_prop)==1) {
		 propsen = target_prop$BNBSentence
	}	
	if (nrow(target)>1){
	  bnbs = paste(target$BNBSentence, collapse = "  |  ")
	}
	if (nrow(target)==1){
	  bnbs = target$BNBSentence
	}
	d[index, ]$ResponseModal = modsen
	d[index, ]$ResponseFreqGoodLocation = freqsen
	d[index, ]$ResponsePropGoodLocation = propsen
	d[index, ]$AllBnBs = bnbs
}
#new categories are added to the final dataframe

final = subset(d, select=c("tgrep.id", "entire.sentence", "Num_different_BNBForms", "AllBnBs", "Proportion_goodLoc_perBNB", "ResponseModal", "ResponseFreqGoodLocation", "ResponsePropGoodLocation", "SentenceOverlap"))

length(grep("  |  ",as.character(final$ResponseModal),fixed=T))
final[grep("  |  ",as.character(final$ResponseModal),fixed=T),]$ResponseModal
final$MoreThanOneModalResponse = FALSE
final[grep("  |  ",as.character(final$ResponseModal),fixed=T),]$MoreThanOneModalResponse = TRUE

length(grep("  |  ",as.character(final$ResponsePropGoodLocation),fixed=T))
final[grep("  |  ",as.character(final$ResponsePropGoodLocation),fixed=T),]$ResponsePropGoodLocation
final$MoreThanOnePropResponse = FALSE
final[grep("  |  ",as.character(final$ResponsePropGoodLocation),fixed=T),]$MoreThanOnePropResponse = TRUE

length(grep("  |  ",as.character(final$ResponseFreqGoodLocation),fixed=T))
final[grep("  |  ",as.character(final$ResponseFreqGoodLocation),fixed=T),]$ResponseFreqGoodLocation
final$MoreThanOneFreqResponse = FALSE
final[grep("  |  ",as.character(final$ResponseFreqGoodLocation),fixed=T),]$MoreThanOneFreqResponse = TRUE

length(grep("  |  ",as.character(final$AllBnBs),fixed=T))
final[grep("  |  ",as.character(final$AllBnBs),fixed=T),]$AllBnBs
final$MoreThanOneResponse = FALSE
final[grep("  |  ",as.character(final$AllBnBs),fixed=T),]$MoreThanOneResponse = TRUE


f <- function(modalresponse) {
  finalbnb = ""
  bnbs = strsplit(modalresponse, split=' | ', fixed=TRUE)
  len = 500
  for (bnb in bnbs[[1]]){
    print("bnb")
    print(bnb)
    result = str_match(bnb, "or,? (?! but not both|, but not both).* but not both")
    print("result")
    print(result)
    thislen = nchar(as.character(result))
    print(thislen)
    if (thislen < len){
      len = thislen
      finalbnb = bnb
    }
  }
  print("final sentence: ")
  print(finalbnb)
  return(finalbnb)
}

names(d)
modal = which(final$MoreThanOneModalResponse)
freq = which(final$MoreThanOneFreqResponse)
prop = which(final$MoreThanOnePropResponse)
all = which(final$MoreThanOneResponse)
final$ResponseBNBClosestToOrModal = NA
final$ResponseBNBClosestToOrFreq = NA
final$ResponseBNBClosestToOrProp = NA
final$BNBclosestToOr = NA

for (i in all){
  bnb = f(as.character(final$AllBnBs[i]))
  final$BNBclosestToOr[i] = as.character(bnb)
}

for (i in modal){
  bnb = f(as.character(final$ResponseModal[i]))
  final$ResponseBNBClosestToOrModal[i] = as.character(bnb)
}

for (i in freq){
  bnb = f(as.character(final$ResponseFreqGoodLocation[i]))
  final$ResponseBNBClosestToOrFreq[i] = as.character(bnb)
}

for (i in prop){
  bnb = f(as.character(final$ResponsePropGoodLocation[i]))
  final$ResponseBNBClosestToOrProp[i] = as.character(bnb)
}
names(final)

print("length of final data frame")
nrow(final)

write.table(final, file="./newfinal.csv",sep="\t",quote=F,col.names=T,row.names=F)
print('done')



