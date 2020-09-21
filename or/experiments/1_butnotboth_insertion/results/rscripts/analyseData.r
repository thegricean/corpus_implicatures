require(tidyverse)
library(stringr)
library("ggplot2")
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)

#read in all data, path should point to the directory with all experiment folders
setwd("/home/neele/XPRAG/liveHITS")
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
#all results are read into one dataframe (d)
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


#create three subtables for each category to be extracted. 
sub1 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))
sub2 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))
sub3 = subset(d, select=c("tgrep.id", "BNBSentence","BNBInsertion", "Num_different_BNBForms", "Proportion_goodLoc_perBNB","frequency_BNBSentence", "frequency_BNBInsertion"))

names(d)

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
write.table(sub1, file="./mostfrequent.csv",sep="\t",quote=F,col.names=T,row.names=F)

#each list will contain the sentences of each category. when two ore more sentences have the same number / percentage, they will be concatenated as one string and added to the list so that in the end each tgrep ID correlate with one line per category (which contain one or more sentences with maximum numbers)
modal <-c()
good_freq <-c()
good_prop <-c()
#sentence overlap will be true if the BNBSentence is the same for each category
overlap <-c()
for (id in unique(d$tgrep.id)){
	target_modal = droplevels(sub1[sub1$tgrep.id == id,])
	target_freq = droplevels(sub3[sub3$tgrep.id == id,])
	target_prop = droplevels(sub2[sub2$tgrep.id == id,])
	if(target_modal$BNBSentence == target_freq$BNBSentence && target_modal$BNBSentence == target_prop$BNBSentence)
		{overlap <-c(TRUE, overlap)}
	if (target_modal$BNBSentence != target_freq$BNBSentence || target_modal$BNBSentence != target_prop$BNBSentence)
		{ overlap <-c(FALSE, overlap)
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
	modal <- c(modsen, modal)
	good_freq <- c(freqsen, good_freq)
	good_prop <- c(propsen, good_prop)

}
#new categories are added to the final dataframe
d = d[!duplicated(d[,"tgrep.id",]),]
final = subset(d, select=c("tgrep.id", "Num_different_BNBForms", "Proportion_goodLoc_perBNB"))
final["modal_response"] <- modal
final["best_location_(frequency)"] <- good_freq

final["best_location_(proportion)"] <- good_prop
final["sentence_overlap"] <- overlap

print("length of final data frame")
nrow(final)

write.table(final, file="./final.csv",sep="\t",quote=F,col.names=T,row.names=F)
print('done')



