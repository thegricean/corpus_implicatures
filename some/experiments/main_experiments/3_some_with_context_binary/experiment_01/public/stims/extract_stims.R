library(tidyverse)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the full dataset of "some" sentences and contexts extracted from sw.backtrans_011410.t2c.gz
d = read_tsv("swbdext.tab") %>% 
  select(Item_ID,Context)
head(d)
nrow(d)
# view(d)

# load the dataset of cases analyzed in the 2015 S&P paper
full_database = read_tsv("some_database.csv") %>% 
  select(Item,Sentence,Partitive,StrengthSome,Subjecthood,Modification) %>% 
  unique() %>% 
  rename("Item_ID"="Item")
nrow(full_database)
head(full_database)

items_to_test = full_database %>% 
  left_join(d,by=c("Item_ID")) %>% 
  rename("OriginalSentence"="Sentence")
nrow(items_to_test)
head(items_to_test)

duplicates = items_to_test %>% 
  group_by(OriginalSentence) %>% 
  filter(n()>1) %>% 
  arrange(OriginalSentence,Item_ID) %>% 
  mutate(ComparisonSentence = OriginalSentence) 
view(duplicates)
nrow(duplicates)
head(duplicates)

# write for annotation (no need to do again)
write_tsv(duplicates, "duplicate_some.txt",quote="all")


# filter out cases with multiple instances of "some" (to be hand-annotated)
items_to_test = items_to_test %>% 
  filter(!Item_ID %in% duplicates$Item_ID) %>% 
  mutate(ComparisonSentence = OriginalSentence) %>% 
  mutate(ComparisonSentence = str_replace(ComparisonSentence, " some,", " some, but not all ")) %>% 
  mutate(ComparisonSentence = str_replace(ComparisonSentence, " some ", " some, but not all ")) %>% 
  mutate(ComparisonSentence = str_replace(ComparisonSentence, "some ", "some, but not all "))
nrow(items_to_test)
view(items_to_test %>% select(OriginalSentence,ComparisonSentence))

# code to test for any cases that didn't have "some" replaced by "some, but not all"
temp = items_to_test %>% 
  mutate(Detected = str_detect(ComparisonSentence,"some, but not all"))
temp %>% 
  filter(!Detected) %>% 
  select(OriginalSentence)

# read annotated duplicates
duplicates_annotated = read_tsv("duplicates_butnotall.tsv")
nrow(duplicates_annotated)
head(duplicates_annotated)
view(duplicates_annotated)

all_items = bind_rows(duplicates_annotated,items_to_test)
nrow(all_items)

write_tsv(all_items %>% select(Item_ID,OriginalSentence,ComparisonSentence,Context),"items.txt")
