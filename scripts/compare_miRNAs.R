library(tidyverse)

ast_1065 <- read.csv("Desktop/MolecularStats_Astrangia2021 - AST-1065-test.csv")
ast_2000 <- read.csv("Desktop/MolecularStats_Astrangia2021 - AST-2000 test.csv")

blah <- merge(ast_1065, ast_2000, by = "consensus.mature.sequence")
## different chromosome IDs for samples but same sequence in the end...why is this? Are they located on different places in the chromosome for different samples??? 

colnames(blah)

blah <- blah %>%
  select(consensus.mature.sequence, provisional.id.x, provisional.id.y, miRDeep2.score.x, miRDeep2.score.y, mature.read.count.x, mature.read.count.y) %>%
  filter(miRDeep2.score.x > 10) %>%
  filter(miRDeep2.score.y > 10)

length(unique(blah$consensus.mature.sequence)) # 21 unique seqs

# Look at ast samples filtered down
ast_1065 <- read.csv("Desktop/MolecularStats_Astrangia2021 - AST-1065-test.csv") %>%
  filter(miRDeep2.score > 10)
length(unique(ast_1065$provisional.id)) # 51 unique IDs
length(unique(ast_1065$consensus.mature.sequence)) # 44 unique sequences
# Why are there more IDs than there are sequences? 

ast_2000 <- read.csv("Desktop/MolecularStats_Astrangia2021 - AST-2000 test.csv") %>%
  filter(miRDeep2.score > 10)
length(unique(ast_2000$provisional.id)) # 138 unique IDs
length(unique(ast_2000$consensus.mature.sequence)) # 120 unique sequences

blah <- full_join(ast_1065, ast_2000, by = "consensus.mature.sequence") 
length(unique(blah$consensus.mature.sequence)) # 143 unique seqs when samples are merged 

# What about when NAs are omitted? 
blah <- full_join(ast_1065, ast_2000, by = "consensus.mature.sequence") %>%
  na.omit()
length(unique(blah$consensus.mature.sequence)) # 21 unique seqs when samples are merged and NAs are omitted 

