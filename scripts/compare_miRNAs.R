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




# 20240120
### I ran mirdeep2 on the ast-2000 sample that was trimmed to 25 bp. I'm going to compare the ast-2000 25 bp vs ast-2000 30 bp results 
ast_2000_30bp <- read.csv("output/Molecular/smRNA/mirdeep2/AST-2000_test_30bp.csv")
ast_2000_25bp <- read.csv("output/Molecular/smRNA/mirdeep2/AST-2000_test_25bp.csv")

blah <- full_join(ast_2000_25bp, ast_2000_30bp, by = "consensus.mature.sequence")

blah <- blah %>%
  select(provisional.id.x, 
         miRDeep2.score.x,
         estimated.probability.that.the.miRNA.candidate.is.a.true.positive.x,
         rfam.alert.x, 
         mature.read.count.x, 
         significant.randfold.p.value.x, 
         consensus.mature.sequence,
         provisional.id.y, 
         miRDeep2.score.y, 
         estimated.probability.that.the.miRNA.candidate.is.a.true.positive.y,
         rfam.alert.y, 
         mature.read.count.y, 
         significant.randfold.p.value.y) %>%
  filter(miRDeep2.score.x > 10) %>%
  filter(miRDeep2.score.y > 10) %>%
  filter(significant.randfold.p.value.x == "yes") %>%
  filter(significant.randfold.p.value.y == "yes") %>%
  na.omit()
  
length(unique(blah$consensus.mature.sequence)) # 111 unique seqs when comparing 25 v 30bp trimming for AST-2000

# Look to see how many unique seqs are in 25 v 30 bp
ast_2000_25bp <- ast_2000_25bp %>%
  filter(significant.randfold.p.value == "yes") %>%
  filter(miRDeep2.score > 10)
length(unique(ast_2000_25bp$consensus.mature.sequence)) # 129 unique seqs for 25bp trimming AST-2000

ast_2000_30bp <- ast_2000_30bp %>%
  filter(significant.randfold.p.value == "yes") %>%
  filter(miRDeep2.score > 10)
length(unique(ast_2000_30bp$consensus.mature.sequence)) # 115 unique seqs for 25bp trimming AST-2000




