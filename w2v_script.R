# Doc Setup----
library(word2vec)
library(LSAfun)
library(htm2txt)


library(magrittr)
library(tidyverse)

# freqs ----
w2v_freqs <- read.delim("~/github/manyogana/data/wikipedia-20150422-wordforms.tsv", header=FALSE, stringsAsFactors=FALSE)

# Word2Vec ----
#w2v_jp_full <- read.wordvectors("data/jawiki.word_vectors.100d.txt", type="txt")
#save(w2v_jp_full, file="w2v_jp_test.RData")
#load("./w2v_jp.RData")
#object.size(w2v_jp)

#w2v_jp <- w2v_jp %>% 
#  .[rownames(.) %in% w2v_freqs$V3] %>% 
#  as.matrix(.)
object.size(w2v_jp)

save(w2v_jp, file="w2v_jp_test.RData")

