# Doc Setup----
renv::restore()
library(word2vec)
library(LSAfun)
library(htm2txt)


library(magrittr)
library(tidyverse)
renv::snapshot()

#Word2Vec----
#w2v_jp <- read.wordvectors("./shinyapp/jawiki_20180420_100d.txt", type="txt")
#save(w2v_jp, file="shinyapp/w2v_jp.RData")



