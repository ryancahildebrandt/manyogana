# Doc Setup----
renv::restore()
library(word2vec)
library(LSAfun)
library(htm2txt)

library(magrittr)
library(tidyverse)
renv::snapshot()

#Word2Vec----
w2v_jp <- read.wordvectors("./data/jawiki_20180420_100d.txt", type="txt")
save(w2v_jp, file="data/w2v_jp.RData")

#MYS Scrape----
mys_scrape<-gettxt("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS")
save(mys_scrape, file="data/mys_scrape.RData")



