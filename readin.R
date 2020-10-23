# Doc Setup----
library(rvest)
library(reshape2)
library(zipangu)
library(janitor)
library(udpipe)
library(tidytext)
library(word2vec)
library(LSAfun)


library(magrittr)
library(tidyverse)
renv::snapshot()
#Notes----

# input japanese text 
# parse into tokens
# output manyogana transcription


# In the poem, the sounds mo (母, 毛) and shi (之, 思) are written with multiple, different characters. While all particles and most words are represented phonetically (e.g., 多太 tada, 安佐 asa), the words ji (路), umi (海) and funekaji (船梶) are rendered semantically.
# In some cases, specific syllables in particular words are consistently represented by specific characters. This usage is known as Jōdai Tokushu Kanazukai. This usage has led historical linguists to conclude that certain disparate sounds in Old Japanese, consistently represented by differing sets of man'yōgana characters, may have merged since then.
# Shakuon kana (借音仮名) are based on a Sino-Japanese on'yomi reading, in which one character represents either one mora or two morae.[6]
# Shakkun kana (借訓仮名) are based on a native kun'yomi reading, one to three characters represent one to three morae.[6]
# The vocalic distinction (1 versus 2) is very important linguistically, but for literary purposes (or while reading for pleasure) it can be ignored – as it often is in Japanese works, hence no differentiation is usually made in the hiragana transcription. 

#full text
#http://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS
#https://www.aclweb.org/anthology/W16-4006/




#Transliteration functions----
get_script <- function(x) {
  x <- strsplit(x, split="") %>% unlist(.)
  script <- case_when(
    grepl("[ぁ-ん]",x) ~ "hiragana",
    grepl("[ァ-ン]",x) ~ "katakana",
    grepl("[０-９]",x) ~ "numeric",
    grepl("[一-龯]",x) ~ "kanji",
    grepl("[A-Za-z]",x) ~ "romaji",
    TRUE ~ x)
  return(script)
}

#kanji_myg
# kanji words: leave meaning as much as possible
#  func unnecessary


part_conj_myg <- function(x) {
  myg <- wiki_hiragana$漢字[wiki_hiragana$かな==x]
  return (myg)
}
part_conj_myg("ず")
#particles & conjugations: convert using 1:1 manyogana

kana_myg <- function(x) {
  myg <- 
  return (myg)
}
part_conj_myg("ず")
#kana_myg
#romaji_myg
#other

# kana words: convert straight away, with meaning proximity consideration



#Wiki scrape----
wikiurl <- read_html("https://en.wikipedia.org/wiki/Man%27y%C5%8Dgana") %>%
  html_nodes("table.wikitable") %>%
  html_table(fill = TRUE)
wiki_example <- wikiurl[1] %>% data.frame(.)
wiki_shakuon <- wikiurl[2] %>% data.frame(.)
wiki_shakkun <- wikiurl[3] %>% data.frame(.)
wiki_katakana <- wikiurl[5] %>% data.frame(.)

#Wiki cleaning----
wiki_hiragana <- wikiurl[6] %>% 
  data.frame(.) %>%
  melt(., id = "Var.1") %>% 
  mutate(., variable = ifelse(grepl("\\.", as.character(.$variable)), substr(as.character(.$variable),0,1), as.character(.$variable)),
         Var.1 = gsub("–", "",.$Var.1)) %>% 
  mutate(., mora = case_when(
    paste0(gsub("X","", .$variable),.$Var.1) == "Si" ~ "Shi",
    paste0(gsub("X","", .$variable),.$Var.1) == "Ti" ~ "Chi",
    paste0(gsub("X","", .$variable),.$Var.1) == "Tu" ~ "Tsu",
    paste0(gsub("X","", .$variable),.$Var.1) == "Zi" ~ "Ji",
    paste0(gsub("X","", .$variable),.$Var.1) == "Di" ~ "Ji",
    paste0(gsub("X","", .$variable),.$Var.1) == "Du" ~ "Dzu",
    paste0(gsub("X","", .$variable),.$Var.1) == "Hu" ~ "Fu",
    TRUE ~ paste0(gsub("X","", .$variable),.$Var.1))) %>% 
  mutate(., "かな" = str_conv_romanhira(.$mora, "hiragana")) %>% 
  filter(., .$value!="") %>% 
  filter(., get_script(.$value)=="kanji") %>% 
  rename(., "漢字" = value) %>% 
  select(., "かな", "漢字") %>% 
  rbind(., data.frame(
    かな=c("が","で","ば","ず"),
    漢字=c("賀","傅","婆","受")
  )) %>% 
  distinct(.)
  

wiki_manyogana <- wikiurl[4] %>% 
  data.frame(.) %>%
  mutate(., X1 = ifelse(.$X1 %in% "", "_", .$X1)) %>% 
  row_to_names(., row_number = 1) %>%
  melt(., id = "_") %>% 
  mutate(., 
         "mora" = gsub("[12–]","", paste0(.$variable, .$"_")),
         "漢字" = str_split(.$value,"")) %>%
  mutate(., "mora" = case_when(
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Si" ~ "Shi",
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Ti" ~ "Chi",
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Tu" ~ "Tsu",
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Zi" ~ "Ji",
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Di" ~ "Ji",
    gsub("[12–]","", paste0(.$variable, .$"_")) == "Du" ~ "Dzu",
    TRUE ~ .$mora)) %>% 
  mutate(., "かな" = str_conv_romanhira(.$mora, "hiragana")) %>%
  select(., "かな", "漢字") %>% 
  pivot_wider(.,
              names_from = かな, 
              values_from = 漢字,
              values_fn=list) %>% 
  t(.) %>% 
  as.data.frame(.) %>%
  rownames_to_column(., var = "かな") %>% 
  mutate(., 漢字 = sapply(sapply(.$V1, simplify, USE.NAMES = FALSE), unique), 
         V1 = NULL)



#NLP---- 
exin <- c("私は韓国へ行くつもりです.")
udmodel_file <- udpipe_download_model(language= "japanese")
udmodel_jp <- udpipe_load_model(file = udmodel_file$file_model)


ud_df<-udpipe_annotate(udmodel_jp, x=exin) %>% 
  as.data.frame(.) %>% 
  mutate(., "script" = 1)

trans_df <- data.frame(
  input = strsplit(exin, split=""),
  script = get_script(exin),
  stringsAsFactors = FALSE)

colnames(trans_df) <- c("input","script")

#    grepl("[一-龯]&&[ぁ-ん]",x) ~ "mixedhk",
exin
exout <- paste(trans_df$input, collapse="")
exout

w2v_test <- read.wordvectors("./data/jawiki_20180420_300d.txt", type="txt")
save(w2v_test, file="w2v.RData")
Cosine("ある","月",w2v_test)

#JMDict----

# 
# dict_in<-read.delim2("./data/JMDict_e.txt", sep = "\n") %>%
#   transmute(., text=as.character(.$X..xml.version.1.0.encoding.UTF.8..))  
#   
# JMDict<- paste0(dict_in$text, collapse=";") %>%
#   strsplit(., split="</entry>") %>% 
#   data.frame(., stringsAsFactors = FALSE) %>% 
#   set_colnames(., c("text")) %>% 
#   filter(.,grepl("^;<entry>", .$text)) %>% 
#   transmute(., keb=gsub("<(.*?)>|;","",str_extract(.$text,"<keb>(.*?);")),
#               ke_pri=gsub("<(.*?)>|;","",str_extract(.$text,"<ke_pri>nf(.*?);")),
#          reb=gsub("<(.*?)>|;","",str_extract(.$text,"<reb>(.*?);")),
#          re_pri=gsub("<(.*?)>|;","",str_extract(.$text,"<re_pri>nf(.*?);")),
#          gloss=gsub("<(.*?)>|;","",str_extract_all(.$text,"(<gloss(.*?)</gloss>)"))) %>% 
#   rowwise(.) #%>% 
#   mutate(., gloss=paste(.$gloss, collapse="; "))


