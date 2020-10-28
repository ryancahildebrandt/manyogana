# Doc Setup----
renv::activate()
library(rvest)
library(reshape2)
library(zipangu)
library(janitor)
library(udpipe)
library(tidytext)
library(word2vec)
library(LSAfun)
library(stringi)
library(htm2txt)

library(magrittr)
library(tidyverse)
renv::clean()
renv::snapshot()

#Functions----
get_script <- function(x) {
  x <- strsplit(x, split="") %>% unlist(.)
  script <- case_when(
    grepl("[ぁ-ん]",x) ~ "hiragana",
    grepl("[ァ-ン]",x) ~ "katakana",
    grepl("[0-9]",x) ~ "numeric",
    grepl("[一-龯]",x) ~ "kanji",
    grepl("[A-Za-z]",x) ~ "romaji",
    TRUE ~ x)
  return(script)
}

part_conj_myg <- function(x) {
  get_pc_myg <- function(x) {
    myg<-ifelse(get_script(x) %>% sapply(., unique)=="hiragana",
                wiki_hiragana$漢字[wiki_hiragana$かな == x],
                "_")
    return(myg)
  }
  l1<-unlist(strsplit(x, split=""))
  myg<-paste(sapply(l1, get_pc_myg), collapse="")
  return(myg)
}

get_myg_options<-function(x){
  myg<-wiki_manyogana$漢字[wiki_manyogana$かな == x]
  return(myg)
}

kana_word_myg <- function (xin) {
  xin_h<-str_conv_hirakana(xin, to="hiragana")
  ifelse(get_script(xin) %>% unique(.) =="katakana" | get_script(xin) %>% unique(.) =="hiragana",
         {l1<-unlist(strsplit(xin_h, split=""))
         mt<-mapply(get_myg_options, l1)
         
         get_cos_myg <- function(mt_i) {
           mlist<-sapply(X = mt_i, FUN = Cosine, y = xin, tvectors = w2v_jp) 
           maxcos<-names(mlist[mlist == max(mlist)])
           return(maxcos)
         }
         myg<-paste(sapply(FUN=get_cos_myg, mt), collapse="")
         },
         myg<-part_conj_myg(xin))
  return(myg)
}

mxd_myg<- function(xin) {
  xvec<-unlist(strsplit(xin, split=""))
  myg<-paste(case_when(
    get_script(xvec) == "kanji" ~ xvec,
    get_script(xvec) == "hiragana"~ sapply(xvec, part_conj_myg),
    TRUE ~ xvec), collapse="")
  return(myg)
}

num_suuji<-function (xin=999999999999999){
  options("scipen"=999)
  
  xinstr<-str_pad(as.character(xin) , width=17, side="left", pad="0")
  xinstr_vec<-unlist(strsplit(xinstr, split=""))
  xinstr_kanji<-sapply(xinstr_vec, function(i) suuji_df$suuji[i==suuji_df$num], USE.NAMES = FALSE)
  
  suuji_pat<-c("京","千","百","十","兆","千","百","十","億","千","百","十","万","千","百","十","一")
  suuji_1<-suuji_pat %>%
    rbind(xinstr_kanji %>% 
            paste(., collapse="") %>% 
            strsplit(.,split="") %>% 
            unlist(.),.) %>%
    c(.) %>%
    paste(., collapse="") %>% 
    stri_sub(., 0,-2) %>%
    gsub("([一二三四五六七八九]十)○([^○])","\\1\\2",., perl=TRUE) %>% 
    gsub("(○.)*", "", .) %>% 
    gsub("○", "", .) %>% 
    gsub("一(百|十)", "\\1", .)
  
  return(list(suuji_1))
}


#Wiki scrape----
wikiurl <- read_html("https://en.wikipedia.org/wiki/Man%27y%C5%8Dgana") %>%
  html_nodes("table.wikitable") %>%
  html_table(fill = TRUE)
wiki_example <- wikiurl[1] %>% data.frame(.)
wiki_shakuon <- wikiurl[2] %>% data.frame(.)
wiki_shakkun <- wikiurl[3] %>% data.frame(.)
wiki_katakana <- wikiurl[5] %>% data.frame(.)



#DF cleaning----
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
    かな=c("が","で","ば","ず","べ","ぎ","ぐ","げ","ご","ざ","じ","ぜ","ぞ","だ","ぢ","っ","づ","ど","ぱ","び","ぴ","ぶ","ぷ","ぺ","ぼ","ぽ","ゃ","ゅ","ょ"),
    漢字=c("賀","傅","婆","受","倍","芸","具","宜","其","射","士","是","曽","太","知","都","豆","杼","波","毘","毘","夫","布","閇","煩","富","夜","由","余") # pulled from manyoshu
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
  rename(., 漢字 = V1) %>% 
  merge(., wiki_hiragana, by="かな", all = TRUE, ) %>% 
  mutate(., 漢字 = sapply(.$漢字.x, simplify, USE.NAMES=FALSE)) %>% 
  mutate(., 漢字 = sapply(.$漢字, paste, collapse="")) %>% 
  mutate(., 漢字 = paste0(.$漢字, .$漢字.y) %>% gsub("NA","",.) %>% str_split(., pattern="")) %>% 
  transmute(., 
            かな = .$かな,
            漢字 = sapply(.$漢字, unique))

suuji_df<-data.frame(
  num = c("0","1","2","3","4","5","6","7","8","9","10","100","1000","10000","100000000","1000000000000","10000000000000000"),
  suuji = c("○","一","二","三","四","五","六","七","八","九","十","百","千","万","億","兆","京"), 
  stringsAsFactors = FALSE) 

#NLP setup---- 
load("./data/w2v_jp.RData")
udmodel_file <- udpipe_download_model(language= "japanese")
udmodel_jp <- udpipe_load_model(file = udmodel_file$file_model)
options("scipen"=999)

#Transliteration----
  
transliterate_it<-function(xin="使ってみて下さいね！"){
  options(warn=-1)
  ud_df <- udpipe_annotate(udmodel_jp, x = xin) %>% 
    as.data.frame(.) %>% 
    mutate(., script = sapply(.$token, get_script)) %>% 
    mutate(., kana = case_when(
      .$upos == "NOUN" & sapply(.$script, unique) == "hiragana" ~ .$token,
      TRUE ~ sapply(.$token, str_conv_hirakana, to="hiragana"))) %>% 
    mutate(., myg =  case_when(
      sapply(.$script, unique) == "kanji" ~ .$token,
      sapply(.$script, unique) == "numeric" ~ paste(sapply(.$token, num_suuji)),
      .$upos == "ADP" ~ paste(sapply(.$token, part_conj_myg)),
      .$upos == "VERB" & sapply(.$script, unique) == "hiragana" ~ paste(sapply(.$token, part_conj_myg)),
      (.$upos == "AUX" | .$upos == "SCONJ") & sapply(.$script, unique) == "hiragana" ~ paste(sapply(.$token, part_conj_myg)),
      sapply(sapply(.$script, unique), length) >= 2 ~ paste(sapply(.$kana, mxd_myg)),
      .$upos == "PUNCT" | .$upos == "SYM"  ~ .$token,
      TRUE ~ "MYG")) %>% 
    mutate(., myg = ifelse(.$myg=="MYG", sapply(.$token[.$myg=="MYG"],kana_word_myg), .$myg)) 
  xout <- paste(ud_df$myg, collapse="")
  return(list(xin,xout))
}
transliterate_it("こん日は!")

#works better if any possible words are in form found in dict, probably as much kanji as possible



#Full text ML----
#"https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS"

mys_scrape1<-read_html("http://jti.lib.virginia.edu/japanese/manyoshu/Man1Yos.html") %>% 
  html_nodes(.,"p") %>% 
  html_text(.,) 
  
mys_1<-data.frame(myg=mys_scrape1[grepl("^\\[原文\\]",mys_scrape1)],
                  kana=mys_scrape1[grepl("^\\[仮名\\]",mys_scrape1)], 
                  stringsAsFactors = FALSE) %>% 
  mutate(., 
         myg=gsub("^\\[原文\\]","",.$myg) %>% gsub("  ",",",.),
         kana=gsub("^\\[仮名\\],","",.$kana))

strsplit(mys_1$myg, split=",") %>%unlist()
. %>% strsplit(., split=",") %>% unlist(.)
    



# 
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.) %>%
# data.frame(V1=.,
# stringsAsFactors = FALSE) %>%
# filter(.,V1 != "") %>%
# mutate(., script= get_script(substr(.$V1,1,1)),
# id=c(1:length(.$V1)),
# len= nchar(.$V1))
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.) %>%
# data.frame(V1=.,
# stringsAsFactors = FALSE) %>%
# filter(.,V1 != "") %>%
# mutate(., script= get_script(substr(.$V1,1,1)),
# id=c(1:length(.$V1)),
# len= nchar(.$V1)) %>%
# filter(., len != 1)
# mys_df$V1[mys_df$script=="romaji"] %>% length()
# mys_df$V1[mys_df$script=="kanji"] %>% length()
# mys_df$V1[mys_df$script=="romaji"] %>% unique() %>% length()
# mys_df$V1[mys_df$script=="kanji"] %>% unique() %>% length()
# mys_df$V1[mys_df$script=="romaji"]%>% length()
# mys_df$V1[mys_df$script=="kanji"]%>% length()
# rmj<-mys_df$V1[mys_df$script=="romaji"]
# myg<-mys_df$V1[mys_df$script=="kanji"]
# rmj
# test<-data.frame(myg=c(myg, NA, NA),
# rmj=rmj,
# stringsAsFactors = FALSE)
# View(test)
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist)
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.)
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.) %>%
# data.frame(V1=.,
# stringsAsFactors = FALSE) %>%
# #filter(.,V1 != "") %>%
# mutate(., script= get_script(substr(.$V1,1,1)),
# id=c(1:length(.$V1)),
# len= nchar(.$V1)) %>%
# filter(., len != 1)
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.) %>%
# data.frame(V1=.,
# stringsAsFactors = FALSE) #%>%
# mys_df<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.)
# mys_str<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.)
# mys_str %>%
# mys_df<-
# mys_str
# mys_str
# mys_str<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*") %>%
# sapply(., unlist) %>%
# sapply(., str_split, pattern="\\n") %>%
# sapply(., unlist) %>%
# unlist(.) %>%
# gsub("[0-9]", "",.)
# mys_str %>% gsub("([一-龯]*)","",.)
# mys_str %>% gsub("[一-龯]*","",.)
# mys_str %>% gsub("\\n[一-龯]*","",.)
# mys_str<-mys_scrape %>%
# gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*", USE.NAMES = FALSE) %>%
# sapply(., unlist, USE.NAMES = FALSE) %>%
# sapply(., str_split, pattern="\\n", USE.NAMES = FALSE) %>%
# sapply(., unlist, USE.NAMES = FALSE) %>%
# unlist(.) %>%
# gsub("[0-9]", "",.)
# mys_str %>% gsub("\\n[一-龯]*","",.)
# mys_str %>% gsub("[一-龯]*","",.)
# mys_str %>% gsub("[一-龯]*","",.) %>% unique(.[.!=""])
# mys_str %>% gsub("[a-z]*","",.) %>% unique(.[.!=""])
# mys_str %>% gsub("[一-龯]*","",.) %>% unique(.[.!=""]) %>% length()
# mys_str %>% gsub("[a-z]*","",.) %>% unique(.[.!=""]) %>% length()
# mys_str %>% gsub("[一-龯]*","",.) %>% filter(., .!="")# %>% length()
# mys_str %>% gsub("[一-龯]*","",.) %>% .[.!=""]
# mys_str %>% gsub("[a-z]*","",.) %>% .[.!=""]
# mys_str %>% gsub("[一-龯]*","",.) %>% .[.!=""] %>% length()
# mys_str %>% gsub("[a-z]*","",.) %>% .[.!=""] %>% length()
# mys_df<-data.frame(mys=c(mys_str %>% gsub("[a-z]*","",.) %>% .[.!=""], NA, NA, NA, NA, NA, NA),
# rmj=mys_str %>% gsub("[一-龯]*","",.) %>% .[.!=""],
# stringsAsFactors = FALSE)
# View(mys_df)
# mys_str<-mys_scrape %>%
# #gsub("\\(.*\\)","",.) %>%
# gsub("[ -]*","",.) %>%
# gsub(".*?([一-龯\\n]*)([a-z\\n]*)","\\1\\2",.) %>%
# gsub("\\n\\n","\n",.) %>%
# gsub(".*Kojikikayō", "",.) %>%
# str_split(., pattern ="[A-Z]*\\.[0-9]*\\.[0-9]*[a-z]") %>%
# sapply(., str_split, pattern="[A-Z]*\\.[0-9]*", USE.NAMES = FALSE) %>%
# sapply(., unlist, USE.NAMES = FALSE) %>%
# sapply(., str_split, pattern="\\n", USE.NAMES = FALSE) %>%
# sapply(., unlist, USE.NAMES = FALSE) %>%
# unlist(.) %>%
# gsub("[0-9]", "",.)
# mys_df<-data.frame(mys=c(mys_str %>% gsub("[a-z]*","",.) %>% .[.!=""], NA, NA, NA, NA, NA, NA),
# rmj=mys_str %>% gsub("[一-龯]*","",.) %>% .[.!=""],
# stringsAsFactors = FALSE)
# mys_html<-read_html<-("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS")
# mys_html<-read_html<-("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS") %>%
# html_structure(.)
# mys_html<-read_html("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS") %>%
# html_structure(.)
# mys_html<-read_html("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS") %>%
# html_attrs(.)
# mys_html
# mys_html<-read_html("https://vsarpj.orinst.ox.ac.uk/corpus/ojcorpus.html#MYS") %>%
# html_nodes(.)
# mys_html
# 
# 
