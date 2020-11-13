library(shiny)
library(shinythemes)
library(rsconnect)





server <- function(input, output, session) {
  # Doc Setup----
  
  {
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
    library(shiny)
    library(shinythemes)
    library(rsconnect)
    
    library(magrittr)
    library(tidyverse)
  }
  
  
  #Functions----
  get_script <- function(x) {
    x <- strsplit(x, split="") %>% unlist(.)
    script <- case_when(
      grepl("[ぁ-ん]",x) ~ "hiragana",
      grepl("[ァ-ン]",x) ~ "katakana",
      grepl("[0-9]",x) ~ "numeric",
      grepl("[一-龯]",x) ~ "kanji",
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
    
    xinstr<-str_pad(as.character(xin) , width=17, side="left", pad="0") %>% str_conv_zenhan(., to="hankaku")
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
    
    return(suuji_1)
  }
  base::load("wikiurl.RData")
  
  
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
  base::load("w2v_jp_test.RData")
  udmodel_jp <- udpipe_load_model(file = "japanese-gsd-ud-2.5-191206.udpipe")
  options("scipen"=999)
  
  #Transliteration----
  transliterate_it<-function(xin="使ってみて下さいね！"){
    options(warn=-1)
    xin<-as.character(xin)
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
      mutate(., myg = ifelse(.$myg == "MYG", sapply(.$token, part_conj_myg), .$myg))
    xout <- paste(ud_df$myg, collapse = "") %>% gsub("[']","",.)
    return(xout)
  }
  
  #Server-----
  modern <- reactive({
    transliterate_it(paste0("\'",input$modern_user,"\'"))
  })
  number <- reactive({
    num_suuji(input$number_user)
  })
  
  
  output$text <- renderText({input$modern_user})
  output$text_code <- renderText({modern()})
  output$suuji <- renderText({input$number_user})
  output$suuji_code <- renderText({number()})
  
  
  
}