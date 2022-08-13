
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

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
deployApp(appName="manyogana", 
    appTitle="Manyōgana Transliterator", 
    account = "rhildebrandt",
    appFiles = c("app.R","wikiurl.RData","w2v_jp_test.RData","japanese-gsd-ud-2.5-191206.udpipe", "ui.R", "server.R"))

#https://rhildebrandt.shinyapps.io/manyogana/





