library(shinythemes)
library(shiny)
library(rsconnect)
library(codetools)

source("shinyapp/readin.R")
source("shinyapp/ui.R")
source("shinyapp/server.R")

shinyApp(ui = ui, server = server)
deployApp("./shinyapp",appName="manyogana", appTitle="Manyōgana Transliterator")
#https://rhildebrandt.shinyapps.io/manyogana/




