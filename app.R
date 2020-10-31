library(shinythemes)
library(rsconnect)
library(shiny)
library(codetools)

source("shinyapp/readin.R")
source("shinyapp/ui.R")
source("shinyapp/server.R")

#shinyApp(ui = ui, server = server)
deployApp()

#runGitHub( "manyogana", "ryancahildebrandt")



