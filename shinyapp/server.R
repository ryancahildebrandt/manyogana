library(shinythemes)
library(rsconnect)
library(shiny)

server <- function(input, output, session) {
  output$text <- renderText("ジャジャーン!!")
  output$text_code <- renderText(transliterate_it(paste0("\'",input$modern,"\'")))
  output$suuji <- renderText("ジャジャーン!!")
  output$suuji_code <- renderText(num_suuji(input$number))
}