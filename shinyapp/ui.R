library(shinythemes)
library(rsconnect)
library(shiny)


ui <- fluidPage(
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  theme = shinythemes::shinytheme("superhero"),
  titlePanel(tags$h2("Manyōgana Transliterator")),
  fluidRow(
    column(4, tags$h3("Input some Japanese....")),
    column(8, tags$h3("and here's your manyōgana!"))
  ),
  fluidRow(
    column(4, textAreaInput("modern", "If you get an error, try inputting your text in a different script.\n I've found the transliteration works best if there are as many words as possible written in kanji where appropriate.\n For example, if something like こんにちは doesn't work, try こん日は.", rows = 5)),
    column(8, textOutput("text"),verbatimTextOutput("text_code"))
  ),
  titlePanel("Arabic-to-Kansūji Converter"),
  fluidRow(
    column(4, tags$h3("Input a number....")),
    column(8, tags$h3("and here's your kansūji!"))
  ),
  fluidRow(
    column(4, textInput("number", "In Arabic numerals, no commas or decimals please!\n The conversion should work up to 17 digits (##,###,###,###,###,###)")),
    column(8, textOutput("suuji"),verbatimTextOutput("suuji_code"))))
