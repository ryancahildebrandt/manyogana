library(shiny)
library(shinythemes)
library(rsconnect)

ui <- fluidPage(
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  theme = shinythemes::shinytheme("superhero"),
  titlePanel("Manyōgana Transliterator"),
  tags$h5("By Ryan Hildebrandt :: github.com/ryancahildebrandt"),
  tags$h6("Code @ github.com/ryancahildebrandt/manyogana"),
  fluidRow(
    column(4, tags$h3("Input some Japanese....")),
    column(8, tags$h3("and here's your manyōgana!"),
           tags$h4("ジャジャーン"))
  ),
  fluidRow(
    column(4, textAreaInput("modern_user", "If you get an error, try inputting your text in a different script.\n I've found the transliteration works best if there are as many words as possible written in kanji where appropriate.\n For example, if something like こんにちは doesn't work, try こん日は.", rows = 5, value="よろしくお願い致します！")),
    column(8, textOutput("text"),verbatimTextOutput("text_code"))
  ),
  titlePanel("Arabic-to-Kansūji Converter"),
  fluidRow(
    column(4, tags$h3("Input a number....")),
    column(8, tags$h3("and here's your kansūji!"),
           tags$h4("ジャジャーン"))
  ),
  fluidRow(
    column(4, textInput("number_user", "In Arabic numerals, no commas or decimals please!\n The conversion should work up to 17 digits (##,###,###,###,###,###)", value=123456789)),
    column(8, textOutput("suuji"),verbatimTextOutput("suuji_code"))))

