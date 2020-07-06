library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("Berekenen van de steekproefgrootte"),
  
  fluidRow(column(2,
                  sliderInput("sl_succeskans",
                              "Verwachte kans op succes:",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              step = 0.01),
                  sliderInput("sl_betrouwbaarheid",
                              "Betrouwbaarheid:",
                              min = 0,
                              max = 1,
                              value = 0.95,
                              step = 0.01),
                  sliderInput("sl_nauwkeurigheid",
                              "Gewenste nauwkeurigheid:",
                              min = 0,
                              max = 0.5,
                              value = 0.05,
                              step = 0.01),
                  # selectInput("sel_formule",
                  #             "steekproef o.b.v.",
                  #             c("Benaderingsformule", "Uitgebreide formule")),
                  numericInput("num_populatie",
                               "Grootte van de populatie",
                               value = 1000000,
                               min = 1)
  ),
  column(10,
         plotOutput("grafiek")
  )
  ),
  
  fluidRow(verbatimTextOutput("samenvatting")) #,
  
#  fluidRow(verbatimTextOutput("debug_txt"))
)
