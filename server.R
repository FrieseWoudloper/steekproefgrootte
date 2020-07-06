library(shiny)
library(ggplot2)
library(glue)

source("functies.R")

server <- function(input, output) {
  resultaat <- reactive({
    bereken_resultaat(p = input$sl_succeskans,
                      conf_level = input$sl_betrouwbaarheid,
                      gewenste_nauwkeurigheid = input$sl_nauwkeurigheid,
                      populatie = input$num_populatie)
  })
  
  output$grafiek <- renderPlot({
    plot_resultaat(resultaat())
  })
  
  output$samenvatting <- renderText({
    beschrijf_resultaat(resultaat())
  })
  
  output$debug_txt <- renderPrint({
    reactiveValuesToList(input)
  })
    resultaat <- reactive({
      bereken_resultaat(p = input$sl_succeskans,
                        conf_level = input$sl_betrouwbaarheid,
                        gewenste_nauwkeurigheid = input$sl_nauwkeurigheid,
                        populatie = input$num_populatie)
    })

    output$grafiek <- renderPlot({
      plot_resultaat(resultaat())
    })
    
    output$samenvatting <- renderText({
      beschrijf_resultaat(resultaat())
    })

    output$debug_txt <- renderPrint({
      reactiveValuesToList(input)
    })
}
