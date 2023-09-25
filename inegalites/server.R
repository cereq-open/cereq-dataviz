# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  graph_sizes <- eventReactive(input$dimension, {
    dims <- input$dimension

    if (dims[1] > 1200) {
      list(
        largeur_bar_chart = 15
      )
    } else if (dims[1] > 992) {
      list(
        largeur_bar_chart = 10
      )
    } else if (dims[1] > 576) {
      list(
        largeur_bar_chart = 7
      )
    } else {
      list(
        largeur_bar_chart = 4.8
      )
    }
  })

  filtered_data <- reactive({
    req(input$facteur, input$indicateur)
    data_par_facteur_analyse[[input$facteur]][[input$indicateur]]
  })


  output$graph <- renderGirafe({
    dat <- filtered_data()
    gg <- generatePlot(
      .data = dat,
      colors = color_palette(dat),
      .caption = indicateur_captions[[input$indicateur]],
      .title = indicateur_titles[[input$indicateur]]
    )
    girafe(
      ggobj = gg,
      width_svg = graph_sizes()$largeur_bar_chart,
      height_svg = generate_height(dat)
    )
  })

  # Download Data --------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OpenData_Cereq-Enq_Generation-Donnees_INEGALITES", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_INEGALITES.xlsx", file)
    }
  )
})
