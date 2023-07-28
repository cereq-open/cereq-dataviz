# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ###################### Define the data streams according to the levels selected ######################

  filtered_data <- reactive({
    req(input$facteur)
    generateData(tab_inegalites, input$facteur)
    print(head(generateData(tab_inegalites, input$facteur)))
  })

  ###################### Create the graph_situation_apres_3_ans plots according to the selected level from the scrolling menu ######################

  reactive_indicateur <- reactive({
    dplyr::filter(tab_variables_inegalites, Titre_graphique %in% input$indicateur) %>% pull(Nom_colonne)
  })
  
  reactive_graph <- reactive({
    req(input$facteur)
      indicateur <- unlist(filtered_data()[,reactive_indicateur()])
      gg <- generatePlot(filtered_data(), indicateur)
      girafe(
        ggobj = gg,
        fonts = list(sans = "Arimo"),
        width_svg = largeur_bar_chart,
        height_svg = hauteur_1_barre
      )
  })

  output$graph <- renderGirafe({
    reactive_graph()
  })

  ###################### Create the graph title ######################

  reactive_titre_graph <- reactive({
    info_str <- input$indicateur
    infobulle_str <- dplyr::filter(tab_variables_inegalites, Titre_graphique %in% input$indicateur) %>% pull(Bulle)
    
    if (is.na(infobulle_str)) {
      labellize_stats_no_i(info_str = info_str)
    }
    else labellize_stats_end_i(info_str = info_str,
                               infobulle_str = infobulle_str)
  })
  
  output$titre_graph <- renderUI({
    reactive_titre_graph()
  })

  # Download Data --------------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OpenData_Cereq-Enq_Generation-Donnees_DIPLOME", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_DIPLOME.xlsx", file)
    }
  )
})