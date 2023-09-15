# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ###################### Define the data streams according to the levels selected ######################

  filtered_data1 <- reactive({
    req(input$facteur)
    generateData(tab_inegalites %>% filter(Diplôme %in% c("Ensemble des sortants","Non-diplômés")), input$facteur)
  })
  
  filtered_data2 <- reactive({
    req(input$facteur)
    generateData(tab_inegalites %>% filter(Diplôme %in% c("Diplômés du secondaire","Diplômés du supérieur court","Diplômés du supérieur long")), input$facteur)
  })

  ###################### Create the graph according to the selected levels from both scrolling menus ######################

  reactive_indicateur <- reactive({
    dplyr::filter(tab_variables_inegalites, Titre_graphique %in% input$indicateur) %>% pull(Nom_colonne)
  })
  
  reactive_graph1 <- reactive({
      indicateur <- unlist(filtered_data1()[,reactive_indicateur()])
      DF <- filtered_data1() %>% mutate(indicateur = indicateur) %>% filter(indicateur != 0)
      gg <- generatePlot(DF, indicateur, generateColors(input$facteur), 
                         generateCaption(reactive_indicateur()),
                         generate_Nb_rows(input$facteur),
                         generate_legend_key_height(input$facteur),
                         generateSymbol(input$indicateur),
                         guides = NULL
                         )
      girafe(
        ggobj = gg,
        fonts = list(sans = "Arimo"),
        width_svg = generateWidth(input$facteur),
        height_svg = generateHeight(input$facteur)
      )
  })

  output$graph1 <- renderGirafe({
    reactive_graph1()
  })

  reactive_graph2 <- reactive({
    indicateur <- unlist(filtered_data2()[,reactive_indicateur()])
    DF <- filtered_data2() %>% mutate(indicateur = indicateur) %>% filter(indicateur != 0)
    gg <- generatePlot(DF, indicateur, generateColors(input$facteur), 
                       caption = NULL,
                       generate_Nb_rows(input$facteur),
                       generate_legend_key_height(input$facteur),
                       generateSymbol(input$indicateur),
                       guides(fill = "none")
    )
    girafe(
      ggobj = gg,
      fonts = list(sans = "Arimo"),
      width_svg = generateWidth(input$facteur),
      height_svg = generateHeight(input$facteur)
    )
  })
  
  output$graph2 <- renderGirafe({
    reactive_graph2()
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
      paste("OpenData_Cereq-Enq_Generation-Donnees_INEGALITES", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_INEGALITES.xlsx", file)
    }
  )
})