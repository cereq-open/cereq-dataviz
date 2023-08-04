# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  # Download Data --------------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OpenData_Cereq-Enq_Generation-Donnees_EVOLUTION", ".xls", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_EVOLUTION.xls", file)
    }
  )

  # Filtered Data --------------------------------------------------------------

  filtered_data <- reactive({
    tab_evolution %>%
      filter(Libelle_Menu %in% input$indicateurs)
  })

  # Titre ----------------------------------------------------------------------

  # Partie 1 : Situation trois ans après la sortie de formation initiale

  output$tx_emploi <- renderUI({
    titre <- variables_evolution$Titre_graphique[1]
    labellize_stat(titre)
  })

  output$part_chomage <- renderUI({
    titre <- variables_evolution$Titre_graphique[2]
    labellize_stat(titre)
  })

  output$tx_chomage <- renderUI({
    titre <- variables_evolution$Titre_graphique[3]
    info_bulle <- variables_evolution$Bulle[3]
    labellize_stat(titre, info_bulle)
  })

  # Partie 2 : Quelles sont les conditions d’emploi des jeunes en emploi trois ans après leur sortie ?

  output$tx_edi <- renderUI({
    titre <- variables_evolution$Titre_graphique[4]
    info_bulle <- variables_evolution$Bulle[4]
    labellize_stat(titre, info_bulle)
  })

  output$part_tps_partiel <- renderUI({
    titre <- variables_evolution$Titre_graphique[5]
    labellize_stat(titre)
  })

  output$revenu_travail <- renderUI({
    titre <- variables_evolution$Titre_graphique[6]
    info_bulle <- variables_evolution$Bulle[6]
    labellize_stat(titre, info_bulle)
  })

  output$comptence_ok <- renderUI({
    labellize_stat("Le % de jeunes estimant être employé à leur niveau de compétence")
  })

  # Data Viz -------------------------------------------------------------------

  # Partie 1

  output$plot_tx_emploi <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "taux_emploi", caption_part_1)

    girafe(ggobj = gg)
  })

  output$plot_part_chomage <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "part_chomage", caption_part_1)

    girafe(ggobj = gg)
  })

  output$plot_tx_chomage <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "taux_chomage", caption_part_1)

    girafe(ggobj = gg)
  })

  # Partie 2

  output$plot_tx_edi <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "taux_edi", caption_part_2)

    girafe(ggobj = gg)
  })

  output$plot_part_tps_partiel <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "part_tps_partiel", caption_part_2)

    girafe(ggobj = gg)
  })

  output$plot_revenu_travail <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "revenu_travail", caption_part_2)

    girafe(ggobj = gg)
  })
  
  output$plot_comptence_ok <- renderGirafe({
    gg <- plot_barchart(filtered_data(), "competence_ok", caption_part_2)
    
    girafe(ggobj = gg)
  })
})
