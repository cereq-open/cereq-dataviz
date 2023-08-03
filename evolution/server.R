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
      write.xlsx("data/OpenData_Cereq-Enq_Generation-Donnees_EVOLUTION.xls", file)
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
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    generateTitle(titre)
  })

  output$part_chomage <- renderUI({
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "part_chomage") %>% pull(Titre_graphique)
    generateTitle(titre)
  })

  output$tx_chomage <- renderUI({
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_chomage") %>% pull(Titre_graphique)
    info_bulle <- tab_variables_evolution %>% filter(Nom_colonne == "taux_chomage") %>% pull(Bulle)
    generateTitle(titre,info_bulle)
  })

  # Partie 2 : Quelles sont les conditions d’emploi des jeunes en emploi trois ans après leur sortie ?

  output$tx_edi <- renderUI({
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_edi") %>% pull(Titre_graphique)
    info_bulle <- tab_variables_evolution %>% filter(Nom_colonne == "taux_edi") %>% pull(Bulle)
    generateTitle(titre,info_bulle)
  })

  output$part_tps_partiel <- renderUI({
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "part_tps_partiel") %>% pull(Titre_graphique)
    generateTitle(titre)
  })

  output$revenu_travail <- renderUI({
    titre <- tab_variables_evolution %>% filter(Nom_colonne == "revenu_travail") %>% pull(Titre_graphique)
    info_bulle <- tab_variables_evolution %>% filter(Nom_colonne == "revenu_travail") %>% pull(Bulle)
    generateTitle(titre,info_bulle)
  })

  output$comptence_ok <- renderUI({
    generateTitle("Le % de jeunes estimant être employé à leur niveau de compétence")
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