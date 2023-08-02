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
      paste("OpenData_Cereq-Enq_Generation-Donnees_EVOLUTION", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(open_data, file)
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
    labellize_stat("Taux d’emploi à trois ans")
  })

  output$part_chomage <- renderUI({
    labellize_stat("Proportion de sortants au chômage à trois ans")
  })

  output$tx_chomage <- renderUI({
    info_bulle <- "Le taux de chômage correspond à la part des individus sans emploi et à la recherche d'un emploi parmi les actifs (individus en emploi ou au chômage)"
    labellize_stat("Taux de chômage à trois ans", info_bulle)
  })

  # Partie 2 : Quelles sont les conditions d’emploi des jeunes en emploi trois ans après leur sortie ?

  output$tx_edi <- renderUI({
    info_bulle <- "Proportion d'individus en emploi non-salarié (personne à son compte ou aide familial), en contrat à durée indéterminée (CDI) ou avec le statut de fonctionnaire"
    labellize_stat("Proportion de sortants en emploi à durée indéterminé à trois ans", info_bulle)
  })

  output$part_tps_partiel <- renderUI({
    labellize_stat("Proportion de sortants en emploi à temps partiel à trois ans")
  })

  output$revenu_travail <- renderUI({
    info_bulle <- "Niveau de salaire ou traitement mensuel net primes incluses médian. Le revenu médian est la valeur telle que la moitié des individus de la population considérée gagne plus, l'autre moitié gagne moins."
    labellize_stat("Revenu mensuel médian à trois ans", info_bulle)
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
