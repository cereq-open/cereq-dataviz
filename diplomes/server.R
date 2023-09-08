# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  ###################### Create the scrolling menu ######################

  observeEvent(input$niveau, {
    code <- as_code(tab_diplome, input$niveau)

    if (!is.null(code) & str_sub(code, -2, -1) != "00") {
      from <- as.numeric(code + 1)
      to <- as.numeric(code + 9)
      sequence <- seq(from, to, by = 1)
      values <- dplyr::filter(tab_diplome, Code %in% sequence) %>% pull(Libelle_Menu)
    } else {
      values <- character()
    }

    updateRadioGroupButtons(
      session = session,
      inputId = "degre3",
      choices = values,
      selected = "Ensemble"
    )
  })

  code_niveau3 <- reactive({
    req(input$niveau)
    code <- as_code(tab_diplome, input$niveau)
    from <- as.numeric(code + 1)
    to <- as.numeric(code + 9)
    sequence <- seq(from, to, by = 1)
    as.numeric(filter(tab_diplome, Code %in% sequence) %>% pull(Code))
  })

  # Do not display the second SeletInput when the first or the second level are selected (set condition as output from server).

  output$sousniveau <- reactive(!identical(code_niveau3(), numeric(0)))
  outputOptions(output, "sousniveau", suspendWhenHidden = FALSE)

  ###################### Define the data streams according to the levels selected ######################

  filtered_data <- reactive({
    generateDataForLevel(tab_diplome, input$niveau)
  })

  filtered_data_level3 <- reactive({
    generateDataForLevel3(tab_diplome, code_niveau3(), input$degre3)
  })

  ###################### Create the barplot ######################
  
  reactive_graph_situation_apres_3_ans <- reactive({
    if (is.null(input$degre3)) {
      gg <- generatePlot(tab_diplome, input$niveau)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_bar_chart,
        height_svg = hauteur_1_barre
      )
    } else if (!is.null(input$degre3)){
      gg <- generatePlotSpec(tab_diplome, code_niveau3(), input$degre3)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_bar_chart,
        height_svg = hauteur_1_barre
      )
    }
  }) %>% debounce(500)

  output$graph_situation_apres_3_ans <- renderGirafe({
    reactive_graph_situation_apres_3_ans()
  })

  ###################### Create Donut Charts ######################

  reactive_plot_repartition_par_profession <- reactive({
    
    if (is.null(input$degre3)) {
      caption <- generateCaptionDonutChart(input$niveau)
      gg <- generateDonutProfession(tab_diplome, input$niveau, caption)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_donut_chart,
        height_svg = hauteur_donut_chart
      )
    } else if (!is.null(input$degre3)) {
      caption <- generateCaptionDonutChart(input$niveau, input$degre3)
      gg <- generateDonutProfessionSpec(tab_diplome, code_niveau3(), input$degre3, caption)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_donut_chart,
        height_svg = hauteur_donut_chart
      )
    }
  }) %>% debounce(500)

  output$plot_repartition_par_profession <- renderGirafe({
    reactive_plot_repartition_par_profession()
  })

  reactive_plot_repartition_par_secteur <- reactive({
    if (is.null(input$degre3)) {
      caption <- generateCaptionDonutChart(input$niveau)
      gg <- generateDonutSecteur(tab_diplome, input$niveau, caption)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_donut_chart,
        height_svg = hauteur_donut_chart
      )
    } else if (!is.null(input$degre3)) {
      caption <- generateCaptionDonutChart(input$niveau, input$degre3)
      gg <- generateDonutSecteurSpec(tab_diplome, code_niveau3(), input$degre3, caption)
      girafe(
        ggobj = gg,
        fonts = fonts_arimo,
        width_svg = largeur_donut_chart,
        height_svg = hauteur_donut_chart
      )
    }
  }) %>% debounce(500)

  output$plot_repartition_par_secteur <- renderGirafe({
    reactive_plot_repartition_par_secteur()
  })

  ###################### Create Statistics ######################

  reactive_tx_en_emploi <- reactive({
    infobulle_str <- variables_diplome %>% filter(Nom_colonne == "taux_emploi") %>% pull(Bulle)
    info_str <- "En emploi"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$taux_emploi, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_emploi, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_en_emploi <- renderUI({
    reactive_tx_en_emploi()
  })

  reactive_tx_chomage <- reactive({
    infobulle_str <- variables_diplome %>% filter(Nom_colonne == "taux_chomage") %>% pull(Bulle)
    info_str <- "Taux de chômage"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$taux_chomage, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_chomage, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_chomage <- renderUI({
    reactive_tx_chomage()
  })

  reactive_tx_en_edi <- reactive({
    infobulle_str <- variables_diplome %>% filter(Nom_colonne == "taux_edi") %>% pull(Bulle)
    info_str <- "En emploi à durée indéterminée"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$taux_edi, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_edi, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_en_edi <- renderUI({
    reactive_tx_en_edi()
  })

  reactive_tx_a_tps_partiel <- reactive({
    info_str <- "À temps partiel"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$part_tps_partiel, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, symbole_pourcentage)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$part_tps_partiel, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_a_tps_partiel <- renderUI({
    reactive_tx_a_tps_partiel()
  })

  reactive_revenu_median <- reactive({
    infobulle_str <- variables_diplome %>% filter(Nom_colonne == "revenu_travail") %>% pull(Bulle)
    info_str <- "Revenu mensuel médian"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, symbole_euro)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$revenu_travail, symbole_euro)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, symbole_euro, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, symbole_euro)
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$revenu_travail, symbole_euro)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, symbole_euro, " pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
    }
  }) %>% debounce(500)


  output$revenu_median <- renderUI({
    reactive_revenu_median()
  })

  reactive_tx_jugent_coherent <- reactive({
    info_str <- "jugent leur emploi cohérent avec leur formation initiale"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, symbole_pourcentage)
        labellize_stats_row(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$correspondance_ok, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_row(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, symbole_pourcentage)
        labellize_stats_row(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$correspondance_ok, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_row(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_jugent_coherent <- renderUI({
    reactive_tx_jugent_coherent()
  })

  reactive_tx_estiment_ss_employes <- reactive({
    info_str <- "estiment être employés sous leur niveau de compétence"

    if (is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, symbole_pourcentage)
        labellize_stats_row(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data()$competence_ok, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_row(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    } else if (!is.null(input$degre3)) {
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, symbole_pourcentage)
        labellize_stats_row(
          stat1_str = text_info1, stat2_str = NULL,
          info_str = info_str
        )
      } else {
        text_info2 <- paste0(filtered_data_level3()$competence_ok, symbole_pourcentage)
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, symbole_pourcentage, " pour l'ensemble des sortants)"))
        labellize_stats_row(
          stat1_str = text_info2, stat2_str = text_info3,
          info_str = info_str
        )
      }
    }
  }) %>% debounce(500)

  output$tx_estiment_ss_employes <- renderUI({
    reactive_tx_estiment_ss_employes()
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