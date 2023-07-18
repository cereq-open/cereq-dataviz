# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  ###################### Create the scrolling menu ######################

  code <- reactive({
    req(input$niveau)
    as.numeric(filter(db_diplome, Libelle_Menu %in% input$niveau) %>% pull(Code))
  })

  # Do not display the second SeletInput when the first or the second level are selected (set condition as output from server).
  output$sousniveau <- reactive(str_sub(code(), -2, -1) != "00")
  outputOptions(output, "sousniveau", suspendWhenHidden = FALSE)

  observeEvent(code(), {
    if (!is.null(code()) & str_sub(code(), -2, -1) != "00") {
      from <- as.numeric(code() + 1)
      to <- as.numeric(code() + 9)
      sequence <- seq(from, to, by = 1)
      values <- dplyr::filter(db_diplome, Code %in% sequence) %>% pull(Libelle_Menu)
    } else {
      values <- character()
    }
    updateSelectInput(
      inputId = "degre3",
      label = NULL,
      choices = values
    )
  })

  niveau3 <- reactive({
    req(input$degre3)
  })

  code_niveau3 <- reactive({
    req(input$niveau)
    from <- as.numeric(code() + 1)
    to <- as.numeric(code() + 9)
    sequence <- seq(from, to, by = 1)
    as.numeric(filter(db_diplome, Code %in% sequence) %>% pull(Code))
  })

  ###################### Define the data streams according to the levels selected ######################

  filtered_data <- reactive({
    generateDataForLevel(db_diplome, input$niveau)
  })

  filtered_data_level3 <- reactive({
    generateDataForLevel3(db_diplome, code_niveau3(), input$degre3)
  })

  ###################### Create the graph_situation_apres_3_ans plots according to the selected level from the scrolling menu ######################

  observeEvent(input$niveau, {
    output$graph_situation_apres_3_ans <- renderGirafe({
      gg <- generatePlot(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 2.5)
    })
  })

  selectedValue <- reactiveVal(NULL) # Use the reactive value selectedValue to keep track of the selected value from the second list of third levels.

  observeEvent(input$degre3, {
    selectedValue(input$degre3)
  })

  observeEvent(input$degre3, {
    output$graph_situation_apres_3_ans <- renderGirafe({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      gg <- generatePlotSpec(db_diplome, code_niveau3(), input$degre3)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 2.5)
    })
  })

  ###################### Create Pie Plots ######################

  observeEvent(input$niveau, {
    output$plot_repartition_par_profession <- renderGirafe({
      gg <- generateDonutProfession(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })
  })

  observeEvent(input$niveau, {
    output$plot_repartition_par_secteur <- renderGirafe({
      gg <- generateDonutSecteur(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })
  })

  observeEvent(input$degre3, {
    output$plot_repartition_par_profession <- renderGirafe({
      req(selectedValue(), code_niveau3(), input$degre3) # Make sure that the selected value is not NULL before rendering the plot.
      DT <- db_diplome %>%
        select(Code, Libelle_Menu, pos_cadres, pos_prof_int, pos_emp_ouv_q, pos_emp_ouv_nq, pos_autres) %>%
        filter(Code == code_niveau3() & Libelle_Menu == input$degre3) %>%
        mutate(across(everything(), ~ gsub(",", ".", .))) %>%
        pivot_longer(
          cols = c("pos_cadres", "pos_prof_int", "pos_emp_ouv_q", "pos_emp_ouv_nq", "pos_autres"),
          names_to = "profession",
          values_to = "taux"
        ) %>%
        mutate(taux = as.numeric(taux)) %>%
        mutate(
          fraction = taux / sum(taux), # Calculer les pourcentages
          ymax = cumsum(fraction), # Calculer les pourcentages cumulés (en haut de chaque rectangle)
          ymin = c(0, head(ymax, n = -1)), # Calculer le bas de chaque rectangle
          labelPosition = (ymax + ymin) / 2,
          label = paste0(profession, "\n ", taux),
          profession = case_when(
            profession == "pos_cadres" ~ "Cadres",
            profession == "pos_prof_int" ~ "Professions intermédiaires",
            profession == "pos_emp_ouv_q" ~ "Ouvriers et employés qualifiés",
            profession == "pos_emp_ouv_nq" ~ "Ouvriers et employés non qualifiés",
            profession == "pos_autres" ~ "Autres professions",
            TRUE ~ profession
          ),
          profession = factor(profession, levels = c("Cadres", "Professions intermédiaires",
                                                     "Ouvriers et employés qualifiés",
                                                     "Ouvriers et employés non qualifiés",
                                                     "Autres professions"))
        )

      colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

      caption <- paste0(
        '<span style="color:#008B99;">Champ : </span>',
        "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
        "<br>",
        '<span style="color:#008B99;">Source : </span>',
        "Céreq, enquête Génération 2017 à trois ans."
      )

      gg <- ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
        geom_rect_interactive(mapping = aes(data_id = profession), color = "gray") +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        geom_text(aes(x = 3.5, y = labelPosition, label = taux), color = "white") +
        scale_fill_manual(values = colors) +
        scale_y_continuous(trans = "reverse") +
        labs(caption = caption) +
        theme(legend.position = "left",
              axis.text.y = element_blank())
      
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })
  })

  observeEvent(input$degre3, {
    output$plot_repartition_par_secteur <- renderGirafe({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.

      DT <- db_diplome %>%
        select(Code, Libelle_Menu, sec_industries_btp, sec_commerce, sec_administration, sec_a_services, sec_autres)

      if (isTruthy(code_niveau3())) {
        DT <- filter(DT, Code == code_niveau3())
      }

      if (isTruthy(input$degre3)) {
        DT <- filter(DT, Libelle_Menu == input$degre3)
      }
      DT <- DT %>%
        mutate(across(everything(), ~ gsub(",", ".", .))) %>%
        pivot_longer(
          cols = c("sec_industries_btp", "sec_commerce", "sec_administration", "sec_a_services", "sec_autres"),
          names_to = "secteur",
          values_to = "taux"
        ) %>%
        mutate(taux = as.numeric(taux)) %>%
        mutate(
          fraction = taux / sum(taux), # Calculer les pourcentages
          ymax = cumsum(fraction), # Calculer les pourcentages cumulés (en haut de chaque rectangle)
          ymin = c(0, head(ymax, n = -1)), # Calculer le bas de chaque rectangle
          labelPosition = (ymax + ymin) / 2,
          label = paste0(secteur, "\n ", taux),
          secteur = case_when(
            secteur == "sec_industries_btp" ~ "Industrie et BTP",
            secteur == "sec_commerce" ~ "Commerce",
            secteur == "sec_administration" ~ "Administration",
            secteur == "sec_a_services" ~ "Autres services",
            secteur == "sec_autres" ~ "Autres secteurs",
            TRUE ~ secteur
          ),
          secteur = factor(secteur, levels = c("Industrie et BTP", "Commerce",
                                               "Administration",
                                               "Autres services",
                                               "Autres secteurs"))
        )

      colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

      caption <- paste0(
        '<span style="color:#008B99;">Champ : </span>',
        "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
        "<br>",
        '<span style="color:#008B99;">Source : </span>',
        "Céreq, enquête Génération 2017 à trois ans."
      )

      gg <- ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
        geom_rect_interactive(mapping = aes(data_id = secteur), color = "gray") +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        geom_text(aes(x = 3.5, y = labelPosition, label = taux), color = "white") +
        scale_fill_manual(values = colors) +
        scale_y_continuous(trans = "reverse") +
        labs(caption = caption) +
        theme(legend.position = "left",
              axis.text.y = element_blank())
      
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })
  })


  ###################### Create Statistics ######################
  ####### input$niveau #########
  
  # text_info_reactive <- reactive({
  #   if (is.null(input$degre3)) {
  #    paste0(ensemble_de_sortants_data$taux_emploi, "%")
  # } else {
      
  #  }
  # })
  
  output$tx_en_emploi <- renderUI({
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, "%")
      labellize_stats_middle_i(
         stat1_str = text_info1, stat2_str = NULL, 
         info_str = "En emploi",
         infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")

    } else {
      text_info2 <- paste0(filtered_data()$taux_emploi, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, "%)"))
      labellize_stats_middle_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "En emploi",
        infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")
    }
  })

  output$tx_chomage <- renderUI({
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, "%")
      labellize_stats_middle_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "Taux de chômage",
        infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$taux_chomage, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, "%)"))
      labellize_stats_middle_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "Taux de chômage",
        infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        )
    }
    
  })
  
  output$tx_en_edi <- renderUI({
    
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, "%")
      labellize_stats_end_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "En emploi à durée indéterminée",
        infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$taux_edi, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, "%)"))
      labellize_stats_end_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "En emploi à durée indéterminée",
        infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
      )
   }

  })
  
  output$tx_a_tps_partiel <- renderUI({
    
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, "%")
      labellize_stats_end_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "À temps partiel",
        infobulle_str = ""
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$part_tps_partiel, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, "%)"))
      labellize_stats_end_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "À temps partiel",
        infobulle_str = ""
      )
    }

  })
  
  output$revenu_median <- renderUI({
    
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, " €")
      labellize_stats_end_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "Revenu mensuel médian",
        infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$revenu_travail, " €")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, " €)"))
      labellize_stats_end_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "Revenu mensuel médian",
        infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
      )
    }
   })
  
  output$tx_jugent_coherent <- renderUI({
    
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, "%")
      labellize_stats_row_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "jugent leur emploi cohérent avec leur formation initiale",
        infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$correspondance_ok, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, "%)"))
      labellize_stats_row_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "jugent leur emploi cohérent avec leur formation initiale",
        infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
      )
    }

  })

  output$tx_estiment_ss_employes <- renderUI({
    
    req(input$niveau)
    if (input$niveau %in% "Ensemble des sortants") {
      text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, "%")
      labellize_stats_row_i(
        stat1_str = text_info1, stat2_str = NULL, 
        info_str = "estiment être employés sous leur niveau de compétence",
        infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
      )
      
    } else {
      text_info2 <- paste0(filtered_data()$competence_ok, "%")
      text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, "%)"))
      labellize_stats_row_i(
        stat1_str = text_info2, stat2_str = text_info3, 
        info_str = "estiment être employés sous leur niveau de compétence",
        infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
      )
    }

  })
  

  


  ####### input$degre3 #########

  observeEvent(input$degre3, {
    
    
    output$tx_en_emploi <- renderUI({
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, "%")
        labellize_stats_middle_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "En emploi",
          infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_emploi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, "%)"))
        labellize_stats_middle_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "En emploi",
          infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")
      }
    })
    
    output$tx_chomage <- renderUI({
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, "%")
        labellize_stats_middle_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "Taux de chômage",
          infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_chomage, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, "%)"))
        labellize_stats_middle_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "Taux de chômage",
          infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        )
      }
      
    })
    
    output$tx_en_edi <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "En emploi à durée indéterminée",
          infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_edi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "En emploi à durée indéterminée",
          infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
        )
      }
      
    })
    
    output$tx_a_tps_partiel <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "À temps partiel",
          infobulle_str = ""
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$part_tps_partiel, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "À temps partiel",
          infobulle_str = ""
        )
      }
      
    })
    
    output$revenu_median <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, " €")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "Revenu mensuel médian",
          infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$revenu_travail, " €")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, " €)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "Revenu mensuel médian",
          infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
        )
      }
    })
    
    output$tx_jugent_coherent <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "jugent leur emploi cohérent avec leur formation initiale",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$correspondance_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "jugent leur emploi cohérent avec leur formation initiale",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
      }
      
    })
    
    output$tx_estiment_ss_employes <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "estiment être employés sous leur niveau de compétence",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$competence_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "estiment être employés sous leur niveau de compétence",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
      }
      
    })
    
  })

  ######### Click on the Clear button ########################

  observeEvent(input$clear, {
    updateSelectInput(session, "degre3", selected = character()) # When the clear button is clicked, we update the selectInput to have a selected value of NULL.
    selectedValue(NULL) # We set selectedValue to NULL as well.

    output$graph_situation_apres_3_ans <- renderGirafe({
       gg <- generatePlot(db_diplome, input$niveau)
       girafe(ggobj = gg,
              width_svg = 6,
              height_svg = 2.5)
       
    })

    output$plot_repartition_par_profession <- renderGirafe({
      gg <- generateDonutProfession(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })

    output$plot_repartition_par_secteur <- renderGirafe({
      gg <- generateDonutSecteur(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = 6,
             height_svg = 4)
    })

    output$tx_en_emploi <- renderUI({
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, "%")
        labellize_stats_middle_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "En emploi",
          infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_emploi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, "%)"))
        labellize_stats_middle_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "En emploi",
          infobulle_str = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants.")
      }
    })
    
    
    output$tx_chomage <- renderUI({
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, "%")
        labellize_stats_middle_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "Taux de chômage",
          infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_chomage, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, "%)"))
        labellize_stats_middle_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "Taux de chômage",
          infobulle_str = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        )
      }
      
    })
    
    output$tx_en_edi <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "En emploi à durée indéterminée",
          infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_edi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "En emploi à durée indéterminée",
          infobulle_str = "Fonctionnaires et salariés en contrats à durée indéterminée."
        )
      }
      
    })
    
    output$tx_a_tps_partiel <- renderUI({
      
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "À temps partiel",
          infobulle_str = ""
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$part_tps_partiel, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "À temps partiel",
          infobulle_str = ""
        )
      }
      
      
    })
    
    output$revenu_median <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, " €")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "Revenu mensuel médian",
          infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$revenu_travail, " €")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, " €)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "Revenu mensuel médian",
          infobulle_str = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
        )
      }
    })

    output$tx_jugent_coherent <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "jugent leur emploi cohérent avec leur formation initiale",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$correspondance_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "jugent leur emploi cohérent avec leur formation initiale",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
      }
      
    })
  
    output$tx_estiment_ss_employes <- renderUI({
      
      req(input$niveau)
      if (input$niveau %in% "Ensemble des sortants") {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = "estiment être employés sous leur niveau de compétence",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$competence_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = "estiment être employés sous leur niveau de compétence",
          infobulle_str = "Question d’opinion posée aux jeunes en emploi trois ans après leur sortie de formation initiale."
        )
      }
      
    })

     })
  
  # Download Data --------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('db_diplome-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(db_diplome, file)
    }
  )
  
})
