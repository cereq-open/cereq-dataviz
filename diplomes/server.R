# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ###################### Create the scrolling menu ######################

  code <- reactive({
    req(input$niveau)
    as.numeric(filter(db_diplome, Libelle_Menu %in% input$niveau) %>% pull(Code))
  })
  
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

    code_niveau3 <- reactive({
    req(input$niveau)
    from <- as.numeric(code() + 1)
    to <- as.numeric(code() + 9)
    sequence <- seq(from, to, by = 1)
    as.numeric(filter(db_diplome, Code %in% sequence) %>% pull(Code))
  })
  
  # Do not display the second SeletInput when the first or the second level are selected (set condition as output from server).
    
    output$sousniveau <- reactive(!identical(code_niveau3() , numeric(0)))
    outputOptions(output, "sousniveau", suspendWhenHidden = FALSE)

  ###################### Define the data streams according to the levels selected ######################

  filtered_data <- reactive({
    generateDataForLevel(db_diplome, input$niveau)
  })

  filtered_data_level3 <- reactive({
    generateDataForLevel3(db_diplome, code_niveau3(), input$degre3)
  })

  selectedValue <- reactiveVal(NULL) # Use the reactive value selectedValue to keep track of the selected value from the second list of third levels.
  
  ###################### Create the graph_situation_apres_3_ans plots according to the selected level from the scrolling menu ######################
  
  reactive_graph_situation_apres_3_ans <- reactive({
  
    if (is.null(input$degre3)) {
      
      print("A")
      req(input$niveau)
      gg <- generatePlot(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = largeur_bar_chart,
             height_svg = hauteur_1_barre)
      
    }
    
    else if (!is.null(input$degre3)) {
      
      print("B")
      gg <- generatePlotSpec(db_diplome, code_niveau3(), input$degre3)
      girafe(ggobj = gg,
             width_svg = largeur_bar_chart,
             height_svg = hauteur_1_barre)
      
    }
    
  })
  
  output$graph_situation_apres_3_ans <- renderGirafe({
    reactive_graph_situation_apres_3_ans()
  })
  
  ###################### Create Donut Plots ######################
  
  reactive_plot_repartition_par_profession <- reactive({
    
    if (is.null(input$degre3)) {
      
      gg <- generateDonutProfession(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = largeur_donut_chart,
             height_svg = hauteur_donut_chart)
      
    }
    
    else if (!is.null(input$degre3)) {
      
      DT <- db_diplome %>%
        select(Code, Libelle_Menu, pos_cadres, pos_prof_int, pos_emp_ouv_q, pos_emp_ouv_nq, pos_autres) %>%
        filter(Code %in% code_niveau3() & Libelle_Menu %in% input$degre3) %>%
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
            profession == "pos_emp_ouv_q" ~ "Employés ou ouvriers qualifiés",
            profession == "pos_emp_ouv_nq" ~ "Employés ou ouvriers non qualifiés",
            profession == "pos_autres" ~ "Autres",
            TRUE ~ profession
          ),
          profession = factor(profession, levels = c(
            "Cadres", "Professions intermédiaires",
            "Employés ou ouvriers qualifiés",
            "Employés ou ouvriers non qualifiés",
            "Autres"
          )),
          taux_str = paste0(taux, "%"),
          tooltip_value = paste0(profession, " : ", taux_str)
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
        geom_rect_interactive(mapping = aes(data_id = profession, tooltip = tooltip_value), color = "white") +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        geom_text(x = 3.5, aes(y = labelPosition, label = taux_str), color = "white") +
        scale_fill_manual(values = colors, labels = scales::label_wrap(20),
                          guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))) +
        scale_y_continuous(trans = "reverse") +
        labs(caption = caption) +
        theme(legend.position = "top",
              legend.text = element_text(size = 9, face = "plain"),
              plot.caption = element_textbox_simple(
                hjust = 0,
                color = "#C0C0C2",
                size = 9
              ),
              axis.text.y = element_blank()) +
        guides(fill = guide_legend(ncol = 3, byrow = TRUE))
      
      girafe(ggobj = gg,
             width_svg = largeur_donut_chart,
             height_svg = hauteur_donut_chart)
      
    }
    
  })
  
  output$plot_repartition_par_profession <- renderGirafe({
    reactive_plot_repartition_par_profession()
  })
  
  reactive_plot_repartition_par_secteur <- reactive({
    
    if (is.null(input$degre3)) {
      
      gg <- generateDonutSecteur(db_diplome, input$niveau)
      girafe(ggobj = gg,
             width_svg = largeur_donut_chart,
             height_svg = hauteur_donut_chart)
      
    }
    
    else if (!is.null(input$degre3)) {
      
      DT <- db_diplome %>% 
        select(Code, Libelle_Menu, sec_industries_btp, sec_commerce, sec_administration, sec_a_services, sec_autres)
      
      if (isTruthy(code_niveau3())) {
        DT <- filter(DT, Code %in% code_niveau3())
      }
      
      if (isTruthy(input$degre3)) {
        DT <- filter(DT, Libelle_Menu %in% input$degre3)
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
            secteur == "sec_industries_btp" ~ "Industries, bâtiment et travaux publics",
            secteur == "sec_commerce" ~ "Commerce",
            secteur == "sec_administration" ~ "Administrations, Education, Santé Action sociale",
            secteur == "sec_a_services" ~ "Services",
            secteur == "sec_autres" ~ "Autres",
            TRUE ~ secteur
          ),
          secteur = factor(secteur, levels = c("Industries, bâtiment et travaux publics", "Commerce",
                                               "Administrations, Education, Santé Action sociale",
                                               "Services",
                                               "Autres")),
          taux_str = paste0(taux, "%"),
          tooltip_value = paste0(secteur, " : ", taux_str)
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
        geom_rect_interactive(mapping = aes(data_id = secteur, tooltip = tooltip_value), color = "white") +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        geom_text(x = 3.5, aes(y = labelPosition, label = taux_str), color = "white") +
        scale_fill_manual(values = colors, labels = scales::label_wrap(20),
                          guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))) +
        scale_y_continuous(trans = "reverse") +
        labs(caption = caption) +
        theme(legend.position = "top",
              legend.text = element_text(size = 9, face = "plain"),
              plot.caption = element_textbox_simple(
                hjust = 0,
                color = "#C0C0C2",
                size = 9
              ),
              axis.text.y = element_blank()) +
        guides(fill = guide_legend(ncol = 3, byrow = TRUE))
      
      girafe(ggobj = gg,
             width_svg = largeur_donut_chart,
             height_svg = hauteur_donut_chart)
      
    }
    
  })
  
  output$plot_repartition_par_secteur <- renderGirafe({
    reactive_plot_repartition_par_secteur()
  })
  
  ###################### Create Statistics ######################
  
  reactive_tx_en_emploi <- reactive({
    
    infobulle_str <- "Le taux d'emploi correspond à la part des individus en emploi parmi la population totale."
    info_str <- "En emploi"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str)
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_emploi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, "% pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str)
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_emploi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str)
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_emploi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_emploi, "% pour l'ensemble des sortants)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str)
      }
      
    }
    
  })
  
  output$tx_en_emploi <- renderUI({
    reactive_tx_en_emploi()
  })
  
  reactive_tx_chomage <- reactive({
    
    infobulle_str <- "Le taux de chômage correspond à la part des individus sans emploi et à la recherche d'un emploi parmi les actifs (individus en emploi ou au chômage)."
    info_str <- "Taux de chômage"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_chomage, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_chomage, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_chomage, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_chomage, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    }
    
  })
  
  output$tx_chomage <- renderUI({
    reactive_tx_chomage()
  })
  
  reactive_tx_en_edi <- reactive({
    
    infobulle_str <- "La proportion des individus en emploi non-salarié (personne à son compte ou aide familial), en contrat à durée indéterminée (CDI) ou avec le statut de fonctionnaire."
    info_str <- "En emploi à durée indéterminée"
    
    if (is.null(input$degre3)) {

      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$taux_edi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$taux_edi, "%")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$taux_edi, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$taux_edi, "%)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    } 
    
  })
  
  output$tx_en_edi <- renderUI({
    reactive_tx_en_edi()
  })
  
  reactive_tx_a_tps_partiel <- reactive({
    
    info_str <- "À temps partiel"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, "%")
        labellize_stats_no_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$part_tps_partiel, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, "%)"))
        labellize_stats_no_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$part_tps_partiel, "%")
        labellize_stats_no_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$part_tps_partiel, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$part_tps_partiel, "%)"))
        labellize_stats_no_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    } 
    
  })
  
  output$tx_a_tps_partiel <- renderUI({
    reactive_tx_a_tps_partiel()
  })
  
  reactive_revenu_median <- reactive({
    
    infobulle_str <- "Niveau de salaire ou traitement mensuel net primes incluses médian. Le revenu médian est la valeur telle que la moitié des individus de la population considérée gagne plus, l'autre moitié gagne moins."
    info_str <- "Revenu mensuel médian"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, " €")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$revenu_travail, " €")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, " €)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$revenu_travail, " €")
        labellize_stats_end_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$revenu_travail, " €")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$revenu_travail, " €)"))
        labellize_stats_end_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str,
          infobulle_str = infobulle_str
        )
      }
      
    } 
    
  })
  
  
  output$revenu_median <- renderUI({
    reactive_revenu_median()
  })
  
  reactive_tx_jugent_coherent <- reactive({
    
    info_str <- "jugent leur emploi cohérent avec leur formation initiale"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$correspondance_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$correspondance_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$correspondance_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$correspondance_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    }
    
  })
  
  output$tx_jugent_coherent <- renderUI({
    reactive_tx_jugent_coherent()
  })
  
  reactive_tx_estiment_ss_employes <- reactive({
    
    info_str <- "estiment être employés sous leur niveau de compétence"
    
    if (is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data()$competence_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    }
    
    else if (!is.null(input$degre3)) {
      
      req(input$niveau)
      if (input$niveau %in% ensemble_des_sortants) {
        text_info1 <- paste0(ensemble_de_sortants_data$competence_ok, "%")
        labellize_stats_row_i(
          stat1_str = text_info1, stat2_str = NULL, 
          info_str = info_str
        )
        
      } else {
        text_info2 <- paste0(filtered_data_level3()$competence_ok, "%")
        text_info3 <- paste0("(", paste0(ensemble_de_sortants_data$competence_ok, "%)"))
        labellize_stats_row_i(
          stat1_str = text_info2, stat2_str = text_info3, 
          info_str = info_str
        )
      }
      
    }
    
  })
  
  output$tx_estiment_ss_employes <- renderUI({
    reactive_tx_estiment_ss_employes()
  })

    # Download Data --------------------------------------------------------------
  
   output$downloadData <- downloadHandler(
     filename = function() {
       paste('OpenData_Cereq-Enq_Generation-Donnees_DIPLOME', '.xlsx', sep='')
    },
    content = function(file) {
       file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_DIPLOME.xlsx", file)
    }
   )

  # Download PDF --------------------------------------------------------------
  
 output$downloadPDF <- downloadHandler(
    
      "report.pdf"  # spécifier le nom du fichier pdf à télécharger
    ,
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "report.Rmd",
            output_format = "pdf_document",
            params = list(reactive_graph_situation_apres_3_ans = reactive_graph_situation_apres_3_ans(), 
                          reactive_plot_repartition_par_profession = reactive_plot_repartition_par_profession(),
                          reactive_plot_repartition_par_secteur = reactive_plot_repartition_par_secteur(),
                          reactive_tx_en_emploi = reactive_tx_en_emploi(),
                          reactive_tx_chomage = reactive_tx_chomage(),
                          reactive_tx_en_edi = reactive_tx_en_edi(),
                          reactive_tx_a_tps_partiel = reactive_tx_a_tps_partiel(),
                          reactive_revenu_median = reactive_revenu_median(),
                          reactive_tx_jugent_coherent = reactive_tx_jugent_coherent(),
                          reactive_tx_estiment_ss_employes = reactive_tx_estiment_ss_employes())
            )
          
          readBin(con = "report.pdf", 
                  what = "raw",
                  n = file.info("report.pdf")[, "size"]) %>%
            writeBin(con = file)

        }
    )
})
