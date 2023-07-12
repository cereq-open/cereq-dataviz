# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session){
  
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
      label = "Choisir la spécialité",
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
    output$graph_situation_apres_3_ans<- renderPlot({
      generatePlot(db_diplome, input$niveau)
    })
  })
  
  selectedValue <- reactiveVal(NULL) # Use the reactive value selectedValue to keep track of the selected value from the second list of third levels.
  
  observeEvent(input$degre3, {
    selectedValue(input$degre3)
  })
  
  observeEvent(input$degre3, {
    output$graph_situation_apres_3_ans <- renderPlot({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      generatePlotSpec(db_diplome,code_niveau3(),input$degre3)
    })
  })
  
  ###################### Create Pie Plots ###################### 
  
  observeEvent(input$niveau, {
    output$plot_repartition_par_profession <- renderPlot({
      generateDonutProfession(db_diplome, input$niveau)
    })
  })
  
  observeEvent(input$niveau, {
    output$plot_repartition_par_secteur <- renderPlot({
      generateDonutSecteur(db_diplome, input$niveau)
    })
  })
  
  observeEvent(input$degre3, {
    output$plot_repartition_par_profession <- renderPlot({
      req(selectedValue(), code_niveau3(), input$degre3) # Make sure that the selected value is not NULL before rendering the plot.
      DT <- db_diplome %>%
        select(Code, Libelle_Menu, pos_cadres,	pos_prof_int,	pos_emp_ouv_q,	pos_emp_ouv_nq,	pos_autres) %>%
        filter(Code == code_niveau3() & Libelle_Menu == input$degre3) %>%
        mutate(across(everything(), ~ gsub(",", ".", .))) %>%
        pivot_longer(
          cols = c("pos_cadres",	"pos_prof_int",	"pos_emp_ouv_q",	"pos_emp_ouv_nq",	"pos_autres"),
          names_to = "profession",
          values_to = "taux") %>% 
        mutate(taux = as.numeric(taux)) %>% 
        mutate(fraction = taux / sum(taux), # Calculer les pourcentages
               ymax = cumsum(fraction),  # Calculer les pourcentages cumulés (en haut de chaque rectangle)
               ymin = c(0, head(ymax, n=-1)), # Calculer le bas de chaque rectangle
               labelPosition = (ymax + ymin) / 2,
               label = paste0(profession, "\n ", taux)) 
      
      colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
      
      ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
        geom_rect() +
        coord_polar(theta = "y") + 
        xlim(c(2, 4)) + 
        geom_label(x = 3.5, aes(y = labelPosition, label = taux), size = 6) +
        scale_fill_manual(values = colors)
    })
  })
  
  observeEvent(input$degre3, {
    output$plot_repartition_par_secteur <- renderPlot({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      
      DT <- db_diplome %>%
        select(Code, Libelle_Menu, sec_industries_btp,	sec_commerce,	sec_administration,	sec_a_services,	sec_autres)
      
      if (isTruthy(code_niveau3())) {
        # print(code_niveau3())
        DT <- filter(DT, Code == code_niveau3())
      }

      if (isTruthy(input$degre3)) {
        # print(input$degre3)
        DT <- filter(DT, Libelle_Menu == input$degre3)
      }
      DT <- DT %>% 
        mutate(across(everything(), ~ gsub(",", ".", .))) %>% 
        pivot_longer(
          cols = c("sec_industries_btp",	"sec_commerce",	"sec_administration",	"sec_a_services",	"sec_autres"),
          names_to = "secteur",
          values_to = "taux") %>% 
        mutate(taux = as.numeric(taux)) %>% 
        mutate(fraction = taux / sum(taux), # Calculer les pourcentages
               ymax = cumsum(fraction),  # Calculer les pourcentages cumulés (en haut de chaque rectangle)
               ymin = c(0, head(ymax, n=-1)), # Calculer le bas de chaque rectangle
               labelPosition = (ymax + ymin) / 2,
               label = paste0(secteur, "\n ", taux)) 
      
      colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
      
      ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
        geom_rect() +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) + 
        geom_label(x = 3.5, aes(y = labelPosition, label = taux), size = 6) +
        scale_fill_manual(values = colors)
    })
  })
  
  
  ###################### Create Statistics ###################### 
  
  observeEvent(input$niveau, {
    output$tx_jugent_coherent <- renderUI({
      
      text_info <- paste0(filtered_data()$correspondance_ok, "%" , ' ' ,'jugent leur emploi cohérent avec leur formation initiale')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
      
    })
    
    output$tx_estiment_ss_employes <- renderUI({
      
      text_info <- paste0(filtered_data()$competence_ok, "%", ' ' ,'estiment être employés sous leur niveau de compétence')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
      
    })
    
    output$tx_en_emploi <- renderText({
      
      paste(paste0(filtered_data()$taux_emploi, "% "),"(",paste0(ensemble_de_sortants_data$taux_emploi, "%)"))
      
    })
    
    output$tx_chomage <- renderText({
      
      paste0(filtered_data()$taux_chomage, "%")
    })
    
    output$tx_en_edi <- renderText({
      
      paste0(filtered_data()$taux_edi, "%")
      
    })
    
    output$tx_a_tps_partiel <- renderText({
      
      paste0(filtered_data()$part_tps_partiel, "%")
      
    })
    
    output$revenu_median <- renderText({
      
      paste0(filtered_data()$revenu_travail, ' ' ,"€")
      
    })
    
  })
  
  observeEvent(input$degre3, {
    
    output$tx_jugent_coherent <- renderUI({
      
      text_info <- paste0(filtered_data_level3()$correspondance_ok, "%" , ' ' ,'jugent leur emploi cohérent avec leur formation initiale')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
      
    })
    
    output$tx_estiment_ss_employes <- renderUI({
      
      text_info <- paste0(filtered_data_level3()$competence_ok, "%", ' ' ,'estiment être employés sous leur niveau de compétence')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
      
    })
    
    output$tx_en_emploi <- renderText({
      
      paste0(filtered_data_level3()$taux_emploi, "%")
      
    })
    
    output$tx_chomage <- renderText({
      
      paste0(filtered_data_level3()$taux_chomage, "%")
    })
    
    output$tx_en_edi <- renderText({
      
      paste0(filtered_data_level3()$taux_edi, "%")
      
    })
    
    output$tx_a_tps_partiel <- renderText({
      
      paste0(filtered_data_level3()$part_tps_partiel, "%")
      
    })
    
    output$revenu_median <- renderText({
      
      paste0(filtered_data_level3()$revenu_travail, ' ' ,"€")
      
    })
    
  })
  
  ######### Click on the Clear button ########################
  
  observeEvent(input$clear, {
    updateSelectInput(session, "degre3", selected = character()) # When the clear button is clicked, we update the selectInput to have a selected value of NULL.
    selectedValue(NULL) # We set selectedValue to NULL as well.
    
    output$graph_situation_apres_3_ans <- renderPlot({
      generatePlot(db_diplome, input$niveau)
    })
    
    output$plot_repartition_par_profession <- renderPlot({
      generateDonutProfession(db_diplome, input$niveau)
    })
    
    output$plot_repartition_par_secteur <- renderPlot({
      generateDonutSecteur(db_diplome, input$niveau)
    })
    
    output$tx_jugent_coherent <- renderUI({
      
      text_info <- paste0(filtered_data()$correspondance_ok, "%", ' ', 'jugent leur emploi cohérent avec leur formation initiale')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
    })
    
    output$tx_estiment_ss_employes <- renderUI({
      
      text_info <- paste0(filtered_data()$competence_ok, "%", ' ' ,'estiment être employés sous leur niveau de compétence')
      
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          text_info
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol")
      )
      
    })
    
    output$tx_en_emploi <- renderText({
      paste0(filtered_data()$taux_emploi, "%")
    })
    
    
    output$tx_chomage <- renderText({
      
      paste0(filtered_data()$taux_chomage, "%")
    })
    
    output$tx_en_edi <- renderText({
      
      paste0(filtered_data()$taux_edi, "%")
      
    })
    
    output$tx_a_tps_partiel <- renderText({
      
      paste0(filtered_data()$part_tps_partiel, "%")
      
    })
    
    output$revenu_median <- renderText({
      
      paste0(filtered_data()$revenu_travail, ' ' ,"€")
      
    })
    
  })
  
})
