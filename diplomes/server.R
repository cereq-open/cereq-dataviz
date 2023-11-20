# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  graph_sizes <- eventReactive(input$dimension, {
    dims <- input$dimension
    if (dims[1] > 1200) {
      list(
        largeur_bar_chart = 12,
        hauteur_1_barre = 3,
        hauteur_2_barre = 4,
        hauteur_donut_chart = 10,
        largeur_donut_chart = 8,
        donut_col_legend = 2
      )
    } else if (dims[1] > 800) {
      list(
        largeur_bar_chart = 10,
        hauteur_1_barre = 3,
        hauteur_2_barre = 4,
        hauteur_donut_chart = 8.2,
        largeur_donut_chart = 6,
        donut_col_legend = 1
      )
    } else {
      list(
        largeur_bar_chart = 4.7,
        hauteur_1_barre = 3.5,
        hauteur_2_barre = 4.5,
        hauteur_donut_chart = 7.2,
        largeur_donut_chart = 4,
        donut_col_legend = 1
      )
    }
  })

  global <- reactiveValues(data_key_value = "100")

  ### buttons -----
  output$sub_type_diploma <- renderUI({
    req(input$level_diploma)
    subkeys <- menus_data[menus_data$key %in% input$level_diploma, "subkeys"][[1]]
    req(subkeys)
    props <- setNames(subkeys$subkey |> as.character(), subkeys$Libelle_Menu)
    radioGroupButtons(
      inputId = "sub_key",
      choices = props,
      selected = props[1],
      individual = TRUE
    )
  })

  observeEvent(input$sub_key, {
    if (isTruthy(input$sub_key)) {
      global$data_key_value <- input$sub_key
    } else {
      global$data_key_value <- input$level_diploma
    }
  })

  observeEvent(input$level_diploma, {
    global$data_key_value <- input$level_diploma
  })

  ###################### barplot ######################
  output$graph_situation_apres_3_ans <- renderGirafe({
    gg <- generatePlotSpec(
      .data = barchart_datasets_subsets[[global$data_key_value]],
      .caption = barchart_captions_subsets[[global$data_key_value]]
    )

    height_svg <- graph_sizes()$hauteur_2_barre
    if (global$data_key_value %in% "100") {
      height_svg <- graph_sizes()$hauteur_1_barre
    }

    girafe(
      ggobj = gg,
      width_svg = graph_sizes()$largeur_bar_chart,
      height_svg = height_svg
    )
  })

  ###################### Donut Charts ######################


  output$plot_repartition_par_profession <- renderGirafe({
    gg <- generateDonutProfession(
      .data = donut_profession_datasets_subsets[[global$data_key_value]],
      .caption = donuts_captions_subsets[[global$data_key_value]],
      .donut_col_legend = graph_sizes()$donut_col_legend
    )
    girafe(
      ggobj = gg,
      width_svg = graph_sizes()$largeur_donut_chart,
      height_svg = graph_sizes()$hauteur_donut_chart
    )
  })

  output$plot_repartition_par_secteur <- renderGirafe({
    gg <- generateDonutSecteur(
      .data = donut_secteur_datasets_subsets[[global$data_key_value]],
      .caption = donuts_captions_subsets[[global$data_key_value]],
      .donut_col_legend = graph_sizes()$donut_col_legend
    )
    girafe(
      ggobj = gg,
      width_svg = graph_sizes()$largeur_donut_chart,
      height_svg = graph_sizes()$hauteur_donut_chart
    )
  })
  
  ################### Titre #############################################
output$titre_2 <- renderUI({
donuts_captions_titre[[global$data_key_value]]
  
  })
  ###################### Create Statistics ######################

  output$tx_en_emploi <- renderUI({
    tx_en_emploi_labels[[global$data_key_value]]
  })

  output$tx_chomage <- renderUI({
    tx_chomage_labels[[global$data_key_value]]
  })

  output$tx_en_edi <- renderUI({
    taux_edi_labels[[global$data_key_value]]
  })

  output$tx_a_tps_partiel <- renderUI({
    part_tps_partiel_labels[[global$data_key_value]]
  })

  output$revenu_median <- renderUI({
    revenu_travail_labels[[global$data_key_value]]
  })

  output$tx_jugent_coherent <- renderUI({
    correspondance_ok_labels[[global$data_key_value]]
  })

  output$tx_estiment_ss_employes <- renderUI({
    competence_ok_labels[[global$data_key_value]]
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
