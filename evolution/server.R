shinyServer(function(input, output, session) {
  
  add_interactive_caption <- eventReactive(input$dimension, {
    dims <- input$dimension
    if (dims[1] < 577)
      FALSE
    else TRUE
  })
  
  graph_sizes <- eventReactive(input$dimension, {
    dims <- input$dimension

    if (dims[1] > 1200) {
      list(
        largeur_bar_chart = 7,
        hauteur_bar_chart = 5.25
      )
    } else if (dims[1] > 992) {
      list(
        largeur_bar_chart = 6,
        hauteur_bar_chart = 4.5
      )
    } else if (dims[1] > 576) {
      list(
        largeur_bar_chart = 7,
        hauteur_bar_chart = 4.125
      )
    } else {
      list(
        largeur_bar_chart = 4.5,
        hauteur_bar_chart = 3.75
      )
    }
  })

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
      filter(Libelle_Menu %in% input$type_diplome)
  })

  output$plot_tx_emploi <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "taux_emploi",
      .caption = caption_part_1,
      .title = columns_titles[["taux_emploi"]]$.title,
      .tooltip = columns_titles[["taux_emploi"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_part_chomage <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "part_chomage",
      .caption = caption_part_1,
      .title = columns_titles[["part_chomage"]]$.title,
      .tooltip = columns_titles[["part_chomage"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_tx_chomage <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "taux_chomage",
      .caption = caption_part_1,
      .title = columns_titles[["taux_chomage"]]$.title,
      .tooltip = columns_titles[["taux_chomage"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_tx_edi <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "taux_edi",
      .caption = caption_part_2,
      .title = columns_titles[["taux_edi"]]$.title,
      .tooltip = columns_titles[["taux_edi"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_part_tps_partiel <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "part_tps_partiel",
      .caption = caption_part_2,
      .title = columns_titles[["part_tps_partiel"]]$.title,
      .tooltip = columns_titles[["part_tps_partiel"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_revenu_travail <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "revenu_travail",
      .caption = caption_part_2,
      .title = columns_titles[["revenu_travail"]]$.title,
      .tooltip = columns_titles[["revenu_travail"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })

  output$plot_comptence_ok <- renderGirafe({
    gg <- evolution_barchart(
      .data = data_by_type_diplome[[input$type_diplome]],
      colname = "competence_ok",
      .caption = caption_part_2,
      .title = columns_titles[["competence_ok"]]$.title,
      .tooltip = columns_titles[["competence_ok"]]$.tooltip,
      interactive_caption = add_interactive_caption()
    )

    girafe(
      ggobj = gg,
      height_svg = graph_sizes()$hauteur_bar_chart,
      width_svg = graph_sizes()$largeur_bar_chart
    )
  })
})
