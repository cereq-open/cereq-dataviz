shinyServer(function(input, output, session) {
  graph_sizes <- eventReactive(input$dimension, {
    dims <- input$dimension

    if (dims[1] > 1200) {
      list(
        largeur_map = 8,
        hauteur_map = 9
      )
    } else if (dims[1] > 992) {
      list(
        largeur_map = 9,
        hauteur_map = 10
      )
    } else if (dims[1] > 576) {
      list(
        largeur_map = 7,
        hauteur_map = 8
      )
    } else {
      list(
        largeur_bar_chart = 5,
        hauteur_bar_chart = 7
      )
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OpenData_Cereq-Enq_Generation-Donnees_REGION", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_REGION.xlsx", file)
    }
  )

  # Titres et labels ----

  output$stat_indicateur <- renderUI({
    liste_label_indicateurs[[input$valeur_indicateur]]
  })

  output$stat_niveau <- renderUI({
    liste_label_niveaux_diplomes[[input$valeur_diplome_niveau]]
  })

  # cartes -----
  output$carte_indicateur <- renderGirafe({
    caption <- generateCaption(input$valeur_indicateur)

    augmented_db_stats_par_regions <- concatenate_columns(db_stats_par_regions, input$valeur_indicateur)
    gg <- region_map(
      .data = augmented_db_stats_par_regions,
      column_stat_name = input$valeur_indicateur,
      column_label_name = "label",
      .caption = caption,
      .title = liste_titre_indicateurs[[input$valeur_indicateur]]$.title,
      .tooltip = liste_titre_indicateurs[[input$valeur_indicateur]]$.tooltip
    )
    girafe(
      ggobj = gg,
      fonts = list(sans = "Arimo"),
      width_svg = graph_sizes()$largeur_map,
      height_svg = graph_sizes()$hauteur_map
    )
  })

  output$carte_niveau_diplome <- renderGirafe({
    caption <- generateCaption(input$valeur_indicateur)
    augmented_db_stats_par_regions <- concatenate_columns(db_stats_par_regions, input$valeur_diplome_niveau)

    gg <- region_map(
      .data = augmented_db_stats_par_regions,
      column_stat_name = input$valeur_diplome_niveau,
      column_label_name = "label",
      .caption = caption,
      .title = "Proportion de sortants de formation initiale ayant ce niveau",
      .tooltip = NULL
    )
    girafe(
      ggobj = gg,
      fonts = list(sans = "Arimo"),
      width_svg = graph_sizes()$largeur_map,
      height_svg = graph_sizes()$hauteur_map
    )
  })
})
