# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  # Download Data --------------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OpenData_Cereq-Enq_Generation-Donnees_REGION", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/OpenData_Cereq-Enq_Generation-Donnees_REGION.xlsx", file)
    }
  )

  # Filtered Data --------------------------------------------------------------

  variables_residence_reactive <- reactive({
    variables_residence %>%
      filter(Titre_graphique %in% input$titre_residence)
  })

  variables_niveau_reactive <- reactive({
    variables_niveau %>%
      filter(Titre_graphique %in% input$titre_niveau)
  })

  # Create Title ---------------------------------------------------------------

  # Titres

  output$region_de_residence <- renderUI({
    if (!is.na(variables_residence_reactive()$Bulle)) {
      labellize_row_i(variables_residence_reactive()$Titre_graphique, variables_residence_reactive()$Bulle)
    } else {
      labellize_row_i(variables_residence_reactive()$Titre_graphique)
    }
  })

  output$stat_residence <- renderUI({
    if (variables_residence_reactive()$Nom_colonne != "revenu_travail") {
      symbole <- "%"
    } else {
      symbole <- " €"
    }

    stat_france <- paste0("France : ", db_region[ligne_fr, variables_residence_reactive()$Nom_colonne], symbole)
    stat_drom <- paste0("(", paste0("dont ensemble des D.R.O.M. : ", db_region[ligne_drom, variables_residence_reactive()$Nom_colonne], symbole), ")")

    labellize_stat(stat_france, stat_drom)
  })

  output$stat_niveau <- renderUI({
    stat_france <- paste0("France : ", db_region[ligne_fr, variables_niveau_reactive()$Nom_colonne], "%")
    stat_drom <- paste0("(", paste0("dont ensemble des D.R.O.M. : ", db_region[ligne_drom, variables_niveau_reactive()$Nom_colonne], "%"), ")")

    labellize_stat(stat_france, stat_drom)
  })

  # Create Map -----------------------------------------------------------------

  # Code pour créer la carte avec ggplot
  output$carte_residence <- renderGirafe({
    nom_col <- inv_noms_colonnes_residence[[variables_residence_reactive()$Nom_colonne]]

    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans.",
      "<br>",
      '<span style="color:#008B99;">Note : </span>',
      "Une partie des écarts observés entre région s’explique par les différences de niveaux de formation atteint par les sortants de chaque région. La carte à droite en donne une illustration."
    )

    tab_region <- concatenate_columns(tab_region, nom_col)
    gg <- plot_map(tab_region, nom_col, "label", caption)
    girafe(
      ggobj = gg,
      fonts = list(sans = "Arimo"),
      width_svg = longeur_map,
      height_svg = longeur_map
    )
  })

  # Code pour créer la carte avec ggplot
  output$carte_niveau <- renderGirafe({
    nom_col <- inv_noms_colonnes_niveau[[variables_niveau_reactive()$Nom_colonne]]

    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )

    tab_region <- concatenate_columns(tab_region, nom_col)
    gg <- plot_map(tab_region, nom_col, "label", caption)
    girafe(
      ggobj = gg,
      fonts = list(sans = "Arimo"),
      width_svg = longeur_map,
      height_svg = longeur_map
    )
  })
})