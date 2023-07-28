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
      paste("tab_region", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(db_region, file)
    }
  )

  # Create Title ---------------------------------------------------------------

  # Message contenu dans les bulles informatives

  info_bulle_residence_reactive <- reactive({
    if (input$colonne_residence == "tx_chmg") {
      return("Le taux de chômage correspond à la part des  individus sans emploi et à la recherche d'un emploi parmi les actifs (individus en emploi ou au chômage).")
    } else if (input$colonne_residence == "traj_1" | input$colonne_residence == "traj_2" | input$colonne_residence == "traj_3" | input$colonne_residence == "traj_7") {
      return("Cette information résulte d'une typologie des trajectoires construite par le Céreq par des méthodes statistiques.")
    } else if (input$colonne_residence == "taux_ed") {
      return("Proportion d'individus en emploi non-salarié (personne à son compte ou aide familial), en contrat à durée indéterminée (CDI) ou avec le statut de fonctionnaire.")
    } else if (input$colonne_residence == "rvn_trv") {
      return("Niveau de salaire ou traitement mensuel net primes incluses médian. Le revenu médian est la valeur telle que la moitié des individus de la population considérée gagne plus, l'autre moitié gagne moins.")
    }
  })

  # Titre

  output$region_de_residence <- renderUI({
    if (input$colonne_residence %in% noms_colonnes_residence_info_bulle) {
      labellize_row_i(noms_colonnes_residence[input$colonne_residence], info_bulle_residence_reactive())
    } else {
      labellize_row_i(noms_colonnes_residence[input$colonne_residence])
    }
  })

  output$stat_residence <- renderUI({
    col <- inv_noms_colonnes_db_region[input$colonne_residence]
    ligne_fr <- 1
    ligne_drom <- 3
    
    stat_france <- paste0("France : ", db_region[ligne_fr, col], "%")
    stat_drom <- paste0("(", paste0("Dont DROM: ", db_region[ligne_drom, col], "%"), ")")

    labellize_stat(stat_france, stat_drom)
  })

  output$stat_niveau <- renderUI({
    col <- inv_noms_colonnes_db_region[input$colonne_niveau]
    ligne_fr <- 1
    ligne_drom <- 3
    
    
    
    stat_france <- paste0("France : ", db_region[ligne_fr, col], "%")
    stat_drom <- paste0("(", paste0("Dont DROM: ", db_region[ligne_drom, col], "%"), ")")

    labellize_stat(stat_france, stat_drom)
    
  })

  # Create Map -----------------------------------------------------------------

  # Code pour créer la carte avec ggplot
  output$carte <- renderGirafe({
    
    caption <- paste0('<span style="color:#008B99;">Note : </span>',
                      "Une partie des écarts observés entre région s’explique par les différences de niveaux de formation atteint par les sortants de chaque région. La carte à droite en donne une illustration",
                      "<br>",
                      '<span style="color:#008B99;">Champs : </span>',
                      "Ensemble de la Génération 2017",
                      "<br>",
                      '<span style="color:#008B99;">Source : </span>',
                      "Céreq, enquête Génération 2017 à 3 ans."
                      )

    tab_region <- concatenate_columns(tab_region, input$colonne_residence)
    gg <- plot_map(tab_region, input$colonne_residence, "label", caption)
    girafe(
      ggobj = gg,
      width_svg = longeur_map,
      height_svg = longeur_map
    )
  })

  # Code pour créer la carte avec ggplot
  output$carte2 <- renderGirafe({
    caption <- paste0(
      '<span style="color:#008B99;">Champs : </span>',
      "Ensemble de la Génération 2017",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à 3 ans."
    )

    tab_region <- concatenate_columns(tab_region, input$colonne_niveau)
    gg <- plot_map(tab_region, input$colonne_niveau, "label", caption)
    girafe(
      ggobj = gg,
      width_svg = longeur_map,
      height_svg = longeur_map
    )
  })
})
