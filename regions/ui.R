# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)

# Define UI --------------------------------------------------------------------
fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
  app_head,
  gfontHtmlDependency(family = "Arimo"),
  header_div,
  fluidRow(
    div(
      class = "d-flex",
      column(
        width = 12,
        align = "left",
        class = "p-2 col-sm-12 col-xl-6",
        pickerInput(
          inputId = "valeur_indicateur",
          label = h1("Choisir l'indicateur :"),
          choices = valeurs_indicateurs,
          selected = valeurs_indicateurs[1],
          width = "fit",
          inline = TRUE
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        class = "p-2 col-sm-12 col-xl-6",
        tags$table(
          class = "stat-table",
          tags$tr(
            tags$td(uiOutput("stat_indicateur"))
          )
        ),
        girafeOutput("carte_indicateur", height = NULL)
      ),
      column(
        width = 12,
        class = "p-2 col-sm-12 col-xl-6",
        p("Une partie des écarts observés entre régions pour l’indicateur choisi s’explique par les différences de niveaux de formation atteint par les sortants de chaque région. La carte ci-dessous permet d’en donner une illustration."),
        pickerInput(
          inputId = "valeur_diplome_niveau",
          label = h1("Choisir le niveau du plus haut diplôme :"),
          choices = valeurs_niveaux_diplomes,
          selected = valeurs_niveaux_diplomes[1],
          width = "fit",
          inline = TRUE
        ),
        tags$table(
          class = "stat-table",
          tags$tr(
            tags$td(uiOutput("stat_niveau"))
          )
        ),
        girafeOutput("carte_niveau_diplome", height = NULL)
      )
    )
  )
)
