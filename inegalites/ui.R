library(shiny)
library(bslib)
library(shinyWidgets)

fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
  app_head,
  gfontHtmlDependency(family = "Arimo"),
  header_div,
  fluidRow(
    column(
      width = 12,
      fluidRow(
        pickerInput(
          width = "fit",
          inline = TRUE,
          label = tags$h1("Choisir un facteur d’inégalités :"),
          inputId = "facteur",
          choices = facteur_choices,
          selected = "Sexe"
        ),
        pickerInput(
          width = "fit",
          inline = TRUE,
          label = tags$h1("Choisir un indicateur :"),
          inputId = "indicateur",
          choices = indicateur_choices,
          selected = "Taux d’emploi à trois ans"
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      class = "td",
      girafeOutput("graph", height = NULL)
    )
  )
)
