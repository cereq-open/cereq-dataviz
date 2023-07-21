# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)

# Define UI --------------------------------------------------------------------
fluidPage(
  theme = bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # CSS personnalisé
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"), # Librairie font-awesome
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"), # Web Font Loader
    tags$script(
      "
  WebFont.load({
    google: {
      families: ['Arimo']
    }
  });
  "
    ) # Pour que Arimo soit toujours disponible dans le navigateur de l'utilisateur
  ),
  br(),
  gfontHtmlDependency(family = "Arimo"),
  tags$span(
    tags$div(class = "card", "Génération 2017"),
    tags$div(
      class = "logo-container", # Classe CSS pour le conteneur du logo
      includeHTML("www/logo.svg")
    ) # Inclus le logo
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      downloadButton("downloadData", "Télécharger les données")
    )
  ),
  br(),
  fluidRow(
    column(width = 12,
           pickerInput(inputId = "col_name",
                       label = p("Choisir l'indicateur :"),
                       choices = noms_colonnes_inverse,
                       selected = "tax_mpl",
                       width = "fit",
                       inline = TRUE,
                       options = list(
                         style = "btn-select-input",
                         size = 5))
    )
  ),
  fluidRow(
    column(
      width = 12,
      uiOutput("titre1")
    ),
    column(
      width = 12,
      plotOutput("carte")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$h1("Poids des sortants du supérieur parmi les sortants de la région")
    ),
    column(
      width = 12,
      plotOutput("carte2")
    )
  )
)
