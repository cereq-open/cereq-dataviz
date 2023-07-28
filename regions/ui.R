# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)

# Define UI --------------------------------------------------------------------
fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
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
  br(),
  fluidRow(
    column(
      width = 6,
      align = "left",
      pickerInput(
        inputId = "colonne_residence",
        label = p("Choix de l'indicateur"),
        choices = noms_colonnes_residence_inverse,
        selected = "tax_mpl",
        width = "fit",
        inline = TRUE,
        options = list(
          size = 5
        )
      )
    ),
    column(
      width = 6,
      align = "right",
      tags$img(
        src = "logo-cereq.svg"
      ),
      tags$p(
        style = "font-size:14px;",
        "Données : ",
        tags$img(
          src = "logo-generation.png"
        )
      ),
      tags$img(
        src = "logo-download.svg",
        height = "50px",
        width = "50px"
      ),
      tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
      downloadButton("downloadData", ".xlsx"),
      actionButton("downloadPDF", ".pdf", onclick = "window.print();")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 8,
      uiOutput("region_de_residence"),
      uiOutput("stat_residence"),
      girafeOutput("carte")
    ),
    column(
      width = 4,
      p("Niveau de formation des sortants", class = "d-inline"),
      pickerInput(
        inputId = "colonne_niveau",
        label = p("Plus haut diplôme"),
        choices = noms_colonnes_niveau_inverse,
        selected = "tax_mpl",
        width = "fit",
        inline = TRUE
      ),
      uiOutput("stat_niveau"),
      girafeOutput("carte2")
    )
  )
)
