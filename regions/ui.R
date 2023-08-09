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
  gfontHtmlDependency(family = "Arimo"),
  fluidRow(
    div(
      class = "d-flex",
      column(
        width = 6,
        align = "left",
        class = "align-items-start",
        pickerInput(
          inputId = "titre_residence",
          label = p("Choisir l'indicateur :",
                    class = "d-inline"),
          choices = titre_map_residence,
          selected = titre_map_residence[1],
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
        class = "align-items-end",
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
        width = 7,
        align = "left",
        uiOutput("region_de_residence"),
        uiOutput("stat_residence"),
        div(
          style = "max-width:800px; margin-left:0;",
          girafeOutput("carte_residence", height = NULL)
        )
      ),
      column(
        width = 5,
        p("Niveau de formation des sortants",
          class = "d-inline",
          style = "font-size:18px;"
        ),
        pickerInput(
          inputId = "titre_niveau",
          label = p("Choisir le niveau de plus haut diplôme :",
                    class = "d-inline",
                    style = "font-size:18px;"),
          choices = titre_map_niveau,
          selected = titre_map_niveau[1],
          width = "fit",
          inline = TRUE
        ),
        uiOutput("stat_niveau"),
        div(
          style = "max-width:800px; margin-right:0;",
          girafeOutput("carte_niveau", height = NULL)
        )
      )
    ),
    br()
  )
)
