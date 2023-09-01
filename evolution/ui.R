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
  fluidRow(
    div(
      class = "d-flex",
      column(
        width = 6,
        align = "left",
        class = "align-items-start",
        pickerInput(
          inputId = "indicateurs",
          label = h1("Choisir le plus haut diplôme atteint"),
          choices = vec_indicateurs,
          selected = vec_indicateurs[1],
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
        tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
        DownloadButton("downloadData", ".xlsx"),
        actionButton("downloadPDF", ".pdf", onclick = "window.print();")
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 6,
      h1("Situation trois ans après la sortie de formation initiale")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      uiOutput("legende_part_1")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 4,
      uiOutput("tx_emploi"),
      girafeOutput("plot_tx_emploi")
    ),
    column(
      width = 4,
      uiOutput("part_chomage"),
      girafeOutput("plot_part_chomage")
    ),
    column(
      width = 4,
      uiOutput("tx_chomage"),
      girafeOutput("plot_tx_chomage")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 6,
      h1("Quelles sont les conditions d’emploi des jeunes en emploi trois ans après leur sortie ?")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      uiOutput("legende_part_2")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 4,
      uiOutput("tx_edi"),
      girafeOutput("plot_tx_edi")
    ),
    column(
      width = 4,
      uiOutput("part_tps_partiel"),
      girafeOutput("plot_part_tps_partiel")
    ),
    column(
      width = 4,
      uiOutput("revenu_travail"),
      girafeOutput("plot_revenu_travail")
    )
  ),
  fluidRow(
    column(
      width = 4,
      uiOutput("comptence_ok"),
      girafeOutput("plot_comptence_ok")
    )
  ),
  br()
)
