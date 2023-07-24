library(shiny)
library(bslib)
library(shinyWidgets)

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
    tags$head(
      tags$style(type = "text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row; }")
    ),
    column(
      width = 12,
      tags$table(
        width = "100%",
        tags$tr(
          width = "100%",
          tags$td(
            width = "40%",
            div(style = "font-size:24px; text-align: center;", "Choisir le plus haut diplôme atteint :")
          ),
          tags$td(
            width = "60%",
            pickerInput(
              width = "fit",
              inputId = "niveau",
              choices = list_degre1_2,
              choicesOpt = list(
                style = c(
                  "font-weight: bold;",
                  "font-weight: bold;",
                  "font-weight: bold;",
                  "",
                  "",
                  "",
                  "",
                  "font-weight: bold;",
                  "",
                  "",
                  "",
                  "",
                  "font-weight: bold;",
                  "",
                  "",
                  ""
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "output.sousniveau == true",
          tags$tr(
            width = "100%",
            tags$td(width = "60%", tags$div(style = "font-size:24px;", "")),
            tags$td(width = "40%",
                    selectInput("degre3", label = NULL, choices = NULL, selectize = FALSE, size = 2)
                    
                    )
          )
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      tags$h1("Situation trois ans après la sortie de formation")
    ),
    column(
      width = 6,
      uiOutput("tx_en_emploi")
    ),
    column(
      width = 6,
      uiOutput("tx_chomage")
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        style = "max-width:800px; margin-left:auto; margin-right:auto;",
        girafeOutput("graph_situation_apres_3_ans", height = NULL)
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      tags$h1("Quelles sont les conditions d’emploi des jeunes en emploi trois ans après leur sortie ?")
    ),
    column(
      width = 4,
      uiOutput("tx_en_edi")
    ),
    column(
      width = 4,
      uiOutput("tx_a_tps_partiel")
    ),
    column(
      width = 4,
      uiOutput("revenu_median")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$span(
            style = "color: #008B99;",
            "Répartition par profession"
          ),
          tags$i(
            class = "fas fa-info-circle",
            title = "Les professions sont présentées de façon agrégée, à partir d’une nomenclature plus détaillée, celle des professions et catégories socioprofessionnelles de l’INSEE de 2022."
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_repartition_par_profession", height = NULL)
        )
      )
    ),
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$span(
            style = "color: #008B99;",
            "Répartition par secteur"
          ),
          tags$i(
            class = "fas fa-info-circle",
            title = "Les secteurs sont présentés de façon agrégée, à partir d’une nomenclature plus détaillée : la nomenclature des activités françaises de l’INSEE de 2022 (NAF)."
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_repartition_par_secteur", height = NULL)
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      uiOutput("tx_jugent_coherent"),
      uiOutput("tx_estiment_ss_employes"),
      br()
    )
  )
)
