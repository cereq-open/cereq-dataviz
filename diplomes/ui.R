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
      families: ['Arial']
    }
  });
  "
    ) # Pour que Arial soit toujours disponible dans le navigateur de l'utilisateur
  ),
  br(),
  tags$div(
    class = "logo-container", # Classe CSS pour le conteneur du logo
    includeHTML("www/logo.svg") # Inclus le logo
  ),
  # tags$div(
  #   class = "social-icons",
  #   tags$a(
  #     href = "url_linkedin",
  #     target = "_blank",
  #     tags$i(class = "fab fa-linkedin fa-lg")
  #   ),
  #   tags$a(
  #     href = "url_facebook",
  #     target = "_blank",
  #     tags$i(class = "fab fa-facebook fa-lg")
  #   ),
  #   tags$a(
  #     href = "url_twitter",
  #     target = "_blank",
  #     tags$i(class = "fab fa-twitter fa-lg")
  #   )
  # ),
  div(
    class = "row align-items-end",
    column(
      width = 2,
      pickerInput(
        inputId = "niveau",
        label = "Choisir le plus haut diplôme atteint",
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
    ),
    column(
      width = 2,
      selectInput("degre3", label = "Texte", choices = NULL, selectize = FALSE, size = 2),
      div(class = "form-group", actionButton("clear", "Déselectionner"))
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      tags$h1("Trois ans après la sortie de formation")
    ),
    column(
      width = 6,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          "En emploi"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Proportion de jeunes qui sont en emploi trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        ),
        tags$span(
          style = "color: #008B99;",
          textOutput("tx_en_emploi")
        )
      )
    ),
    column(
      width = 6,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          "Taux de chômage"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Proportion de jeunes qui sont au chômage trois ans après leur sortie de formation initiale parmi l'ensemble des sortants."
        ),
        tags$span(
          style = "color: #008B99;",
          textOutput("tx_chomage")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          "Répartition des sortants selon leur situation d'activité"
        )
      ),
      plotOutput("graph_situation_apres_3_ans")
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
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          textOutput("tx_en_edi"),
          "En emploi à durée indéterminée"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Fonctionnaires et salariés en contrats à durée indéterminée."
        )
      )
    ),
    column(
      width = 4,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          textOutput("tx_a_tps_partiel"),
          "à temps partiel",
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Texte informatif affiché au survol"
        )
      )
    ),
    column(
      width = 4,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          textOutput("revenu_median"),
          "Revenu mensuel médian",
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Niveau de revenu mensuel médian des jeunes qui sont en emploi trois ans après leur sortie de formation initiale. Le niveau médian est tel que 50% gagnent plus et 50% gagnent moins."
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 6,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          "Répartition par profession"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Les professions sont présentées de façon agrégée, à partir d’une nomenclature plus détaillée, celle des professions et catégories socioprofessionnelles de l’INSEE de 2022."
        )
      )
    ),
    column(
      width = 6,
      tags$h3(
        tags$span(
          style = "color: #008B99;",
          "Répartition par secteur"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "margin-left: 5px;",
          title = "Les secteurs sont présentés de façon agrégée, à partir d’une nomenclature plus détaillée : la nomenclature des activités françaises de l’INSEE de 2022 (NAF)."
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("plot_repartition_par_profession")
    ),
    column(
      width = 6,
      plotOutput("plot_repartition_par_secteur")
    )
  ),
  fluidRow(
    column(
      width = 12,
      uiOutput("tx_jugent_coherent"),
      uiOutput("tx_estiment_ss_employes")
    )
  )
)
