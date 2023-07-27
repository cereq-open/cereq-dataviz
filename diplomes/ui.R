library(shiny)
library(bslib)
library(shinyWidgets)

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
     column(
       width = 6,
       pickerInput(
         width = "fit",
         inline = TRUE,
         label = p("Choisir le plus haut diplôme atteint : "),
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
       ),
       conditionalPanel(
         condition = "output.sousniveau== true",
         selectInput("degre3", label = NULL, choices = NULL, selectize = FALSE, size = 3)
       )),
     column(
     align = "right",
       width = 6,
     div(
       style = "text-align:right;",
       tags$img(
         src = "logo-cereq.svg"
       ),
       tags$p(
         style = "font-size:14px;",
         "Données : ",
         tags$img(
           src = "logo-generation.png"
         )
       )
     ),
     tags$img(
       src = "logo-download.svg",
       height = "50px",
       width = "50px"
     ),
     tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
     DownloadButton('downloadData',".xlsx"),
     actionButton("downloadPDF", ".pdf", onclick = "window.print();")
     )
    ),
  br(),
  fluidRow(
    column(
      width = 12,
      tags$h1("Situation trois ans après la sortie de formation")
    ),
    column(
      width = 4,
      uiOutput("tx_en_emploi")
    ),
    column(
      width = 4,
      uiOutput("tx_chomage")
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        h2("Répartition des sortants selon leur situation d'activité"),
        style = "max-width:750px; margin-left:0;",
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
          h2("Répartition par profession"),
          style = "max-width:800px; margin-left:0;",
          girafeOutput("plot_repartition_par_profession", height = NULL)
        )
    ),
    column(
      width = 6,
      div(
        class = "custom-border-box",
        h2("Répartition par secteur"),
        style = "max-width:800px; margin-left:0;",
          girafeOutput("plot_repartition_par_secteur", height = NULL)
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