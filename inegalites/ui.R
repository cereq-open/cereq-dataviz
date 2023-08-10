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
       fluidRow(
         pickerInput(
           width = "fit",
           inline = TRUE,
           label = tags$h1("Choisir un facteur d’inégalités :"),
           inputId = "facteur",
           choices = valeurs_facteur_analyse,
           selected = "sexe"
       ),
       pickerInput(
         width = "fit",
         inline = TRUE,
         label = tags$h1("Choisir un indicateur :"),
         inputId = "indicateur",
         choices = valeurs_indicateurs,
         selected = "Taux d’emploi à trois ans"
       )
       )
       ),
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
  fluidRow(
    column(
      width = 12,
      div(
        uiOutput("titre_graph"),
        style = "max-width:1000px; margin-left:0;",
        girafeOutput("graph", height = NULL)
      )
    )
  )
)