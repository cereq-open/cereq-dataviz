
library(gfonts)
library(htmltools)
library(shinydashboardPlus)
library(shinyWidgets)
library(htmlwidgets)



url_link<-"https://www.linkedin.com/shareArticle?mini=true&url=https://cereq-data-visualisation.shinyapps.io/Rexemple/&title=dataviz" 

ui <- fluidPage(
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
    column(width=6),
    column(
      align = "right",
      width = 6,
      div(
        style = "text-align:right;",
        tags$img(src = "logo-cereq.svg")#,
        #tags$p(
        #style = "font-size:14px;",
        # "Données : ",
        # tags$img(src = "logo-generation.png")
        # )
      ))),
  fluidRow(column( width = 12, class='h1')),
  fluidRow(  h1("TAUX D'ACCES"),
    column(
      align ="right", 
      width = 12,
      # tags$img(src = "logo-download.svg", height = "50px", width = "50px"),
      tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
      DownloadButton("downloadData", ".xlsx"),
      actionButton("downloadPDF", ".pdf", onclick = "window.print();"),
      actionButton("linkedin_share",
                   label = "Linkedin",
                   icon = icon("linkedin"),
                   onclick = sprintf("window.open('%s')", url_link))
    )
  ),
  
  
  
  br(),
  gfontHtmlDependency(family = "Arimo"),
  

  
  fluidRow(
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Taux d'acces par CS"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Les secteurs"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_cs", height = NULL)
        )
      )
    ), 
    
    
    #PART FROMATRICE
    br(),
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Taux d'acces par sexe"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Les secteurs"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_sexe", height = NULL)
        )
      )
    )
  ),
  
  
  #TPF
  br(),
  fluidRow(
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Taux d'acces par age "
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Les secteurs"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_age", height = NULL)
        )
      )
      
    ),
    #HEURES DE STAGES
    br(),
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Taux d'acces par age et sexe"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Les secteurs"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_sexe_CS", height = NULL)
        )
      )
      
    ))
  
)



